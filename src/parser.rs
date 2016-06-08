use std::cmp;
use std::path::{Path, PathBuf};

use syntex_syntax::parse;
use syntex_syntax::visit;
use syntex_syntax::ast;
use syntex_syntax::codemap::Span;
use syntex_syntax::parse::token::keywords;

use errors::Result;

#[derive(PartialEq, Eq, PartialOrd, Debug)]
#[repr(u8)]
pub enum Token {
    /// non-symbol text
    Ident = b' ',
    /// function definition
    FuncDef = b'$',
    /// function call
    FuncCall = b'`',
    /// function end
    FuncEnd = b'}',
    /// #define
    Define = b'#',
    /// #define end
    DefineEnd = b')',
    /// #include
    Include = b'~',
    /// direct assignment, increment, or decrement
    Assignment = b'=',
    /// enum/struct/union definition end
    DeclEnd = b';',
    /// class definition
    ClassDef = b'c',
    /// enum definition
    EnumDef = b'e',
    /// other global definition
    GlobalDef = b'g',
    /// function/block local definition
    LocalDef = b'l',
    /// global enum/struct/union member definition
    MemberDecl = b'm',
    /// function parameter definition
    FuncParam = b'p',
    /// struct definition
    StructDef = b's',
    /// typedef definition
    Typedef = b't',
    /// union definition
    UnionDef = b'u',
}

#[derive(PartialEq, Eq, Debug)]
pub struct Symbol {
    pub token: Token,
    pub name: String,
    pub span: Span,
}

impl Symbol {
    fn import_crate(item: &ast::Item) -> Symbol {
        Symbol {
            token: Token::Include,
            name: item.ident.name.as_str().to_string(),
            span: item.span,
        }
    }

    fn use_module(path: &ast::Path, items: Option<&Vec<ast::PathListItem>>) -> Vec<Symbol> {
        items.map_or_else(|| {
            vec![Symbol {
                     token: Token::Include,
                     name: path.to_string(),
                     span: path.span,
                 }]
        },
                          |items| {
            items.iter()
                .map(|item| {
                    Symbol {
                        token: Token::Include,
                        name: if let Some(ident) = item.node.name() {
                            format!("{}::{}", path, ident.name.as_str())
                        } else {
                            path.to_string()
                        },
                        span: item.span,
                    }
                })
                .collect()
        })
    }

    fn define_global(item: &ast::Item, _: &ast::Ty) -> Symbol {
        Symbol {
            token: Token::GlobalDef,
            name: item.ident.name.as_str().to_string(),
            span: item.span,
        }
    }

    fn declare_func(ident: ast::Ident, span: Span, func_decl: &ast::FnDecl) -> Vec<Symbol> {
        let mut symbols = vec![Symbol {
                                   token: Token::FuncDef,
                                   name: ident.name.to_string(),
                                   span: span,
                               }];

        symbols.extend(func_decl.inputs.iter().filter_map(|arg| {
            match arg.pat.node {
                ast::PatKind::Ident(_, ident, _) if ident.node.name !=
                                                    keywords::SelfValue.name() => {
                    Some(Symbol {
                        token: Token::FuncParam,
                        name: ident.node.name.to_string(),
                        span: arg.pat.span,
                    })
                }
                _ => None,
            }
        }));

        symbols
    }

    fn define_trait(item: &ast::Item, trait_definition: &Vec<ast::TraitItem>) -> Vec<Symbol> {
        let mut symbols = vec![Symbol {
                                   token: Token::ClassDef,
                                   name: item.ident.name.as_str().to_string(),
                                   span: item.span,
                               }];

        symbols.extend(trait_definition.iter().flat_map(|ref item| {
            match item.node {
                ast::TraitItemKind::Const(ref typ, _) => {
                    vec![Symbol {
                             token: Token::MemberDecl,
                             name: item.ident.name.to_string(),
                             span: item.span,
                         }]
                }
                ast::TraitItemKind::Method(ref method, _) => {
                    Self::declare_func(item.ident, item.span, &method.decl)
                }
                ast::TraitItemKind::Type(_, _) => {
                    vec![Symbol {
                             token: Token::Typedef,
                             name: item.ident.name.to_string(),
                             span: item.span,
                         }]
                }
            }
        }));

        symbols
    }

    fn define_enum(item: &ast::Item, definition: &ast::EnumDef) -> Vec<Symbol> {
        let mut symbols = vec![Symbol {
                                   token: Token::EnumDef,
                                   name: item.ident.name.as_str().to_string(),
                                   span: item.span,
                               }];

        symbols.extend(definition.variants.iter().map(|var| {
            Symbol {
                token: Token::MemberDecl,
                name: var.node.name.to_string(),
                span: var.span,
            }
        }));

        symbols
    }

    fn define_struct(item: &ast::Item, definition: &ast::VariantData) -> Vec<Symbol> {
        let mut symbols = vec![Symbol {
                                   token: Token::StructDef,
                                   name: item.ident.name.as_str().to_string(),
                                   span: item.span,
                               }];

        if let &ast::VariantData::Struct(ref fields, _) = definition {
            symbols.extend(fields.iter().filter_map(|ref field| {
                field.ident.map(|ident| {
                    Symbol {
                        token: Token::MemberDecl,
                        name: ident.name.as_str().to_string(),
                        span: field.span,
                    }
                })
            }));
        }

        symbols
    }

    fn declare_typedef(item: &ast::Item, _: &ast::Ty) -> Symbol {
        Symbol {
            token: Token::Typedef,
            name: item.ident.name.to_string(),
            span: item.span,
        }
    }

    fn use_macro(mac: &ast::Mac) -> Symbol {
        Symbol {
            token: Token::Ident,
            name: mac.node.path.to_string(),
            span: mac.span,
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct SourceLine {
    pub line_num: usize,
    pub symbols: Vec<Symbol>,
}

impl cmp::PartialOrd for SourceLine {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        self.line_num.partial_cmp(&other.line_num)
    }
}

impl cmp::Ord for SourceLine {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.line_num.cmp(&other.line_num)
    }
}

#[derive(PartialEq, Eq)]
pub struct SourceFile {
    pub path: PathBuf,
    pub lines: Vec<SourceLine>,
}

impl cmp::PartialOrd for SourceFile {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        self.path.partial_cmp(&other.path)
    }
}

impl cmp::Ord for SourceFile {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.path.cmp(&other.path)
    }
}

pub struct SourceFileVisitor {
    session: parse::ParseSess,
    symbols: Vec<Symbol>,
}

impl SourceFileVisitor {
    fn code_span(&self, span: Span) -> String {
        self.session.codemap().span_to_string(span)
    }
}

impl<'a> visit::Visitor<'a> for SourceFileVisitor {
    fn visit_item(&mut self, item: &'a ast::Item) {
        let name = item.ident.name.as_str();

        match item.node {
            ast::ItemKind::ExternCrate(opt_name) => {
                trace!("import crate `{}`{} @ {}",
                       name,
                       if let Some(ref as_name) = opt_name {
                           format!(" as {}", as_name)
                       } else {
                           "".to_string()
                       },
                       self.code_span(item.span));

                let symbol = Symbol::import_crate(item);

                self.symbols.push(symbol);
            }

            ast::ItemKind::Use(ref vp) => {
                let mut symbols = match vp.node {
                    ast::ViewPathSimple(ident, ref path) => {
                        trace!("use {}{} @ {}",
                               path,
                               match path.segments.last() {
                                   Some(seg) if seg.identifier == ident => "".to_string(),
                                   _ => format!(" as {}", ident),
                               },
                               self.code_span(item.span));

                        Symbol::use_module(path, None)
                    }
                    ast::ViewPathGlob(ref path) => {
                        trace!("use {}::* @ {}", path, self.code_span(item.span));

                        Symbol::use_module(path, None)
                    }
                    ast::ViewPathList(ref prefix, ref list) => {
                        trace!("use {}::{{{}}} @ {}",
                               prefix,
                               list.iter()
                                   .filter_map(|item| {
                                       item.node.name().map(|ident| ident.name.as_str().to_string())
                                   })
                                   .collect::<Vec<String>>()
                                   .join(", "),
                               self.code_span(item.span));

                        Symbol::use_module(prefix, Some(list))
                    }
                };

                self.symbols.append(&mut symbols);
            }

            ast::ItemKind::Static(ref typ, _, ref expr) |
            ast::ItemKind::Const(ref typ, ref expr) => {
                trace!("define static/const `{}` : {:?}  = {:?} @ {}",
                       name,
                       typ,
                       expr,
                       self.code_span(item.span));

                let symbol = Symbol::define_global(item, typ);

                self.symbols.push(symbol);
            }

            ast::ItemKind::Trait(_, _, _, ref trait_definition) => {
                debug!("define trait `{}` with {} items @ {}",
                       name,
                       trait_definition.len(),
                       self.code_span(item.span));

                let mut symbols = Symbol::define_trait(item, trait_definition);

                self.symbols.append(&mut symbols);
            }

            ast::ItemKind::Enum(ref enum_definition, _) => {
                trace!("define enum `{}` with {} values @ {}",
                       name,
                       enum_definition.variants.len(),
                       self.code_span(item.span));

                let mut symbols = Symbol::define_enum(item, enum_definition);

                self.symbols.append(&mut symbols);
            }

            ast::ItemKind::Struct(ref struct_definition, _) => {
                trace!("define struct `{}` with {} fields @ {}",
                       name,
                       struct_definition.fields().len(),
                       self.code_span(item.span));

                let mut symbols = Symbol::define_struct(item, struct_definition);

                self.symbols.append(&mut symbols);
            }

            ast::ItemKind::Ty(ref typ, _) => {
                trace!("declare typedef `{}` = {:?} @ {}",
                       name,
                       typ,
                       self.code_span(item.span));

                let symbol = Symbol::declare_typedef(item, typ);

                self.symbols.push(symbol);
            }

            _ => visit::walk_item(self, item),
        }
    }

    fn visit_mac(&mut self, mac: &ast::Mac) {
        trace!("use macro: {} @ {}",
               mac.node.path,
               self.code_span(mac.span));

        self.symbols.push(Symbol::use_macro(mac));
    }
}

pub fn extract_symbols(src_path: &Path) -> Result<Vec<SourceFile>> {
    let cfg = Vec::new();

    let mut visitor = SourceFileVisitor {
        session: parse::ParseSess::new(),
        symbols: Vec::new(),
    };

    let krate = try!(parse::parse_crate_from_file(&src_path, cfg, &visitor.session));

    debug!("walking crate @ {}", visitor.code_span(krate.span));

    visit::walk_crate(&mut visitor, &krate);

    let source_file = SourceFile {
        path: PathBuf::from(src_path),
        lines: Vec::new(),
    };

    debug!("found {} symbols in {}",
           visitor.symbols.len(),
           source_file.path.to_str().unwrap());

    Ok(vec![source_file])
}
