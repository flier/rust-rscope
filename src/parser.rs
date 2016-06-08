use std::cmp;
use std::iter::Extend;
use std::string::ToString;
use std::path::{Path, PathBuf};

use syntex_syntax::parse;
use syntex_syntax::visit;
use syntex_syntax::ast::{self, Ident, SpannedIdent};
use syntex_syntax::codemap::{Span, Spanned, spanned, respan};
use syntex_syntax::print::pprust;
use syntex_syntax::parse::token::keywords;
use syntex_syntax::ptr::P;

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

impl Token {
    fn with_ident(self, ident: &SpannedIdent) -> Symbol {
        Symbol {
            token: self,
            name: ident.node.name.as_str().to_string(),
            span: ident.span,
        }
    }

    fn with_name<T: ToString>(self, name: &Spanned<T>) -> Symbol {
        Symbol {
            token: self,
            name: name.node.to_string(),
            span: name.span,
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct Symbol {
    pub token: Token,
    pub name: String,
    pub span: Span,
}

impl Symbol {
    fn use_ident(ident: &SpannedIdent) -> Symbol {
        Token::Ident.with_ident(ident)
    }

    fn import_crate(ident: &SpannedIdent) -> Symbol {
        Token::Include.with_ident(ident)
    }

    fn use_module(path: &ast::Path, items: Option<&Vec<ast::PathListItem>>) -> Vec<Symbol> {
        items.map_or_else(|| vec![Token::Include.with_name(&respan(path.span, path.to_string()))],
                          |items| {
            items.iter()
                .map(|item| {
                    Token::Ident.with_name(&respan(item.span,
                                                   if let Some(ident) = item.node.name() {
                                                       format!("{}::{}", path, ident.name.as_str())
                                                   } else {
                                                       path.to_string()
                                                   }))
                })
                .collect()
        })
    }

    fn declare_func(name: Option<ast::Name>, span: Span, func_decl: &ast::FnDecl) -> Vec<Symbol> {
        let name = name.map_or_else(|| "".to_string(), |s| s.to_string());

        let mut symbols = vec![Token::FuncDef.with_name(&respan(span, &name)),
                               Token::FuncEnd.with_name(&spanned(span.hi, span.hi, &name))];

        symbols.extend(func_decl.inputs.iter().filter_map(|arg| {
            match arg.pat.node {
                ast::PatKind::Ident(_, ref ident, _) if ident.node.name !=
                                                        keywords::SelfValue.name() => {
                    Some(Token::FuncParam.with_ident(ident))
                }
                _ => None,
            }
        }));

        symbols
    }

    fn define_trait(ident: &SpannedIdent, trait_definition: &Vec<ast::TraitItem>) -> Vec<Symbol> {
        let mut symbols = vec![Token::ClassDef.with_ident(ident)];

        symbols.extend(trait_definition.iter()
            .filter_map(|ref item| {
                match item.node {
                    ast::TraitItemKind::Const(_, _) => {
                        Some(Token::MemberDecl.with_ident(&respan(item.span, item.ident)))
                    }
                    ast::TraitItemKind::Type(_, _) => {
                        Some(Token::Typedef.with_ident(&respan(item.span, item.ident)))
                    }
                    ast::TraitItemKind::Method(_, _) => None,
                }
            }));

        symbols
    }

    fn define_enum(ident: &SpannedIdent, definition: &ast::EnumDef) -> Vec<Symbol> {
        let mut symbols = vec![Token::EnumDef.with_ident(ident),
                 Token::DeclEnd.with_ident(&spanned(ident.span.hi, ident.span.hi, ident.node))];

        symbols.extend(definition.variants
            .iter()
            .map(|var| Token::MemberDecl.with_ident(&respan(var.span, var.node.name))));

        symbols
    }

    fn define_struct(ident: &SpannedIdent, fields: &Vec<SpannedIdent>) -> Vec<Symbol> {
        let mut symbols = vec![Token::StructDef.with_ident(ident),
                 Token::DeclEnd.with_ident(&spanned(ident.span.hi, ident.span.hi, ident.node))];

        symbols.extend(fields.iter().map(|ref field| Token::MemberDecl.with_ident(field)));

        symbols
    }

    fn declare_typedef(ident: &SpannedIdent, _: &ast::Ty) -> Symbol {
        Token::Typedef.with_ident(ident)
    }

    fn define_global(ident: &SpannedIdent, _: &P<ast::Ty>) -> Symbol {
        Token::GlobalDef.with_ident(ident)
    }

    fn define_local(name: &Spanned<&str>, _: &Option<P<ast::Ty>>) -> Symbol {
        Token::LocalDef.with_name(name)
    }

    fn define_macro(mac: &ast::MacroDef) -> Vec<Symbol> {
        vec![Token::Define.with_ident(&respan(mac.span, mac.ident)),
             Token::DefineEnd.with_ident(&spanned(mac.span.hi, mac.span.hi, mac.ident))]
    }

    fn use_macro(mac: &ast::Mac) -> Symbol {
        Token::Ident.with_name(&respan(mac.span, &mac.node.path))
    }

    fn call_func(name: &Spanned<&str>) -> Symbol {
        Token::FuncCall.with_name(name)
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

macro_rules! walk_list {
    ($visitor: expr, $method: ident, $list: expr) => {
        for elem in $list {
            $visitor.$method(elem)
        }
    };
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
    fn visit_ident(&mut self, span: Span, ident: Ident) {
        self.symbols.push(Symbol::use_ident(&respan(span, ident)));
    }

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

                let symbol = Symbol::import_crate(&respan(item.span, item.ident));

                self.symbols.push(symbol);
            }

            ast::ItemKind::Mod(ref module) => {
                trace!("import module `{}` with {} items @ {}",
                       name,
                       module.items.len(),
                       self.code_span(item.span));

                self.visit_mod(module, item.span, item.id)
            }

            ast::ItemKind::ForeignMod(ref module) => {
                trace!("extern `{:?}` module with {} items @ {}",
                       module.abi,
                       module.items.len(),
                       self.code_span(item.span));

                walk_list!(self, visit_foreign_item, &module.items);
            }

            ast::ItemKind::Use(ref vp) => {
                let symbols = match vp.node {
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

                self.symbols.extend(symbols);
            }

            ast::ItemKind::Static(ref typ, _, ref expr) |
            ast::ItemKind::Const(ref typ, ref expr) => {
                trace!("define static/const `{}` : {:?}  = {:?} @ {}",
                       name,
                       typ,
                       expr,
                       self.code_span(item.span));

                let symbol = Symbol::define_global(&respan(item.span, item.ident), typ);

                self.symbols.push(symbol);

                self.visit_ty(typ);
                self.visit_expr(expr);
            }

            ast::ItemKind::Trait(_, ref generics, ref bounds, ref methods) => {
                trace!("define trait `{}` with {} items @ {}",
                       name,
                       methods.len(),
                       self.code_span(item.span));

                self.symbols.extend(Symbol::define_trait(&respan(item.span, item.ident), methods));

                self.visit_generics(generics);
                walk_list!(self, visit_ty_param_bound, bounds);
                walk_list!(self, visit_trait_item, methods);
            }

            ast::ItemKind::Enum(ref enum_definition, ref type_parameters) => {
                trace!("define enum `{}` with {} values @ {}",
                       name,
                       enum_definition.variants.len(),
                       self.code_span(item.span));

                self.symbols
                    .extend(Symbol::define_enum(&respan(item.span, item.ident), enum_definition));

                self.visit_generics(type_parameters);
                self.visit_enum_def(enum_definition, type_parameters, item.id, item.span)
            }

            ast::ItemKind::Struct(ref struct_definition, ref generics) => {
                trace!("define struct `{}` with {} fields @ {}",
                       name,
                       struct_definition.fields().len(),
                       self.code_span(item.span));

                let fields = if let &ast::VariantData::Struct(ref fields, _) = struct_definition {
                    fields.iter()
                        .filter_map(|ref field| {
                            field.ident.map(|ident| {
                                ast::SpannedIdent {
                                    node: ident,
                                    span: field.span,
                                }
                            })
                        })
                        .collect()
                } else {
                    Vec::new()
                };

                self.symbols.extend(Symbol::define_struct(&respan(item.span, item.ident), &fields));

                self.visit_generics(generics);
                self.visit_variant_data(struct_definition,
                                        item.ident,
                                        generics,
                                        item.id,
                                        item.span);
            }

            ast::ItemKind::Ty(ref typ, ref type_parameters) => {
                trace!("declare typedef `{}` = {:?} @ {}",
                       name,
                       typ,
                       self.code_span(item.span));

                let symbol = Symbol::declare_typedef(&respan(item.span, item.ident), &**typ);

                self.symbols.push(symbol);

                self.visit_ty(typ);
                self.visit_generics(type_parameters)
            }

            _ => visit::walk_item(self, item),
        }
    }

    fn visit_fn(&mut self,
                kind: visit::FnKind,
                decl: &ast::FnDecl,
                block: &ast::Block,
                span: Span,
                _: ast::NodeId) {
        let symbols = match kind {
            /// fn foo() or extern "Abi" fn foo()
            visit::FnKind::ItemFn(ref ident, _, _, _, _, _) => {
                trace!("declare function `{}` @ {}",
                       ident.name.as_str(),
                       self.code_span(span));

                Symbol::declare_func(Some(ident.name), span, decl)
            }

            /// fn foo(&self)
            visit::FnKind::Method(ref ident, _, _) => {
                trace!("declare method `{}`(...) @ {}",
                       ident.name.as_str(),
                       self.code_span(span));

                Symbol::declare_func(Some(ident.name), span, decl)
            }

            /// |x, y| {}
            visit::FnKind::Closure => {
                trace!("declare closure @ {}", self.code_span(span));

                Symbol::declare_func(None, span, decl)
            }
        };

        self.symbols.extend(symbols);

        visit::walk_fn(self, kind, decl, block, span);
    }

    fn visit_macro_def(&mut self, macro_def: &ast::MacroDef) {
        debug!("define macro `{}` @ {}",
               macro_def.ident.name.as_str(),
               self.code_span(macro_def.span));

        let symbols = Symbol::define_macro(macro_def);

        self.symbols.extend(symbols);

        visit::walk_opt_ident(self, macro_def.span, macro_def.imported_from);
        walk_list!(self, visit_attribute, &macro_def.attrs);
    }

    fn visit_mac(&mut self, mac: &ast::Mac) {
        trace!("use macro `{}` @ {}",
               mac.node.path,
               self.code_span(mac.span));

        self.symbols.push(Symbol::use_macro(mac));
    }

    fn visit_local(&mut self, local: &ast::Local) {
        let name = pprust::pat_to_string(&*local.pat);

        trace!("define local variable `{}` @ {}",
               name,
               self.code_span(local.span));

        self.symbols.push(Symbol::define_local(&respan(local.span, &name), &local.ty));

        visit::walk_local(self, local);
    }

    fn visit_expr(&mut self, expr: &ast::Expr) {
        match expr.node {
            ast::ExprKind::Call(ref callee, ref args) => {
                let name = pprust::expr_to_string(callee);

                trace!("call function `{}` ({}) @ {}",
                       name,
                       args.iter()
                           .map(|expr| pprust::expr_to_string(expr))
                           .collect::<Vec<String>>()
                           .join(", "),
                       self.code_span(callee.span));

                self.symbols.push(Symbol::call_func(&respan(callee.span, &name)))
            }
            ast::ExprKind::MethodCall(ref ident, _, ref args) => {
                let name = ident.node.name.as_str();

                trace!("call method `{}` ({}) arguments @ {}",
                       name,
                       args.iter()
                           .map(|expr| pprust::expr_to_string(expr))
                           .collect::<Vec<String>>()
                           .join(", "),
                       self.code_span(ident.span));

                self.symbols.push(Symbol::call_func(&respan(ident.span, &name)))
            }
            _ => {}
        };

        visit::walk_expr(self, expr);
    }
}

pub fn extract_symbols(src_path: &Path) -> Result<Vec<SourceFile>> {
    let cfg = Vec::new();

    let mut visitor = SourceFileVisitor {
        session: parse::ParseSess::new(),
        symbols: Vec::new(),
    };

    let krate = try!(parse::parse_crate_from_file(&src_path, cfg, &visitor.session));

    debug!("walking crate with {} exported macros @ {}",
           krate.exported_macros.len(),
           visitor.code_span(krate.span));

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
