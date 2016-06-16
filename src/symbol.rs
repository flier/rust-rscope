use std::iter::Extend;
use std::string::ToString;

use syntex_syntax::ast::{self, SpannedIdent};
use syntex_syntax::codemap::{Span, Spanned, spanned, respan};
use syntex_syntax::parse::token::keywords;
use syntex_syntax::ptr::P;

#[derive(PartialEq, Eq, PartialOrd, Debug, Clone, Copy)]
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
    MacroDef = b'#',
    /// #define end
    MacroEnd = b')',
    /// #include
    Include = b'~',
    /// direct assignment, increment, or decrement
    Assignment = b'=',
    /// enum/struct/union definition end
    DefineEnd = b';',
    /// class definition
    ClassDef = b'c',
    /// enum definition
    EnumDef = b'e',
    /// other global definition
    GlobalDef = b'g',
    /// function/block local definition
    LocalDef = b'l',
    /// global enum/struct/union member definition
    MemberDef = b'm',
    /// function parameter definition
    ParamDef = b'p',
    /// struct definition
    StructDef = b's',
    /// typedef definition
    TypedefDef = b't',
    /// union definition
    #[allow(dead_code)]
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
    pub fn use_ident(ident: &SpannedIdent) -> Symbol {
        Token::Ident.with_ident(ident)
    }

    pub fn import_crate(ident: &SpannedIdent) -> Symbol {
        Token::Include.with_ident(ident)
    }

    pub fn use_module(path: &ast::Path, items: Option<&Vec<ast::PathListItem>>) -> Vec<Symbol> {
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

    pub fn declare_func(name: Option<ast::Name>,
                        span: Span,
                        func_decl: &ast::FnDecl)
                        -> Vec<Symbol> {
        let name = name.map_or_else(|| "".to_string(), |s| s.to_string());

        let mut symbols = vec![Token::FuncDef.with_name(&respan(span, &name)),
                               Token::FuncEnd.with_name(&spanned(span.hi, span.hi, &name))];

        symbols.extend(func_decl.inputs.iter().filter_map(|arg| {
            match arg.pat.node {
                ast::PatKind::Ident(_, ref ident, _) if ident.node.name !=
                                                        keywords::SelfValue.name() => {
                    Some(Token::ParamDef.with_ident(ident))
                }
                _ => None,
            }
        }));

        symbols
    }

    pub fn define_trait(ident: &SpannedIdent,
                        trait_definition: &Vec<ast::TraitItem>)
                        -> Vec<Symbol> {
        let mut symbols = vec![Token::ClassDef.with_ident(ident)];

        symbols.extend(trait_definition.iter()
            .filter_map(|ref item| {
                match item.node {
                    ast::TraitItemKind::Const(_, _) => {
                        Some(Token::MemberDef.with_ident(&respan(item.span, item.ident)))
                    }
                    ast::TraitItemKind::Type(_, _) => {
                        Some(Token::TypedefDef.with_ident(&respan(item.span, item.ident)))
                    }
                    ast::TraitItemKind::Method(_, _) => None,
                }
            }));

        symbols
    }

    pub fn define_enum(ident: &SpannedIdent, definition: &ast::EnumDef) -> Vec<Symbol> {
        let mut symbols = vec![Token::EnumDef.with_ident(ident),
                 Token::DefineEnd.with_ident(&spanned(ident.span.hi, ident.span.hi, ident.node))];

        symbols.extend(definition.variants
            .iter()
            .map(|var| Token::MemberDef.with_ident(&respan(var.span, var.node.name))));

        symbols
    }

    pub fn define_struct(ident: &SpannedIdent, fields: &Vec<SpannedIdent>) -> Vec<Symbol> {
        let mut symbols = vec![Token::StructDef.with_ident(ident),
                 Token::DefineEnd.with_ident(&spanned(ident.span.hi, ident.span.hi, ident.node))];

        symbols.extend(fields.iter().map(|ref field| Token::MemberDef.with_ident(field)));

        symbols
    }

    pub fn declare_typedef(ident: &SpannedIdent, _: &ast::Ty) -> Symbol {
        Token::TypedefDef.with_ident(ident)
    }

    pub fn define_global(ident: &SpannedIdent, _: &P<ast::Ty>) -> Symbol {
        Token::GlobalDef.with_ident(ident)
    }

    pub fn define_local(name: &Spanned<&str>, _: &Option<P<ast::Ty>>) -> Symbol {
        Token::LocalDef.with_name(name)
    }

    pub fn define_macro(mac: &ast::MacroDef) -> Vec<Symbol> {
        vec![Token::MacroDef.with_ident(&respan(mac.span, mac.ident)),
             Token::MacroEnd.with_ident(&spanned(mac.span.hi, mac.span.hi, mac.ident))]
    }

    pub fn use_macro(mac: &ast::Mac) -> Symbol {
        Token::Ident.with_name(&respan(mac.span, &mac.node.path))
    }

    pub fn call_func(name: &Spanned<&str>) -> Symbol {
        Token::FuncCall.with_name(name)
    }

    pub fn assign_to(name: &Spanned<&str>) -> Symbol {
        Token::Assignment.with_name(name)
    }
}
