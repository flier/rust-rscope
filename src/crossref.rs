use std::str::{self, FromStr};
use std::path::PathBuf;

use nom::{digit, space, tab, newline};

use symbol::Token;

pub struct CrossRef {
    header: Header,
}

#[derive(PartialEq, Eq, PartialOrd, Debug)]
pub struct Header {
    fmt_ver: usize,
    cur_dir: PathBuf,
    no_compress: Option<bool>,
    has_symbols: Option<bool>,
    short_match: Option<bool>,
    symbols_off: Option<usize>,
    trailer_off: usize,
}

#[derive(PartialEq, Eq, PartialOrd, Debug)]
pub struct SourceFile {
    filename: PathBuf,
}

#[derive(PartialEq, Eq, PartialOrd, Debug)]
pub enum Symbol<'a> {
    Symbol(Token, &'a [u8]),
    Text(&'a [u8]),
}

#[derive(PartialEq, Eq, PartialOrd, Debug)]
pub struct SourceLine<'a> {
    line_num: usize,
    symbols: Vec<Symbol<'a>>,
}

named!(digits<usize>,
    map_res!(
        map_res!(
            digit,
            str::from_utf8
        ),
        FromStr::from_str
    )
);

named!(path<PathBuf>,
    map!(
        map_res!(
            take_until_either!(" \n"),
            str::from_utf8
        ),
        PathBuf::from
    )
);

named!(pub db_header<Header>,
    chain!(
        tag!("cscope") ~
        space ~
        fmt_ver: digits ~
        space ~
        cur_dir: path ~
        space ~
        no_compress: map!(tag!("-c"), |_| true)? ~
        space? ~
        has_symbols: map!(tag!("-q"), |_| true)? ~
        space? ~
        symbols_off: digits? ~
        space? ~
        short_match: map!(tag!("-T"), |_| true)? ~
        space? ~
        trailer_off: digits ~
        newline,
        || {
            Header {
                fmt_ver: fmt_ver,
                cur_dir: cur_dir,
                no_compress: no_compress,
                has_symbols: has_symbols,
                short_match: short_match,
                symbols_off: symbols_off,
                trailer_off: trailer_off,
            }
        }
    )
);

named!(pub token<Token>, alt!(
        chain!(tab ~ char!('$'), || Token::FuncDef) |
        chain!(tab ~ char!('`'), || Token::FuncCall) |
        chain!(tab ~ char!('}'), || Token::FuncEnd) |
        chain!(tab ~ char!('#'), || Token::MacroDef) |
        chain!(tab ~ char!(')'), || Token::MacroEnd) |
        chain!(tab ~ char!('~') ~ alt!(char!('<') | char!('"')), || Token::Include) |
        chain!(tab ~ char!('='), || Token::Assignment) |
        chain!(tab ~ char!(';'), || Token::DefineEnd) |
        chain!(tab ~ char!('c'), || Token::ClassDef) |
        chain!(tab ~ char!('e'), || Token::EnumDef) |
        chain!(tab ~ char!('g'), || Token::GlobalDef) |
        chain!(tab ~ char!('l'), || Token::LocalDef) |
        chain!(tab ~ char!('m'), || Token::MemberDef) |
        chain!(tab ~ char!('p'), || Token::ParamDef) |
        chain!(tab ~ char!('s'), || Token::StructDef) |
        chain!(tab ~ char!('t'), || Token::TypedefDef) |
        chain!(tab ~ char!('u'), || Token::UnionDef)
    )
);

named!(pub symbol<Symbol>,
    chain!(
        token: token ~
        text: take_until!("\n") ~
        newline,
        || Symbol::Symbol(token, text)
    )
);

named!(pub data<Symbol>,
    chain!(
        text: is_not!("\n") ~
        newline,
        || Symbol::Text(text)
    )
);

named!(pub source_line<SourceLine>,
    chain!(
        line_num: digits ~
        space ~
        symbols: many1!(alt!(symbol | data)) ~
        newline,
        || {
            SourceLine {
                line_num: line_num,
                symbols: symbols,
            }
        }
    )
);

named!(pub file_header<SourceFile>,
    chain!(
        tab ~
        char!('@') ~
        filename: path ~
        newline,
        || {
            SourceFile {
                filename: filename,
            }
        }
    )
);

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use nom::IResult;

    use super::*;
    use super::super::symbol::Token;

    #[test]
    fn parse_db_header() {
        assert_eq!(db_header(b"cscope 15 $HOME/github/rust -q 0000134629 0061565201\n"),
            IResult::Done(&[][..],
                Header{
                    fmt_ver: 15,
                    cur_dir: PathBuf::from("$HOME/github/rust"),
                    no_compress: None,
                    has_symbols: Some(true),
                    short_match: None,
                    symbols_off: Some(134629),
                    trailer_off: 61565201,
                }
            )
        );
    }

    #[test]
    fn parse_file_header() {
        assert_eq!(file_header(b"\t@src/compiler-rt/include/sanitizer/allocator_interface.h\n"),
            IResult::Done(&[][..],
                SourceFile {
                    filename:
                        PathBuf::from("src/compiler-rt/include/sanitizer/allocator_interface.h"),
                }
            )
        );
    }

    #[test]
    fn parse_symbol() {
        assert_eq!(symbol(b"\t$Block_size\n"), IResult::Done(&[][..],
            Symbol::Symbol(Token::FuncDef, b"Block_size")));

        assert_eq!(symbol(b"\t`_Block_assign\n"), IResult::Done(&[][..],
            Symbol::Symbol(Token::FuncCall, b"_Block_assign")));

        assert_eq!(symbol(b"\t}\n"), IResult::Done(&[][..],
            Symbol::Symbol(Token::FuncEnd, b"")));

        assert_eq!(symbol(b"\t#ASAN_ALLOCATOR_H\n"), IResult::Done(&[][..],
            Symbol::Symbol(Token::MacroDef, b"ASAN_ALLOCATOR_H")));

        assert_eq!(symbol(b"\t)\n"), IResult::Done(&[][..],
            Symbol::Symbol(Token::MacroEnd, b"")));

        assert_eq!(symbol(b"\t~<sys/time.h\n"), IResult::Done(&[][..],
            Symbol::Symbol(Token::Include, b"sys/time.h")));
        assert_eq!(symbol(b"\t~\"sys/time.h\n"), IResult::Done(&[][..],
            Symbol::Symbol(Token::Include, b"sys/time.h")));

        assert_eq!(symbol(b"\tcFakeSck\n"), IResult::Done(&[][..],
            Symbol::Symbol(Token::ClassDef, b"FakeSck")));
        assert_eq!(symbol(b"\teLE_RESULT\n"), IResult::Done(&[][..],
            Symbol::Symbol(Token::EnumDef, b"LE_RESULT")));
        assert_eq!(symbol(b"\tgkShadowAddr\n"), IResult::Done(&[][..],
            Symbol::Symbol(Token::GlobalDef, b"kShadowAddr")));
        assert_eq!(symbol(b"\tli\n"), IResult::Done(&[][..],
            Symbol::Symbol(Token::LocalDef, b"i")));
        assert_eq!(symbol(b"\tmLE_LESS\n"), IResult::Done(&[][..],
            Symbol::Symbol(Token::MemberDef, b"LE_LESS")));

        assert_eq!(symbol(b"\tpthis\n"), IResult::Done(&[][..],
            Symbol::Symbol(Token::ParamDef, b"this")));
        assert_eq!(symbol(b"\tsBlock_basic\n"), IResult::Done(&[][..],
            Symbol::Symbol(Token::StructDef, b"Block_basic")));
        assert_eq!(symbol(b"\ttCache\n"), IResult::Done(&[][..],
            Symbol::Symbol(Token::TypedefDef, b"Cache")));
        assert_eq!(symbol(b"\tuW128_T\n"), IResult::Done(&[][..],
            Symbol::Symbol(Token::UnionDef, b"W128_T")));
    }

    #[test]
    fn parse_source_line() {
        let line = br#"11940 {
INT64_C
(0x0000810020000020), 0x1.02004000004
p
+47, 0x0p+0 },

"#;

        assert_eq!(source_line(line), IResult::Done(&[][..],
            SourceLine {
                line_num: 11940,
                symbols: vec![
                    Symbol::Text(b"{"),
                    Symbol::Text(b"INT64_C"),
                    Symbol::Text(b"(0x0000810020000020), 0x1.02004000004"),
                    Symbol::Text(b"p"),
                    Symbol::Text(b"+47, 0x0p+0 },")
                ],
            }
        ));
    }
}
