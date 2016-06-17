use std::str::{self, FromStr};

use nom::{digit, space, tab, newline};

use symbol::Token;

pub struct CrossRef<'a> {
    header: Header<'a>,
    files: Vec<SourceFile<'a>>,
    trailer: Trailer<'a>,
}

#[derive(PartialEq, Eq, PartialOrd, Debug)]
pub struct Header<'a> {
    fmt_ver: usize,
    cur_dir: &'a str,
    no_compress: Option<bool>,
    inverted_index: Option<bool>,
    truncate_symbol: Option<bool>,
    symbols_off: Option<usize>,
    trailer_off: usize,
}

#[derive(PartialEq, Eq, PartialOrd, Debug)]
pub struct Trailer<'a> {
    src_dirs: Vec<&'a str>,
    inc_dirs: Vec<&'a str>,
    src_files: Vec<&'a str>,
    skip_spaces: usize,
}

#[derive(PartialEq, Eq, PartialOrd, Debug)]
pub struct SourceFile<'a> {
    filename: &'a str,
    lines: Vec<SourceLine<'a>>,
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

named!(pub header<Header>,
    chain!(
        tag!("cscope") ~
        space ~
        fmt_ver: digits ~
        space ~
        cur_dir: map_res!(take_until!(" "), str::from_utf8) ~
        space ~
        no_compress: map!(tag!("-c"), |_| true)? ~
        space? ~
        inverted_index: map!(tag!("-q"), |_| true)? ~
        space? ~
        symbols_off: digits? ~
        space? ~
        truncate_symbol: map!(tag!("-T"), |_| true)? ~
        space? ~
        trailer_off: digits ~
        newline,
        || {
            Header {
                fmt_ver: fmt_ver,
                cur_dir: cur_dir,
                no_compress: no_compress,
                inverted_index: inverted_index,
                truncate_symbol: truncate_symbol,
                symbols_off: symbols_off,
                trailer_off: trailer_off,
            }
        }
    )
);

named!(path_line<&str>,
    map_res!(
        chain!(
            path: take_until!("\n") ~
            newline,
            || path
        ),
        str::from_utf8
    )
);

named!(pub trailer<Trailer>,
    chain!(
        src_dirs_num: digits ~ newline ~ // number of source directories
        src_dirs: many_m_n!(0, src_dirs_num, path_line) ~
        inc_dirs_num: digits ~ newline ~ // number of include directories
        inc_dirs: many_m_n!(0, inc_dirs_num, path_line) ~
        src_files_num: digits ~ newline ~ // number of source and included files
        skip_spaces: digits ~ newline ~ // length of string space needed
        src_files: many_m_n!(0, src_files_num, path_line),
        || {
            Trailer {
                src_dirs: src_dirs,
                inc_dirs: inc_dirs,
                src_files: src_files,
                skip_spaces: skip_spaces,
            }
        }
    )
);

named!(pub token<Token>, alt!(
        chain!(char!('$'), || Token::FuncDef) |
        chain!(char!('`'), || Token::FuncCall) |
        chain!(char!('}'), || Token::FuncEnd) |
        chain!(char!('#'), || Token::MacroDef) |
        chain!(char!(')'), || Token::MacroEnd) |
        chain!(char!('~') ~ alt!(char!('<') | char!('"')), || Token::Include) |
        chain!(char!('='), || Token::Assignment) |
        chain!(char!(';'), || Token::DefineEnd) |
        chain!(char!('c'), || Token::ClassDef) |
        chain!(char!('e'), || Token::EnumDef) |
        chain!(char!('g'), || Token::GlobalDef) |
        chain!(char!('l'), || Token::LocalDef) |
        chain!(char!('m'), || Token::MemberDef) |
        chain!(char!('p'), || Token::ParamDef) |
        chain!(char!('s'), || Token::StructDef) |
        chain!(char!('t'), || Token::TypedefDef) |
        chain!(char!('u'), || Token::UnionDef)
    )
);

named!(pub symbol<Symbol>,
    chain!(
        tab ~
        token: token ~
        text: take_until!("\n") ~
        newline,
        || Symbol::Symbol(token, text)
    )
);

named!(pub data<Symbol>,
    chain!(
        text: is_not!("\t\n") ~
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

named!(pub source_file<SourceFile>,
    chain!(
        tab ~ char!('@') ~
        filename: map_res!(take_until!("\n"), str::from_utf8) ~
        newline ~
        newline ~
        lines: many0!(source_line),
        || {
            SourceFile {
                filename: filename,
                lines: lines,
            }
        }
    )
);

#[cfg(test)]
mod tests {
    use nom::IResult;

    use super::*;
    use super::super::symbol::Token;

    #[test]
    fn parse_header() {
        assert_eq!(header(b"cscope 15 $HOME/github/rust -q 0000134629 0061565201\n"),
            IResult::Done(&[][..],
                Header {
                    fmt_ver: 15,
                    cur_dir: "$HOME/github/rust",
                    no_compress: None,
                    inverted_index: Some(true),
                    truncate_symbol: None,
                    symbols_off: Some(134629),
                    trailer_off: 61565201,
                }
            )
        );
    }

    #[test]
    fn parse_trailer() {
        assert_eq!(trailer(br#"1
.
0
4859
234564
src/compiler-rt/include/sanitizer/allocator_interface.h
src/compiler-rt/include/sanitizer/asan_interface.h
"#),
            IResult::Done(&[][..],
                Trailer {
                    src_dirs: vec!["."],
                    inc_dirs: vec![],
                    src_files: vec![
                        "src/compiler-rt/include/sanitizer/allocator_interface.h",
                        "src/compiler-rt/include/sanitizer/asan_interface.h"
                    ],
                    skip_spaces: 234564,
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
        let lines = br#"11940 {
INT64_C
(0x0000810020000020), 0x1.02004000004
p
+47, 0x0p+0 },

"#;

        assert_eq!(source_line(lines), IResult::Done(&[][..],
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

    #[test]
    fn parse_source_file() {
        let lines = b"\t@src/compiler-rt/test/builtins/Unit/floatditf_test.c\n\
\n\
14 \n\
\t~\"int_lib.h\n\
\"\n\
\n\
15 \n\
\t~<float.h\n\
>\n\
\n";

        assert_eq!(source_file(lines), IResult::Done(&[][..],
            SourceFile {
                filename: "src/compiler-rt/test/builtins/Unit/floatditf_test.c",
                lines: vec![
                    SourceLine {
                        line_num: 14,
                        symbols: vec![
                            Symbol::Text(b"\x02"),
                            Symbol::Symbol(Token::Include, b"int_lib.h"),
                            Symbol::Text(b"\"")
                        ],
                    },
                    SourceLine {
                        line_num: 15,
                        symbols: vec![
                            Symbol::Text(b"\x02"),
                            Symbol::Symbol(Token::Include, b"float.h"),
                            Symbol::Text(b">")
                        ],
                    }
                ],
            }
        ));
    }
}
