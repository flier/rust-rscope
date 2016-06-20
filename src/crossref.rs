use std::fmt;
use std::mem;
use std::marker;
use std::str::{self, FromStr};

use nom::{IResult, digit, space, tab, newline};

use errors::{ErrorKind, Result};
use symbol::Token;
use digraph::{self, Decoder};

pub struct CrossRef<'a> {
    pub header: Header<'a>,
    pub files: Vec<SourceFile<'a>>,
    pub trailer: Trailer<'a>,
}

#[derive(PartialEq, Eq, PartialOrd, Debug)]
pub struct Header<'a> {
    pub fmt_ver: usize,
    pub cur_dir: &'a str,
    pub no_compress: Option<bool>,
    pub inverted_index: Option<bool>,
    pub truncate_symbol: Option<bool>,
    pub symbols_off: Option<usize>,
    pub trailer_off: usize,
}

#[derive(PartialEq, Eq, PartialOrd, Debug)]
pub struct Trailer<'a> {
    pub src_dirs_num: usize,
    pub src_dirs: Vec<&'a str>,
    pub inc_dirs_num: usize,
    pub inc_dirs: Vec<&'a str>,
    pub src_files_num: usize,
    pub skip_spaces: usize,
    pub src_files: Vec<&'a str>,
}

#[derive(PartialEq, Eq, PartialOrd, Debug)]
pub struct SourceFile<'a> {
    pub filename: &'a str,
    pub lines: Vec<SourceLine<'a>>,
}

#[derive(PartialEq, Eq, PartialOrd, Debug)]
pub enum Symbol<'a> {
    Symbol(Token, &'a [u8]),
    Text(&'a [u8]),
}

#[derive(PartialEq, Eq, PartialOrd, Debug)]
pub struct SourceLine<'a> {
    pub line_num: usize,
    pub symbols: Vec<Symbol<'a>>,
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
                src_dirs_num: src_dirs_num,
                src_dirs: src_dirs,
                inc_dirs_num: inc_dirs_num,
                inc_dirs: inc_dirs,
                src_files_num: src_files_num,
                skip_spaces: skip_spaces,
                src_files: src_files,
            }
        }
    )
);

named!(pub token<Token>, alt!(
        map!(char!('$'), |_| Token::FuncDef) |
        map!(char!('`'), |_| Token::FuncCall) |
        map!(char!('}'), |_| Token::FuncEnd) |
        map!(char!('#'), |_| Token::MacroDef) |
        map!(char!(')'), |_| Token::MacroEnd) |
        map!(char!('~'), |_| Token::Include) |
        map!(char!('='), |_| Token::Assignment) |
        map!(char!(';'), |_| Token::DefineEnd) |
        map!(char!('c'), |_| Token::ClassDef) |
        map!(char!('e'), |_| Token::EnumDef) |
        map!(char!('g'), |_| Token::GlobalDef) |
        map!(char!('l'), |_| Token::LocalDef) |
        map!(char!('m'), |_| Token::MemberDef) |
        map!(char!('p'), |_| Token::ParamDef) |
        map!(char!('s'), |_| Token::StructDef) |
        map!(char!('t'), |_| Token::TypedefDef) |
        map!(char!('u'), |_| Token::UnionDef)
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

pub fn parse<'a>(buf: &'a [u8]) -> IResult<&'a [u8], CrossRef<'a>> {
    let (rest, header) = try_parse!(buf, header);
    let (symbols_buf, trailer_buf) = rest.split_at(header.trailer_off);
    let (rest, trailer) = try_parse!(trailer_buf, trailer);
    let (_, files) = try_parse!(symbols_buf, many_m_n!(0, trailer.src_files.len(), source_file));

    IResult::Done(rest,
                  CrossRef {
                      header: header,
                      files: files,
                      trailer: trailer,
                  })
}

#[derive(PartialEq, Eq, PartialOrd, Debug)]
pub enum VisitResult<T> {
    Terminated(T),
    Continue,
}

pub trait Visitor<T> {
    fn visit_header<'a>(&mut self, header: &Header<'a>) -> Result<VisitResult<T>> {
        Ok(VisitResult::Continue)
    }

    fn visit_trailer<'a>(&mut self, trailer: &Trailer<'a>) -> Result<VisitResult<T>> {
        Ok(VisitResult::Continue)
    }

    fn visit_source_file<'a>(&mut self, file: &SourceFile<'a>) -> Result<VisitResult<T>>
        where Self: TextDecoder + marker::Sized
    {
        walk_source_file(file, self)
    }

    fn visit_source_line<'a>(&mut self,
                             filename: &str,
                             line: &SourceLine<'a>)
                             -> Result<VisitResult<T>>
        where Self: TextDecoder + marker::Sized
    {
        walk_source_line(filename, line, self)
    }

    fn visit_symbol(&mut self,
                    filename: &str,
                    line_num: usize,
                    token: Token,
                    text: String)
                    -> Result<VisitResult<T>> {
        Ok(VisitResult::Continue)
    }

    fn visit_text(&mut self,
                  filename: &str,
                  line_num: usize,
                  text: String)
                  -> Result<VisitResult<T>> {
        Ok(VisitResult::Continue)
    }
}

macro_rules! try_walk {
    ($expr:expr) => (
        match try!($expr) {
            VisitResult::Continue => {}
            VisitResult::Terminated(ret) => return Ok(VisitResult::Terminated(ret)),
        }
    )
}

pub trait TextDecoder {
    fn decode<'a>(&self, s: &'a [u8]) -> Result<String>;
}

pub struct SimpleVisitor {
    decoder: Option<digraph::Digraph>,
}

impl SimpleVisitor {
    fn new(encoded: bool) -> Self {
        SimpleVisitor {
            decoder: if encoded {
                Some(digraph::new())
            } else {
                None
            },
        }
    }
}

impl Default for SimpleVisitor {
    fn default() -> Self {
        unsafe { mem::zeroed() }
    }
}

impl TextDecoder for SimpleVisitor {
    fn decode<'a>(&self, s: &'a [u8]) -> Result<String> {
        Ok(if let Some(ref decoder) = self.decoder {
            try!(String::from_utf8(decoder.decode(s)))
        } else {
            String::from(try!(str::from_utf8(s)))
        })
    }
}

impl<T> Visitor<T> for SimpleVisitor {
    fn visit_header<'a>(&mut self, header: &Header<'a>) -> Result<VisitResult<T>> {
        debug!("visit header v{} with {} text", header.fmt_ver,
               if self.decoder.is_some() { "encoded"} else { "plain" } );

        if let Some(true) = header.no_compress {
            self.decoder = Some(digraph::new());
        }

        debug!("visit header v{} with {} text", header.fmt_ver,
               if self.decoder.is_some() { "encoded"} else { "plain" } );

        Ok(VisitResult::Continue)
    }
}

pub fn walk_source_file<'a, T, V>(file: &SourceFile<'a>, visitor: &mut V) -> Result<VisitResult<T>>
    where V: Visitor<T> + TextDecoder + marker::Sized
{
    debug!("visit source file `{}` with {} lines", file.filename, file.lines.len());

    for line in &file.lines {
        try_walk!(visitor.visit_source_line(file.filename, &line));
    }

    Ok(VisitResult::Continue)
}

pub fn walk_source_line<'a, T, V>(filename: &str,
                                  line: &SourceLine<'a>,
                                  visitor: &mut V)
                                  -> Result<VisitResult<T>>
    where V: Visitor<T> + TextDecoder + marker::Sized
{
    debug!("visit line #{} with {} symbols", line.line_num, line.symbols.len());

    for symbol in &line.symbols {
        try_walk!(walk_symbol(filename, line.line_num, symbol, visitor));
    }

    Ok(VisitResult::Continue)
}

pub fn walk_symbol<'a, T, V: Visitor<T> + TextDecoder>(filename: &str,
                                                       line_num: usize,
                                                       symbol: &Symbol,
                                                       visitor: &mut V)
                                                       -> Result<VisitResult<T>> {
    match *symbol {
        Symbol::Symbol(token, text) => {
            let text = try!(visitor.decode(text));

            debug!("visit {:?} symbol: `{}`", token, text);

            try_walk!(visitor.visit_symbol(filename, line_num, token, text));
        }
        Symbol::Text(text) => {
            let text = try!(visitor.decode(text));

            debug!("visit text: `{}`", text);

            try_walk!(visitor.visit_text(filename, line_num, text));
        }
    }

    Ok(VisitResult::Continue)
}

macro_rules! try_parse {
    ($i:expr, $submac:ident!( $($args:tt)* )) => (
        match $submac!($i, $($args)*) {
            ::nom::IResult::Done(i,o) => (i,o),
            ::nom::IResult::Error(err) => {
                return Err(err.into());
            },
            ::nom::IResult::Incomplete(needed) => {
                return Err(ErrorKind::NoMoreData(needed).into());
            },
        }
    );
    ($i:expr, $f:expr) => (
        try_parse!($i, call!($f))
    );
}

pub fn walk<'a, T, V>(buf: &'a [u8], visitor: &mut V) -> Result<VisitResult<T>>
    where V: Visitor<T> + TextDecoder
{
    let (rest, header) = try_parse!(buf, header);

    try_walk!(visitor.visit_header(&header));

    let (symbols_buf, trailer_buf) = rest.split_at(header.trailer_off);

    let (_, trailer) = try_parse!(trailer_buf, trailer);

    try_walk!(visitor.visit_trailer(&trailer));

    let mut buf = symbols_buf;

    for _ in 0..trailer.src_files.len() {
        let (rest, file) = try_parse!(buf, source_file);

        try_walk!(visitor.visit_source_file(&file));

        buf = rest;
    }

    Ok(VisitResult::Continue)
}

impl<'a> fmt::Display for Header<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let symbols_off = self.symbols_off.map_or(String::new(), |off| format!(" {:010}", off));

        writeln!(f,
               "cscope 15 {}{}{}{}{} {:010}",
               self.cur_dir,
               self.no_compress.map_or("", |_| " -c"),
               self.inverted_index.map_or("", |_| " -q"),
               symbols_off,
               self.truncate_symbol.map_or( "", |_| " -T"),
               self.trailer_off
        )
    }
}

impl<'a> fmt::Display for Trailer<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(writeln!(f, "{}", self.src_dirs_num));
        for dir in &self.src_dirs {
            try!(writeln!(f, "{}", dir));
        }
        try!(writeln!(f, "{}", self.inc_dirs_num));
        for dir in &self.inc_dirs {
            try!(writeln!(f, "{}", dir));
        }
        try!(writeln!(f, "{}", self.src_files_num));
        try!(writeln!(f, "{}", self.skip_spaces));
        for file in &self.src_files {
            try!(writeln!(f, "{}", file));
        }

        Ok(())
    }
}

impl<'a> fmt::Display for Symbol<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Symbol::Symbol(token, text) => {
                writeln!(f, "\t{}{}", token, unsafe { str::from_utf8_unchecked(text) })
            }
            Symbol::Text(text) => writeln!(f, "{}", unsafe { str::from_utf8_unchecked(text) }),
        }
    }
}

impl<'a> fmt::Display for SourceLine<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "{} ", self.line_num));

        for symbol in &self.symbols {
            try!(write!(f, "{}", symbol));
        }

        writeln!(f, "")
    }
}

impl<'a> fmt::Display for SourceFile<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(writeln!(f, "\t@{}\n", self.filename));

        for line in &self.lines {
            try!(write!(f, "{}", line));
        }

        Ok(())
    }
}

impl<'a> fmt::Display for CrossRef<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "{}", self.header));

        for file in &self.files {
            try!(write!(f, "{}", file));
        }

        write!(f, "{}", self.trailer)
    }
}

#[cfg(test)]
mod tests {
    extern crate env_logger;

    use std::str;

    use nom::IResult;

    use super::*;
    use super::super::symbol::Token;

    macro_rules! assert_parser {
        ($buf:expr, $parser:ident, $target:expr) => ({
            assert_eq!($target.to_string().as_bytes(), &$buf[..]);
            assert_eq!($parser($buf), IResult::Done(&[][..], $target));
        })
    }

    macro_rules! assert_symbol {
        ($buf:expr, $token:path, $text:expr) => (
            assert_parser!($buf, symbol, Symbol::Symbol($token, $text))
        )
    }

    #[test]
    fn parse_header() {
        let _ = env_logger::init();

        assert_parser!(
            b"cscope 15 $HOME/github/rust -q 0000134629 0061565201\n",
            header,
            Header {
                fmt_ver: 15,
                cur_dir: "$HOME/github/rust",
                no_compress: None,
                inverted_index: Some(true),
                truncate_symbol: None,
                symbols_off: Some(134629),
                trailer_off: 61565201,
            });
    }

    #[test]
    fn parse_trailer() {
        let _ = env_logger::init();

        assert_parser!(
            br#"1
.
0
4859
234564
src/compiler-rt/include/sanitizer/allocator_interface.h
src/compiler-rt/include/sanitizer/asan_interface.h
"#,
            trailer,
            Trailer {
                src_dirs_num: 1,
                src_dirs: vec!["."],
                inc_dirs_num: 0,
                inc_dirs: vec![],
                src_files_num: 4859,
                skip_spaces: 234564,
                src_files: vec![
                    "src/compiler-rt/include/sanitizer/allocator_interface.h",
                    "src/compiler-rt/include/sanitizer/asan_interface.h"
                ],
            });
    }

    #[test]
    fn parse_symbol() {
        let _ = env_logger::init();

        assert_symbol!(b"\t$Block_size\n", Token::FuncDef, b"Block_size");
        assert_symbol!(b"\t`_Block_assign\n", Token::FuncCall, b"_Block_assign");
        assert_symbol!(b"\t}\n", Token::FuncEnd, b"");

        assert_symbol!(b"\t#ASAN_ALLOCATOR_H\n", Token::MacroDef, b"ASAN_ALLOCATOR_H");
        assert_symbol!(b"\t)\n", Token::MacroEnd, b"");

        assert_symbol!(b"\t~<sys/time.h\n", Token::Include, b"<sys/time.h");
        assert_symbol!(b"\t~\"sys/time.h\n", Token::Include, b"\"sys/time.h");

        assert_symbol!(b"\tcFakeSck\n", Token::ClassDef, b"FakeSck");
        assert_symbol!(b"\teLE_RESULT\n", Token::EnumDef, b"LE_RESULT");
        assert_symbol!(b"\tgkShadowAddr\n", Token::GlobalDef, b"kShadowAddr");
        assert_symbol!(b"\tli\n", Token::LocalDef, b"i");
        assert_symbol!(b"\tmLE_LESS\n", Token::MemberDef, b"LE_LESS");

        assert_symbol!(b"\tpthis\n", Token::ParamDef, b"this");
        assert_symbol!(b"\tsBlock_basic\n", Token::StructDef, b"Block_basic");
        assert_symbol!(b"\ttCache\n", Token::TypedefDef, b"Cache");
        assert_symbol!(b"\tuW128_T\n", Token::UnionDef, b"W128_T");
    }

    #[test]
    fn parse_source_line() {
        let _ = env_logger::init();

        assert_parser!(
            br#"11940 {
INT64_C
(0x0000810020000020), 0x1.02004000004
p
+47, 0x0p+0 },

"#,
        source_line,
        SourceLine {
            line_num: 11940,
            symbols: vec![
                    Symbol::Text(b"{"),
                    Symbol::Text(b"INT64_C"),
                    Symbol::Text(b"(0x0000810020000020), 0x1.02004000004"),
                    Symbol::Text(b"p"),
                    Symbol::Text(b"+47, 0x0p+0 },")
                ],
        });
    }

    #[test]
    fn parse_source_file() {
        let _ = env_logger::init();

        let parsed_file = SourceFile {
            filename: "src/compiler-rt/test/builtins/Unit/floatditf_test.c",
            lines: vec![
                    SourceLine {
                        line_num: 14,
                        symbols: vec![
                            Symbol::Text(b"\x02"),
                            Symbol::Symbol(Token::Include, b"\"\x9at_lib.h"),
                            Symbol::Text(b"\"")
                        ],
                    },
                    SourceLine {
                        line_num: 15,
                        symbols: vec![
                            Symbol::Text(b"\x02"),
                            Symbol::Symbol(Token::Include, b"<m\xa9h.h"),
                            Symbol::Text(b">")
                        ],
                    }
                ],
        };

        let mut visitor = SimpleVisitor::new(true);

        assert_eq!(walk_source_file::<bool, SimpleVisitor>(&parsed_file, &mut visitor).unwrap(),
            VisitResult::Continue);

        let lines = b"\t@src/compiler-rt/test/builtins/Unit/floatditf_test.c\n\
\n\
14 \n\
\t~\"\x9at_lib.h\n\
\"\n\
\n\
15 \n\
\t~<m\xa9h.h\n\
>\n\
\n";

        assert_parser!(lines, source_file, parsed_file);
    }
}
