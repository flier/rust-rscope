use std::io;
use std::fmt;
use std::str;
use std::string;

use cargo;
use syntex_syntax::errors::DiagnosticBuilder;
use nom;
use rustbox;
use rustyline;

error_chain! {
    types {
        Error, ErrorKind, ChainErr, Result;
    }

    links {
    }

    foreign_links {
        io::Error, IoError, "io error";
        str::Utf8Error, Utf8Error, "convert UTF8 slice error";
        string::FromUtf8Error, FromUtf8Error, "convert from UTF8 error";
        Box<cargo::CargoError>, CargoError, "cargo error";
        rustbox::InitError, TermInitError, "term init error";
        rustbox::EventError, TermEventError, "term event error";
        rustyline::error::ReadlineError, ReadlineError, "readline error";
    }

    errors {
        SyntaxError(desc: String) {
            description("syntax error")
            display("syntax error, {}", desc)
        }
        ParseError(err: String) {
            description("parse error")
            display("parse error, {}", err)
        }
        NoMoreData(needed: nom::Needed) {
            description("need more data")
            display("need more data, {:?}", needed)
        }
        OtherError(desc: String) {
            description("error")
            display("error, {}", desc)
        }
    }
}

impl<'a> From<DiagnosticBuilder<'a>> for Error {
    fn from(d: DiagnosticBuilder<'a>) -> Self {
        ErrorKind::SyntaxError(d.message().to_string()).into()
    }
}

impl<I, E> From<nom::Err<I, E>> for Error
    where I: fmt::Debug,
          E: fmt::Debug
{
    fn from(err: nom::Err<I, E>) -> Self {
        ErrorKind::ParseError(format!("{:?}", err)).into()
    }
}
