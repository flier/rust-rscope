use std::io;
use std::fmt;
use std::error;
use std::result;

use cargo;
use syntex_syntax::errors::DiagnosticBuilder;

pub trait Error: error::Error + Send + 'static {}

impl error::Error for Box<Error> {
    fn description(&self) -> &str {
        (**self).description()
    }
    fn cause(&self) -> Option<&error::Error> {
        (**self).cause()
    }
}

impl Error for Box<Error> {}

#[macro_export]
macro_rules! from_error {
    ($($p:ty,)*) => (
        $(impl From<$p> for Box<$crate::errors::Error> {
            fn from(t: $p) -> Box<$crate::errors::Error> { Box::new(t) }
        })*
    )
}

from_error! {
    io::Error,
    Box<cargo::CargoError>,
}

impl Error for io::Error {}
impl Error for Box<cargo::CargoError> {}

pub struct SyntaxError {
    pub desc: String,
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.desc, f)
    }
}

impl fmt::Debug for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl error::Error for SyntaxError {
    fn description(&self) -> &str {
        &self.desc
    }
}

impl Error for SyntaxError {}

impl SyntaxError {
    fn new<'a>(d: DiagnosticBuilder<'a>) -> Self {
        SyntaxError { desc: d.message().to_string() }
    }
}

impl<'a> From<DiagnosticBuilder<'a>> for Box<Error> {
    fn from(d: DiagnosticBuilder<'a>) -> Box<Error> {
        Box::new(SyntaxError::new(d))
    }
}

pub type Result<T> = result::Result<T, Box<Error>>;
