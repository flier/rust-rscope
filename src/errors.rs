use std::io;
use std::error;
use std::result;

use cargo;

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

macro_rules! from_error {
    ($($p:ty,)*) => (
        $(impl From<$p> for Box<Error> {
            fn from(t: $p) -> Box<Error> { Box::new(t) }
        })*
    )
}

from_error! {
    io::Error,
    Box<cargo::CargoError>,
}

impl Error for io::Error {}
impl Error for Box<cargo::CargoError> {}

pub type Result<T> = result::Result<T, Box<Error>>;
