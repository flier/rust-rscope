use std::cmp;
use std::path::PathBuf;

use syntex_syntax::parse;

use errors::Result;

#[derive(PartialEq, Eq, PartialOrd)]
#[repr(u8)]
pub enum Symbol {
    /// non-symbol text
    Nop,
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
    GlobalDecl = b'm',
    /// function parameter definition
    FuncParam = b'p',
    /// struct definition
    StructDef = b's',
    /// typedef definition
    Typedef = b't',
    /// union definition
    UnionDef = b'u',
}

#[derive(PartialEq, Eq, PartialOrd)]
pub struct SourceLine {
    pub line_num: usize,
    pub symbols: Vec<Symbol>,
}

impl cmp::Ord for SourceLine {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.line_num.cmp(&other.line_num)
    }
}

#[derive(PartialEq, Eq, PartialOrd)]
pub struct SourceFile {
    pub path: PathBuf,
    pub lines: Vec<SourceLine>,
}

impl cmp::Ord for SourceFile {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.path.cmp(&other.path)
    }
}

pub fn extract_symbols(src_path: &PathBuf) -> Result<Vec<SourceFile>> {
    let cfg = Vec::new();
    let sess = parse::ParseSess::new();

    let mut krate = try!(parse::parse_crate_from_file(&src_path, cfg, &sess));

    let mut source_files = Vec::new();

    Ok(source_files)
}
