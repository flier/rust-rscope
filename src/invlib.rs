use errors::Result;

#[derive(PartialEq, Eq, PartialOrd, Debug)]
pub struct InvertedIndex {
    pub header: Header,
}

#[derive(PartialEq, Eq, PartialOrd, Debug, Clone, Copy)]
pub struct Header {
    /// inverted index format version
    pub version: u64,
    /// file status word
    pub filestat: u64,
    /// size of logical block in bytes
    pub sizeblk: u64,
    /// first byte of superfinger
    pub startbyte: u64,
    /// size of superfinger in bytes
    pub supsize: u64,
    /// size of max cntl space (should be a multiple of BUFSIZ)
    pub cntlsize: u64,
    /// flag whether to use shared memory
    pub share: u64,
}

pub fn parse(buf: &[u8]) -> Result<InvertedIndex> {
    let h = unsafe { &*(buf.as_ptr() as *const Header) };

    info!("Inverted index loaded, version {}", h.version);

    let blk = unsafe { buf.as_ptr().offset(h.startbyte as isize) as *const u64 };

    Ok(InvertedIndex { header: *h })
}
