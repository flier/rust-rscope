use std::fmt;

pub struct Digraph {
    /// 16 most frequent first chars
    dichar1: &'static [u8],
    /// 8 most frequent second chars
    dichar2: &'static [u8],
    /// digraph first character code
    dicode1: [u8; 256],
    /// digraph second character code
    dicode2: [u8; 256],
}

pub fn new() -> Digraph {
    let mut g = Digraph {
        dichar1: b" teisaprnl(of)=c",
        dichar2: b" tnerpla",
        dicode1: [0; 256],
        dicode2: [0; 256],
    };

    for i in 0..16 {
        g.dicode1[g.dichar1[i] as usize] = (i * 8 + 1) as u8;
    }

    for i in 0..8 {
        g.dicode2[g.dichar2[i] as usize] = (i + 1) as u8;
    }

    g
}

impl fmt::Debug for Digraph {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
               "Digraph {{ dichar1: {:?}, dichar2: {:?}, dicode1: {:?}, dicode2: {:?} }}",
               self.dichar1,
               self.dichar2,
               &self.dicode1[..],
               &self.dicode2[..])
    }
}

impl Digraph {
    /// Check if a given pair of chars is compressable as a dicode
    pub fn is_dicode(&self, b1: u8, b2: u8) -> bool {
        self.dicode1[b1 as usize] != 0 && self.dicode2[b2 as usize] != 0
    }

    /// Combine the pair into a dicode
    pub fn to_dicode(&self, b1: u8, b2: u8) -> Option<u8> {
        if self.is_dicode(b1, b2) {
            let c = 0x80 - 2 + self.dicode1[b1 as usize] + self.dicode2[b2 as usize];

            debug!("encode ({} -> {}, {} -> {}) to {}",
                   b1,
                   self.dicode1[b1 as usize],
                   b2,
                   self.dicode2[b2 as usize],
                   c);

            Some(c)
        } else {
            None
        }
    }

    pub fn compress(&self, buf: &[u8]) -> Vec<u8> {
        let mut dst = Vec::new();
        let mut i = 0;

        while i < buf.len() {
            if let Some(c) = self.to_dicode(buf[i], buf[i + 1]) {
                dst.push(c);
                i += 2;
            } else {
                dst.push(buf[i]);
                i += 1;
            }
        }

        dst
    }

    pub fn decompress(&self, buf: &[u8]) -> Vec<u8> {
        let mut dst = Vec::new();

        for b in buf {
            if *b >= 0x80 && *b != b'\n' {
                let c = *b & 0x7f;

                dst.push(self.dichar1[(c / 8) as usize]);
                dst.push(self.dichar2[(c & 7) as usize]);
            } else {
                dst.push(*b)
            }
        }

        dst
    }
}

#[cfg(test)]
mod tests {
    extern crate env_logger;

    use super::*;

    #[test]
    fn digraph() {
        let _ = env_logger::init();

        let d = new();

        debug!("{:?}", d);

        assert_eq!(d.compress(b"test"), b"\x8b\xa1");
        assert_eq!(d.decompress(b"\x8b\xa1"), b"test");
    }
}
