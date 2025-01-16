use std::rc::Rc;

use anyhow::ensure;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum StrBuf {
    Short(ShortBuf),
    Long(RcString),
}

pub const SHORTBUF_LEN: usize = 22;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ShortBuf {
    buf: [u8; SHORTBUF_LEN],
    len: u8,
}

impl ShortBuf {
    pub const fn empty() -> Self {
        Self {
            buf: [0u8; SHORTBUF_LEN],
            len: 0,
        }
    }

    pub fn new(string: &str) -> anyhow::Result<Self> {
        ensure!(string.len() <= SHORTBUF_LEN, "given string: {string} is too large to fit into ShortBuf. Max length for Shortbuf is {SHORTBUF_LEN}");
        let s = Self::truncate(string);
        Ok(s)
    }

    pub const fn len(&self) -> u8 {
        self.len
    }

    /// Creates a ShortBuf from give string. If string is longer than
    /// 22 characters, only those first 22 characters are memcpy'ed into new ShortBuf and
    /// rest of string is truncated.
    pub fn truncate(string: &str) -> Self {
        let mut buf = [0u8; SHORTBUF_LEN];
        let len = std::cmp::min(string.len(), SHORTBUF_LEN);
        let src = &string.as_bytes()[..len];
        let dst = &mut buf[..len];
        dst.copy_from_slice(src);
        let len = len as u8;
        Self { buf, len }
    }

    pub const fn as_str(&self) -> &str {
        // SAFETY :: We got this byte slice from a valid utf8 string,
        // so we can know this is safe.
        unsafe { std::str::from_utf8_unchecked(&self.buf) }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RcString(Rc<str>);

impl RcString {
    pub fn new(string: &str) -> Self {
        Self(Rc::from(string))
    }

    pub fn as_str(&self) -> &str {
        self.0.as_ref()
    }
}

impl StrBuf {
    pub const fn empty() -> Self {
        Self::Short(ShortBuf::empty())
    }

    pub const fn is_empty(&self) -> bool {
        match self {
            StrBuf::Short(short_buf) => short_buf.len() == 0,
            _ => false,
        }
    }

    pub fn new(string: &str) -> Self {
        if string.len() <= SHORTBUF_LEN {
            Self::Short(ShortBuf::truncate(string))
        } else {
            Self::Long(RcString::new(string))
        }
    }

    pub fn into_rc(self) -> Rc<str> {
        match self {
            StrBuf::Short(short_buf) => {
                let s = short_buf.as_str();
                Rc::from(s)
            }
            StrBuf::Long(rc_string) => rc_string.0,
        }
    }

    pub fn into_string(self) -> String {
        match self {
            StrBuf::Short(short_buf) => String::from_utf8(short_buf.buf.to_vec())
                .expect("Failed to convert byte slice to valid utf8 string!!!"),
            StrBuf::Long(rc_string) => String::from(rc_string.as_str()),
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            StrBuf::Short(short_buf) => short_buf.as_str(),
            StrBuf::Long(rc_string) => rc_string.as_str(),
        }
    }
}
