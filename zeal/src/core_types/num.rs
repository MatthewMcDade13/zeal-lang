use std::{
    hash::Hasher,
    ops::{Add, Deref, DerefMut, Div, Mul, Neg, Sub},
};

#[repr(transparent)]
#[derive(Debug, Default, Copy, Clone, PartialEq, PartialOrd)]
pub struct ZFloat64(f64);

impl ZFloat64 {
    pub const fn new(v: f64) -> Self {
        Self(v)
    }
}

impl Eq for ZFloat64 {}

impl Ord for ZFloat64 {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let s = self.0 as u64;
        let other = other.0 as u64;
        s.cmp(&other)
    }
}

impl std::hash::Hash for ZFloat64 {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let v = self.0 as u64;
        v.hash(state);
    }
}

impl From<f64> for ZFloat64 {
    fn from(value: f64) -> Self {
        Self::new(value)
    }
}

impl Add<f64> for ZFloat64 {
    type Output = ZFloat64;

    fn add(self, rhs: f64) -> Self::Output {
        Self::new(self.0 + rhs)
    }
}

impl Add for ZFloat64 {
    type Output = ZFloat64;

    fn add(self, rhs: Self) -> Self::Output {
        Self::new(self.0 + rhs.0)
    }
}

impl Sub<f64> for ZFloat64 {
    type Output = ZFloat64;

    fn sub(self, rhs: f64) -> Self::Output {
        Self::new(self.0 - rhs)
    }
}

impl Sub for ZFloat64 {
    type Output = ZFloat64;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::new(self.0 - rhs.0)
    }
}

impl Mul<f64> for ZFloat64 {
    type Output = ZFloat64;

    fn mul(self, rhs: f64) -> Self::Output {
        Self::new(self.0 * rhs)
    }
}

impl Mul for ZFloat64 {
    type Output = ZFloat64;

    fn mul(self, rhs: Self) -> Self::Output {
        Self::new(self.0 * rhs.0)
    }
}

impl Div<f64> for ZFloat64 {
    type Output = ZFloat64;

    fn div(self, rhs: f64) -> Self::Output {
        Self::new(self.0 / rhs)
    }
}

impl Div for ZFloat64 {
    type Output = ZFloat64;

    fn div(self, rhs: Self) -> Self::Output {
        Self::new(self.0 / rhs.0)
    }
}

impl Neg for ZFloat64 {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self(-self.0)
    }
}

impl ZFloat64 {
    pub const fn i32(self) -> i32 {
        self.0 as i32
    }

    pub const fn u32(self) -> u32 {
        self.0 as u32
    }

    pub const fn i64(self) -> i64 {
        self.0 as i64
    }

    pub const fn u64(self) -> u64 {
        self.0 as u64
    }

    pub const fn unwrap(self) -> f64 {
        self.0
    }
}

impl Deref for ZFloat64 {
    type Target = f64;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ZFloat64 {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[repr(transparent)]
#[derive(Debug, Default, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ZBool(pub(crate) bool);
impl From<bool> for ZBool {
    fn from(value: bool) -> Self {
        Self(value)
    }
}

impl Deref for ZBool {
    type Target = bool;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ZBool {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl ZBool {
    pub const fn new(v: bool) -> Self {
        Self(v)
    }
}
