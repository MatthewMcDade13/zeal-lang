use core::{
    hash::Hash,
    marker::PhantomData,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

use zerocopy_derive::{FromBytes, Immutable, IntoBytes, KnownLayout};

use crate::{Byteable, util_lite::copy_slice_into};
