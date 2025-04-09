use alloc::boxed::Box;

const DEFAULT_VM_INIT_SIZE: usize = 64;
const INT_REGISTER_COUNT: usize = 4;
const FLOAT_REGISTER_COUNT: usize = 4;
const ADDRESS_REGISTER_COUNT: usize = 4;
const ANY_REGISTER_SIZE: usize = 64;

#[derive(Debug)]
pub struct ZealVM {
    rxi: [Regii; INT_REGISTER_COUNT],
    rxf: [Regif; FLOAT_REGISTER_COUNT],
    rxa: [Regiaddr; ADDRESS_REGISTER_COUNT],
    rxt: [u8; ANY_REGISTER_SIZE],
    bytecode: Box<OpMemory>,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct CallFrame {}

/// Read-only memory for bytecode  instructions
#[derive(Debug)]
pub struct OpMemory([u8]);

#[derive(Debug, Clone, Copy, Default)]
#[repr(transparent)]
pub struct Regii(i64);

#[derive(Debug, Clone, Copy, Default)]
#[repr(transparent)]
pub struct Regif(f64);

#[derive(Debug, Clone, Copy, Default)]
#[repr(transparent)]
pub struct Regiaddr(isize);
