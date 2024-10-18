use std::{fmt::Display, rc::Rc};

use anyhow::bail;

use crate::{env::CompileEnv, val::Val};

use super::{
    opcode::{Bytecode, Op, OpParam, OpParamSize, Opcode, VarOp},
    state::Scope,
};

// #[derive(Debug, Clone, Copy)]
// pub struct ChunkIter {
//     begin: *const u8,
//     i: usize,
//     len: usize,
// }
//
// impl Iterator for ChunkIter {
//     type Item = Opcode;
//
//     fn next(&mut self) -> Option<Self::Item> {
//         if self.len == 0 || self.i >= self.len {
//             None
//         } else {
//             let opcode = unsafe { read_raw_slice_as_bytecode(self.begin, self.i, self.len) }
//                 .expect(&format!("cant get opcode from chunk! {:?}", self));
//             let offset = opcode.offset();
//             self.i += offset;
//             Some(opcode)
//         }
//     }
// }

#[derive(Debug, Clone)]
pub struct FuncChunk {
    pub arity: u8,
    pub chunk: Chunk,
    name: Rc<str>,
}

impl FuncChunk {
    pub fn name(&self) -> &str {
        self.name.as_ref()
    }
}

// impl Display for FuncChunk {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let name = self.name();
//         let chunk = &self.chunk;
//         let arity = self.arity as usize;
//         let s = format!("{name}/{arity} =>\n\t{chunk}");
//         write!(f, "{s}")
//     }
// }
//
#[derive(Debug, Clone)]
pub struct ChunkBuilder(pub Chunk);

impl ChunkBuilder {
    pub const fn from_existing(ch: Chunk) -> Self {
        Self(ch)
    }
    pub fn build(self) -> Chunk {
        self.0
    }

    pub fn start_scope(&mut self) {
        self.0.scope.start_scope();
    }

    pub fn end_scope(&mut self) -> usize {
        self.0.scope.end_scope()
    }

    pub fn build_func_in(self, ch: &mut Chunk, name: &str, arity: u8) {
        let f = self.build_func(name, arity);
        ch.append_func(f);
    }

    pub fn build_func(self, name: &str, arity: u8) -> FuncChunk {
        let fc = FuncChunk {
            arity,
            name: Rc::from(name),
            chunk: self.0,
        };
        fc
    }

    pub fn push_call(&mut self, nargs: u8) {
        let param = OpParam::Byte(nargs);
        let op = Opcode::WithParam {
            op: Op::Call,
            param,
        };
        self.push_opcode(op);
    }

    pub fn push_constant(&mut self, v: Val) -> usize {
        let id = self.add_constant(v);
        let param = OpParam::squash(id as u64);
        let op = param.as_const_op();

        let op = Opcode::WithParam { op, param };
        self.push_opcode(op);
        param.to_u32() as usize
    }

    pub fn push_float(&mut self, n: f64) -> usize {
        self.push_constant(Val::float(n))
    }

    pub fn append_chunk(&mut self, other: &Chunk) {
        self.0.buf.append_bytes(other.buf.slice());
        self.0.constants.extend_from_slice(&other.constants);
    }

    pub fn push_popn(&mut self, n: u8) {
        let opcode = if n == 1 {
            Opcode::new(Op::Pop)
        } else {
            Opcode::with_param(Op::PopN, OpParam::squash(n as u64))
        };
        self.push_opcode(opcode);
    }

    #[inline]
    pub fn push_string(&mut self, string: &str) -> usize {
        self.push_constant(Val::string(string))
    }

    #[inline]
    pub fn push_rune(&mut self, ident: &str) -> usize {
        self.push_constant(Val::rune(ident))
    }

    pub fn declare_local(&mut self, name: &str) -> anyhow::Result<()> {
        if self.0.scope.depth().is_local() {
            self.0.scope.add_local(name);
        } else {
            bail!("Cant compile local binding as global!");
        }
        Ok(())
    }

    pub fn declare_global(&mut self, name: &str) -> anyhow::Result<()> {
        let id = self.add_global(name);
        let param = OpParam::squash(id as u64);
        let op = Op::global(OpParamSize::from_param(&param), VarOp::Declare);

        let op = Opcode::WithParam { op, param };
        self.push_opcode(op);
        Ok(())
    }

    pub fn push_global(&mut self, name: &str, varop: VarOp) {
        let id = self.add_global(name);
        let param = OpParam::squash(id as u64);
        let op = Op::global(OpParamSize::from_param(&param), varop);
        let opcode = Opcode::WithParam { op, param };
        self.push_opcode(opcode);
    }

    pub fn push_local(&mut self, name: &str, varop: VarOp) -> anyhow::Result<()> {
        let (op, id) = if let VarOp::Get | VarOp::Set = varop {
            if let Some(local_depth) = self.0.scope.resolve_local(name) {
                let size = OpParamSize::squash_size(local_depth as u64);
                (Op::local(size, varop), local_depth)
            } else {
                self.push_global(name, varop);
                return Ok(());
            }
        } else {
            bail!(
                "Compile Error :: {} {}",
                /* CompileError::InvalidAssignment */
                varop,
                name
            )
        };
        let param = OpParam::squash(id as u64);
        let opcode = Opcode::WithParam { op, param };
        self.push_opcode(opcode);
        Ok(())
    }

    pub fn declare_binding(&mut self, name: &str) -> anyhow::Result<()> {
        if self.0.scope.depth().is_local() {
            self.declare_local(name)
            // Ok(())
        } else {
            self.declare_global(name)
        }
    }

    pub fn push_binding(&mut self, name: &str, varop: VarOp) -> anyhow::Result<()> {
        if self.0.scope.depth().is_local() {
            // self.declare_local(name)
            // Ok(())

            self.push_local(name.clone(), varop)
        } else {
            self.push_global(name.clone(), varop);
            Ok(())
        }
    }
    /// adds v to constant table, returns id of constant
    /// Pushes given opcode into bytecode buffer. returns index to the beginning of the opcode,
    /// i.e. the opcode op byte
    pub fn push_opcode<T>(&mut self, opcode: T) -> usize
    where
        T: Into<Opcode>,
    {
        let opcode = opcode.into();
        let addr = self.0.buf.len();
        self.0.buf.push(opcode.op_byte());
        if let Some(p) = opcode.try_param() {
            match *p {
                OpParam::Byte(b) => {
                    self.0.buf.push(b);
                }
                OpParam::Byte16(src) => {
                    self.0.buf.extend_from_slice(&src);
                }
                OpParam::Byte24(src) => {
                    self.0.buf.extend_from_slice(&src);
                }
                OpParam::Byte32(src) => {
                    self.0.buf.extend_from_slice(&src);
                }
                OpParam::Byte64(src) => {
                    self.0.buf.extend_from_slice(&src);
                }
            }
        }
        addr
    }

    /// Pushes Jump Op into Bytecode. Returns index of the pushed opcode
    /// so that it can be used downstream to pass to Self::patch_jump()
    pub fn push_jump(&mut self, op: Op) -> Patch {
        let param = match op {
            Op::JumpTrue | Op::JumpFalse | Op::Jump => OpParam::Byte16([0; 2]),
            Op::LongJumpTrue | Op::LongJumpFalse | Op::LongJump => OpParam::Byte32([0; 4]),
            _ => panic!("Cant push op byte that isnt a valid jump instruction!!!. Got: {op}"),
        };

        let opcode = Opcode::WithParam { op, param };
        let patch_addr = self.push_opcode(opcode);
        Patch {
            addr: patch_addr,
            opcode,
        }
    }

    pub fn patch_jump(&mut self, mut patch: Patch) {
        let jump = self.0.buf.len();
        patch.update(jump);
        if let Opcode::WithParam { param, .. } = patch.opcode {
            self.0.buf.write_param_at(patch.addr, param);
        } else {
            panic!("Tried to patch an opcode that does not have any parameters!!!");
        }
    }

    #[inline]
    fn add_global(&mut self, ident: &str) -> usize {
        self.add_constant(Val::rune(ident))
    }

    /// Pushes val to constant array and returns the index it was pushed to.
    fn add_constant(&mut self, val: Val) -> usize {
        let id = self.0.constants.len();
        self.0.constants.push(val);
        id
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}

impl Default for ChunkBuilder {
    fn default() -> Self {
        Self(Default::default())
    }
}

#[derive(Debug, Clone)]
pub struct Chunk {
    pub buf: Bytecode,
    pub constants: Vec<Val>,
    pub scope: Scope,
}

impl Default for Chunk {
    fn default() -> Self {
        Self::with_capacity(Self::DEFAULT_CAPACITY, Self::DEFAULT_CAPACITY)
    }
}

impl Chunk {
    pub const DEFAULT_CAPACITY: usize = 255;
    pub fn with_capacity(buf_cap: usize, const_cap: usize) -> Self {
        Self {
            buf: Bytecode::from(Vec::with_capacity(buf_cap)),
            constants: Vec::with_capacity(const_cap),
            scope: Scope::with_capacity(Self::DEFAULT_CAPACITY),
        }
    }
    pub fn append_func(&mut self, fc: FuncChunk) {
        let _ = ch_push_constant(self, Val::Func(Rc::new(fc)));
    }

    #[inline]
    pub fn opcode_at(&self, index: usize) -> Option<Opcode> {
        self.buf.opcode_at(index)
    }

    #[inline]
    pub fn expect_opcode_at(&self, index: usize) -> Opcode {
        self.buf.expect_opcode_at(index)
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.buf.len()
    }

    #[inline]
    pub fn const_len(&self) -> usize {
        self.constants.len()
    }

    /// Gets bygtecode as a raw &[u8]
    #[inline]
    pub fn bytes(&self) -> &[u8] {
        self.buf.slice()
    }

    pub const fn code(&self) -> &Bytecode {
        &self.buf
    }

    #[inline]
    pub fn constants(&self) -> &[Val] {
        self.constants.as_ref()
    }

    pub fn try_read_const(&self, opcode: Opcode) -> Option<&Val> {
        match opcode.op() {
            Op::Const8 | Op::Const16 | Op::Const24 | Op::Const32 | Op::Const64 => {
                let param = opcode.try_param().unwrap();
                let id = param.to_usize();
                Some(&self.constants[id])
            }
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Patch {
    pub addr: usize,
    pub opcode: Opcode,
}

impl Patch {
    pub fn update(&mut self, addr: usize) {
        match &mut self.opcode {
            Opcode::Byte(_) => {
                panic!("Tried to patch an opcode that does not have any parameters!!!")
            }
            Opcode::WithParam { param, .. } => param.write(addr as u64),
        }
    }
}

fn ch_push_constant(ch: &mut Chunk, v: Val) -> usize {
    let id = ch_add_constant(ch, v);
    let param = OpParam::squash(id as u64);
    let op = param.as_const_op();

    let op = Opcode::WithParam { op, param };
    ch_push_opcode(ch, op);
    param.to_u32() as usize
}

fn ch_add_constant(ch: &mut Chunk, v: Val) -> usize {
    let id = ch.constants.len();
    ch.constants.push(v);
    id
}

fn ch_push_opcode<T>(ch: &mut Chunk, opcode: T) -> usize
where
    T: Into<Opcode>,
{
    let opcode = opcode.into();
    let addr = ch.buf.len();
    ch.buf.push(opcode.op_byte());
    if let Some(p) = opcode.try_param() {
        match *p {
            OpParam::Byte(b) => {
                ch.buf.push(b);
            }
            OpParam::Byte16(src) => {
                ch.buf.extend_from_slice(&src);
            }
            OpParam::Byte24(src) => {
                ch.buf.extend_from_slice(&src);
            }
            OpParam::Byte32(src) => {
                ch.buf.extend_from_slice(&src);
            }
            OpParam::Byte64(src) => {
                ch.buf.extend_from_slice(&src);
            }
        }
    }
    addr
}
