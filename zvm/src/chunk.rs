use std::{fmt::Display, rc::Rc};

use anyhow::bail;

use crate::val::Val;

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

#[derive(Debug, Clone)]
pub struct ChunkBuilder(Chunk);

impl ChunkBuilder {
    pub fn build(self) -> Chunk {
        self.0
    }

    pub fn build_fn(self, arity: u8, name: &str) -> FuncChunk {
        FuncChunk {
            arity,
            name: Rc::from(name),
            chunk: self.0,
        }
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
            if let Some(local_depth) = self.0.scope.resolve_local(&name) {
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
}

#[derive(Debug, Clone)]
pub struct Chunk {
    pub buf: Bytecode,
    pub constants: Vec<Val>,
    pub scope: Scope,
}

// impl IntoIterator for Chunk {
//     type Item = Opcode;
//
//     type IntoIter = ChunkIter;
//
//     fn into_iter(self) -> Self::IntoIter {
//         self.iter()
//     }
// }

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

    // pub fn iter(&self) -> ChunkIter {
    //     let begin = self.buf.slice().as_ptr();
    //     ChunkIter {
    //         begin,
    //         i: 0,
    //         len: self.buf.len(),
    //     }
    // }

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
    // pub fn push_binding(&mut self, name: ZIdent, var_op: VarOp) -> anyhow::Result<()> {
    //     match self.scope.depth() {
    //         BindScope::Global => {
    //             match var_op {
    //                 VarOp::Declare => {
    //
    //                 }
    //                 VarOp::Get => todo!(),
    //                 VarOp::Set => todo!(),
    //             }
    //         }
    //         BindScope::Local { depth } => todo!(),
    //     }
    //     // let (op, id) =
    //     let param = OpParam::pack(id as u64);
    //
    //     let op = Opcode {
    //         op,
    //         param: Some(param),
    //     };
    //     self.push_opcode(op);
    //     Ok(())
    // }
    pub fn debug_opcode(&self, opcode: Opcode) -> String {
        let s = match opcode.op() {
            Op::Return => "RETURN",
            Op::Println => "PRINTLN",
            Op::Print => "PRINT",
            Op::Pop => "POP",
            Op::PopN => {
                let n = opcode.try_param().unwrap().to_u32() as usize;

                &format!("POPN => {n},")
            }
            Op::Add => "ADD",
            Op::Sub => "SUB",
            Op::Div => "DIV",
            Op::Mul => "MUL",
            Op::Neg => "NEGATE",
            Op::Not => "NOT",
            Op::Nil => "NIL",
            Op::True => "TRUE",
            Op::False => "FALSE",
            Op::Concat => "CONCAT",
            Op::Const8 => {
                let index = opcode.try_param().unwrap().to_u32() as usize;
                &format!("CONST8 => {}, actual: {}", index, self.constants[index])
            }
            Op::Const16 => {
                let index = opcode.try_param().unwrap().to_u32() as usize;
                &format!("CONST16 => {}, actual: {}", index, self.constants[index])
            }
            Op::Const24 => {
                let index = opcode.try_param().unwrap().to_u32() as usize;
                &format!("CONST24 => {}, actual: {}", index, self.constants[index])
            }
            Op::Const32 => {
                let index = opcode.try_param().unwrap().to_u32() as usize;
                &format!("CONST32 => {}, actual: {}", index, self.constants[index])
            }
            Op::Const64 => {
                let index = opcode.try_param().unwrap().to_u32() as usize;
                &format!("CONST64 => {}, actual: {}", index, self.constants[index])
            }
            Op::Unknown => "UNKNOWN_OP",
            Op::DeclareGlobal8 => {
                let index = opcode.try_param().unwrap().to_u32() as usize;
                &format!("DEF_GLOBAL8 => {}", index)
            }
            Op::GetGlobal8 => {
                let index = opcode.try_param().unwrap().to_u32() as usize;
                &format!("GET_GLOBAL8 => {}", index)
            }
            Op::SetGlobal8 => {
                let index = opcode.try_param().unwrap().to_u32() as usize;
                &format!("SET_GLOBAL8 => {}", index)
            }

            Op::GetLocal8 => {
                let index = opcode.try_param().unwrap().to_u32() as usize;
                &format!("GET_LOCAL8 => {}", index)
            }
            Op::SetLocal8 => {
                let index = opcode.try_param().unwrap().to_u32() as usize;
                &format!("SET_LOCAL8 => {}", index)
            }
            Op::Eq => "EQ",
            Op::Gt => "GT",
            Op::Lt => "LT",
            Op::Ge => "GE",
            Op::Le => "LE",
            Op::NotEq => "NEQ",
            Op::DeclareGlobal16 => todo!(),
            Op::GetGlobal16 => todo!(),
            Op::SetGlobal16 => todo!(),
            Op::GetLocal16 => todo!(),
            Op::SetLocal16 => todo!(),
            Op::DeclareGlobal32 => todo!(),
            Op::GetGlobal32 => todo!(),
            Op::SetGlobal32 => todo!(),
            Op::GetLocal32 => todo!(),
            Op::SetLocal32 => todo!(),
            Op::JumpFalse => {
                let offset = opcode.try_param().unwrap().to_u32() as usize;
                &format!("JUMP_FALSE => {}", offset)
            }
            Op::LongJumpFalse => {
                let offset = opcode.try_param().unwrap().to_u32() as usize;
                &format!("LONG_JUMP_FALSE => {}", offset)
            }
            Op::Jump => {
                let offset = opcode.try_param().unwrap().to_u32() as usize;
                &format!("JUMP => {}", offset)
            }
            Op::LongJump => {
                let offset = opcode.try_param().unwrap().to_u32() as usize;
                &format!("LONG_JUMP => {}", offset)
            }

            Op::JumpTrue => {
                let offset = opcode.try_param().unwrap().to_u32() as usize;
                &format!("JUMP_TRUE => {}", offset)
            }

            Op::LongJumpTrue => {
                let offset = opcode.try_param().unwrap().to_u32() as usize;
                &format!("LONG_JUMP_TRUE => {}", offset)
            }
            Op::Call => todo!(),
        };

        String::from(s)
    }
}

impl Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();
        let mut i = 0;
        let mut n = 1;
        while i < self.buf.len() {
            if let Some(opcode) = self.opcode_at(i) {
                let op = self.debug_opcode(opcode);
                s.push_str(&format!("(i{i}n{n}): {op}\n"));
                i += opcode.offset();
                n += 1;
            } else {
                break;
            }
        }

        write!(f, "{s}")
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
