use std::fmt::Display;

use anyhow::bail;

use crate::{
    compiler::opcode::OpParam,
    core_types::{
        str::{ZIdent, ZString},
        val::ZValue,
    },
    err::core::CompileError,
};

use super::{
    opcode::{Bytecode, Op, OpParamSize, Opcode, VarOp},
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
pub struct Chunk {
    pub buf: Bytecode,
    pub constants: Vec<ZValue>,
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
    pub const fn zeroed() -> Self {
        Self {
            buf: Bytecode::zeroed(),
            constants: Vec::new(),
            scope: Scope::new(),
        }
    }

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

    pub fn append_chunk(&mut self, other: &Chunk) {
        self.buf.append_bytes(other.buf.slice());
        self.constants.extend_from_slice(&other.constants);
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
    pub fn constants(&self) -> &[ZValue] {
        self.constants.as_ref()
    }

    pub fn try_read_const(&self, opcode: Opcode) -> Option<&ZValue> {
        match opcode.op() {
            Op::Const8 | Op::Const16 | Op::Const24 | Op::Const32 | Op::Const64 => {
                let param = opcode.try_param().unwrap();
                let id = param.to_usize();
                Some(&self.constants[id])
            }
            _ => None,
        }
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
        self.push_constant(ZValue::string(ZString::from(string)))
    }

    #[inline]
    pub fn push_ident(&mut self, ident: ZIdent) -> usize {
        self.push_constant(ZValue::ident(ident))
    }

    #[inline]
    pub fn push_ident_str(&mut self, string: &str) -> usize {
        self.push_constant(ZValue::ident_from_str(string))
    }

    pub fn declare_local(&mut self, name: ZIdent) -> anyhow::Result<()> {
        if self.scope.depth().is_local() {
            self.scope.add_local(name);
        } else {
            bail!("Cant compile local binding as global!");
        }
        Ok(())
    }

    pub fn declare_global(&mut self, name: ZIdent) -> anyhow::Result<()> {
        let id = self.add_global(name);
        let param = OpParam::squash(id as u64);
        let op = Op::global(OpParamSize::from_param(&param), VarOp::Declare);

        let op = Opcode::WithParam { op, param };
        self.push_opcode(op);
        Ok(())
    }

    pub fn push_global(&mut self, name: ZIdent, varop: VarOp) {
        let id = self.add_global(name);
        let param = OpParam::squash(id as u64);
        let op = Op::global(OpParamSize::from_param(&param), varop);
        let opcode = Opcode::WithParam { op, param };
        self.push_opcode(opcode);
    }

    pub fn push_local(&mut self, name: ZIdent, varop: VarOp) -> anyhow::Result<()> {
        let (op, id) = if let VarOp::Get | VarOp::Set = varop {
            if let Some(local_offset) = self.scope.resolve_local(&name) {
                let size = OpParamSize::squash_size(local_offset as u64);
                (Op::local(size, varop), local_offset)
            } else {
                self.push_global(name, varop);
                return Ok(());
            }
        } else {
            bail!("{}", CompileError::InvalidAssignment { op_ty: varop, name })
        };
        let param = OpParam::squash(id as u64);
        let opcode = Opcode::WithParam { op, param };
        self.push_opcode(opcode);
        Ok(())
    }

    pub fn declare_binding(&mut self, name: ZIdent) -> anyhow::Result<()> {
        if self.scope.depth().is_local() {
            self.declare_local(name)
            // Ok(())
        } else {
            self.declare_global(name)
        }
    }

    pub fn push_binding(&mut self, name: ZIdent, varop: VarOp) -> anyhow::Result<()> {
        if self.scope.depth().is_local() {
            // self.declare_local(name)
            // Ok(())

            self.push_local(name.clone(), varop)
        } else {
            self.push_global(name.clone(), varop);
            Ok(())
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
    /// adds v to constant table, returns id of constant
    pub fn push_constant(&mut self, v: ZValue) -> usize {
        let id = self.add_constant(v);
        let param = OpParam::squash(id as u64);
        let op = param.as_const_op();

        let op = Opcode::WithParam { op, param };
        self.push_opcode(op);
        param.to_u32() as usize
    }

    pub fn push_number(&mut self, n: f64) -> usize {
        self.push_constant(ZValue::number(n))
    }

    /// Pushes given opcode into bytecode buffer. returns index to the beginning of the opcode,
    /// i.e. the opcode op byte
    pub fn push_opcode<T>(&mut self, opcode: T) -> usize
    where
        T: Into<Opcode>,
    {
        let opcode = opcode.into();
        let addr = self.buf.len();
        self.buf.push(opcode.op_byte());
        if let Some(p) = opcode.try_param() {
            match *p {
                OpParam::Byte(b) => {
                    self.buf.push(b);
                }
                OpParam::Byte16(src) => {
                    self.buf.extend_from_slice(&src);
                }
                OpParam::Byte24(src) => {
                    self.buf.extend_from_slice(&src);
                }
                OpParam::Byte32(src) => {
                    self.buf.extend_from_slice(&src);
                }
                OpParam::Byte64(src) => {
                    self.buf.extend_from_slice(&src);
                }
            }
        }
        addr
    }

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
                let val = &self.constants[index];
                if let ZValue::Func(f) = val {
                    let name = f.name.clone();
                    let arity = f.arity;
                    let arity_s = if arity == 0 {
                        String::new()
                    } else {
                        arity.to_string()
                    };
                    let code = &f.chunk;
                    &format!("fn {name}/{arity_s}:\n\t{code}")
                } else {
                    &format!("CONST8 => {}, actual: {}", index, self.constants[index])
                }
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
            Op::Call => {
                let nargs = opcode.try_param().unwrap().to_u32() as usize;
                &format!("CALL => {}", nargs)
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
                let local = &self.scope.locals[index];
                &format!("GET_LOCAL8 => {index}, ident: {local}")
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
            Op::NoOp => "<<<NO_OP>>>",
        };

        String::from(s)
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
        let jump = self.buf.len();
        patch.update(jump);
        if let Opcode::WithParam { param, .. } = patch.opcode {
            self.buf.write_param_at(patch.addr, param);
        } else {
            panic!("Tried to patch an opcode that does not have any parameters!!!");
        }
    }

    #[inline]
    fn add_global(&mut self, ident: ZIdent) -> usize {
        self.add_constant(ZValue::Ident(ident))
    }

    /// Pushes val to constant array and returns the index it was pushed to.
    fn add_constant(&mut self, val: ZValue) -> usize {
        let id = self.constants.len();
        self.constants.push(val);
        id
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
