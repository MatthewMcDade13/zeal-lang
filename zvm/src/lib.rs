pub mod chunk;
pub mod compiler;
pub mod err;
pub mod opcode;
pub mod stack;
pub mod state;

use std::{collections::HashMap, fmt::Display, rc::Rc, str::FromStr};

use stack::Stack;

macro_rules! num_binary_op {
    ($stack: expr, $op: tt) => {

        {
        // TODO: Inequalities dont work... not sure why but left and right seems to
        // get switch up somewhere. OOF
            let right = $stack.expect_pop().expect_float64();
            let left = $stack.expect_pop().expect_float64();
            let res = left $op right;
            let op = stringify!($op);

            let res = ZValue::from(res);
            $stack.push(res);



        }

    };
}

pub const STACK_MAX: usize = u16::MAX as usize;
pub const CALL_STACK_MAX: usize = STACK_MAX * 8;
#[derive(Debug, Clone)]
pub struct VM {
    stack: VMStack,
    frames: Stack<CALL_STACK_MAX, CallFrame>,
    // chunks: Vec<Chunk>,
    // chunk: Chunk,
    /// program counter for current running chunk. (chunk counter)
    // pc: usize,
    globals: HashMap<zeal_core::str::ZIdent, crate::val::ZValue>, // runes: RuneTable,
}

pub type VMStack = Stack<STACK_MAX, ZValue>;

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Stack::new(),
            frames: Stack::new(),
            globals: HashMap::new(),
            // runes: RuneTable::empty(),
        }
    }

    pub fn dump_stack(&self) -> String {
        self.stack.to_string()
    }

    pub fn dump_chunk(&self) -> String {
        self.try_frame()
            .expect("Cannot dump when no frames in CallStack!!!")
            .to_string()
        // self.chunk_top().unwrap().to_string()
    }

    pub fn dump(&self) -> String {
        let mut dump = String::from("\n ----- DUMP -----\n");
        let stack = self.stack.to_string();
        let bcode = self.dump_chunk();
        dump.push_str(&stack);
        dump.push_str(&bcode);
        dump.push_str(" ----- DUMP END ------\n");

        dump
    }

    // #[inline]
    /// Compiles string of zeal source code into a single chunk and returns the depth (index) of the newly pushed chunk.
    // pub fn compile_source(&mut self, src: &str) -> anyhow::Result<FuncChunk> {
    //     let ast = Ast::from_str(src)?;
    //     self.compile_ast(&ast)
    // }

    /// Compiles ast into a single chunk and returns the depth (index) of the newly pushed chunk.
    // pub fn compile_ast(&mut self, ast: &Ast) -> anyhow::Result<FuncChunk> {
    //     Archon::compile(&ast)
    // }

    // TODO: pub fn test_opcode(&self, opcode: Opcode) -> anyhow::Result<OpEffect> {}
    // ------------------------------------------------------------------------------------
    // something fn like above that doesnt mutate the VM, but reocrds what the
    // effects of running given opcode WOULD have on the VM. Can be used to test if an opcode is
    // okay/safe to run, or used for debugging ,ect
    //
    //
    //
    // pub fn exec_opcode(&mut self, opcode: Opcode) -> anyhow::Result<()> {}

    #[inline]
    pub fn try_frame(&self) -> Option<&CallFrame> {
        self.frames.peek_top()
    }

    #[inline]
    /// Get current Frame being executed.
    pub fn try_frame_mut(&mut self) -> Option<&mut CallFrame> {
        self.frames.peekn_mut(0)
    }

    #[inline]
    fn frame(&self) -> &CallFrame {
        self.try_frame()
            .expect("Tried to get executing callframe but Call Stack is empty!!!")
    }

    #[inline]
    fn frame_mut(&mut self) -> &mut CallFrame {
        self.try_frame_mut()
            .expect("Tried to get executing callframe but Call Stack is empty!!!")
    }

    /// Pushes ZBalue::Func onto stack memory, also crates a new CallFrame
    /// and pushes that onto Call Stack.
    // pub fn call(&mut self, unit: Rc<FuncChunk>) {
    //     assert!(
    //         matches!(unit.as_ref(), FuncChunk::Fn { .. }),
    //         "Only functions can be called! Got: {:?}",
    //         unit.as_ref()
    //     );
    //     let stack_fn = Rc::clone(&unit);
    //     self.stack.push(ZValue::Func(stack_fn));
    //
    //     let cf = self.new_frame(unit);
    //
    //     self.frames.push(cf);
    // }
    //
    fn next_opcode(&mut self) -> Option<Opcode> {
        let frame = self.try_frame_mut()?;

        frame.next_opcode()
    }

    #[inline]
    pub fn frame_constants(&self) -> &[ZValue] {
        let f = self.frame();
        f.constants()
    }

    //
    // pub fn exec(&mut self) -> anyhow::Result<ZValue> {
    //     while let Some(opcode) = self.next_opcode() {
    //         match opcode.op() {
    //             Op::Return => {
    //                 let retval = self.stack.expect_pop();
    //                 if let Some(frame) = self.frames.pop() {
    //                     self.stack.cursor.set(frame.start_slot);
    //                     self.stack.push(retval);
    //                 } else {
    //                     // let _ = self.stack.pop();
    //                     break;
    //                 }
    //             }
    //             Op::Println => {
    //                 let v = self.stack.peek_top().expect("cannot peek an empty stack!");
    //                 println!("{v}");
    //                 self.stack.expect_pop();
    //             }
    //
    //             Op::Print => {
    //                 let v = self.stack.peek_top().expect("cannot peek an empty stack!");
    //                 print!("{v}");
    //                 self.stack.expect_pop();
    //             }
    //
    //             Op::Pop => {
    //                 let _ = self.stack.expect_pop();
    //             }
    //             Op::PopN => {
    //                 let param = opcode.try_param().unwrap();
    //                 let n = param.to_u32() as usize;
    //                 for _ in 0..n {
    //                     let _ = self.stack.expect_pop();
    //                 }
    //             }
    //             Op::Add => {
    //                 self.binary_op_num(BinaryOpType::Add);
    //                 // let right = self.stack.pop().expect_float64();
    //                 // let left = self.stack.pop().expect_float64();
    //                 // let res = ZValue::Number(left + right);
    //                 // self.stack.push(res);
    //             }
    //             Op::Sub => {
    //                 self.binary_op_num(BinaryOpType::Sub);
    //             }
    //             Op::Div => self.binary_op_num(BinaryOpType::Div),
    //             Op::Mul => self.binary_op_num(BinaryOpType::Mul),
    //             Op::Neg => {
    //                 let val = self.stack.expect_pop().expect_float64();
    //                 self.stack.push(ZValue::Number(-val));
    //             }
    //             Op::Not => {
    //                 let val = self.stack.expect_pop();
    //                 let res = !val.is_truthy();
    //                 self.stack.push(ZValue::bool(res));
    //             }
    //             Op::Nil => {
    //                 let _ = self.stack.push(ZValue::Nil);
    //             }
    //             Op::True => {
    //                 let _ = self.stack.push(ZValue::bool(true));
    //             }
    //             Op::False => {
    //                 let _ = self.stack.push(ZValue::bool(false));
    //             }
    //             Op::Concat => todo!(),
    //             Op::Const8 | Op::Const16 | Op::Const24 | Op::Const32 | Op::Const64 => {
    //                 // let param = opcode.param.unwrap();
    //                 // let id = param.to_u32() as usize;
    //                 // let v = constants[id].clone();
    //                 // let v = self.chunk().try_read_const(opcode).unwrap();
    //                 let v = self.read_constant(&opcode).expect("Empty Call Frame!");
    //
    //                 self.stack.push(v.clone());
    //             }
    //             Op::Unknown => todo!(),
    //
    //             // NOTE: ----- DEFINE GLOBAL -----
    //             Op::DeclareGlobal8 | Op::DeclareGlobal16 | Op::DeclareGlobal32 => {
    //                 let name = self
    //                     .read_constant(&opcode)
    //                     .expect("Failed to read constant global!!!");
    //                 if let ZValue::Ident(s) = name.clone() {
    //                     let val = self.stack.expect_pop();
    //                     self.globals.insert(s.clone(), val);
    //                 }
    //             }
    //             Op::GetGlobal8 | Op::GetGlobal16 | Op::GetGlobal32 => {
    //                 if let Some(ZValue::Ident(name)) = self.read_constant(&opcode) {
    //                     if let Some(val) = self.globals.get(name) {
    //                         self.stack.push(val.clone());
    //                     } else {
    //                         bail!(
    //                             "{}",
    //                             RuntimeError::VMUnknownIdentifier {
    //                                 name: name.clone(),
    //                                 opcode,
    //                                 constants: self.frame_constants().to_vec(),
    //                                 globals: self.globals.clone()
    //                             }
    //                         )
    //                     }
    //                 } else {
    //                     panic!("Attempted to get Global, but its value in VM constants is not ZValue::Ident.")
    //                 }
    //             }
    //
    //             Op::SetGlobal8 | Op::SetGlobal16 | Op::SetGlobal32 => {
    //                 if let Some(ZValue::Ident(name)) = self.read_constant(&opcode) {
    //                     let top = self.stack.peek_top().expect("Nothing in stack to peek!!!");
    //
    //                     if self.globals.contains_key(name) {
    //                         self.globals.insert(name.clone(), top.clone());
    //                     } else {
    //                         bail!(
    //                             "{}",
    //                             RuntimeError::VMUnknownIdentifier {
    //                                 name: name.clone(),
    //                                 opcode,
    //                                 constants: self.frame_constants().to_vec(),
    //                                 globals: self.globals.clone(),
    //                             }
    //                         )
    //                     }
    //                 }
    //             }
    //             // NOTE: ----- END DEFINE GLOBAL -----
    //             Op::Eq => self.binary_op_num(BinaryOpType::Equals),
    //             Op::Gt => self.binary_op_num(BinaryOpType::Gt),
    //             Op::Lt => self.binary_op_num(BinaryOpType::Lt),
    //             Op::Ge => self.binary_op_num(BinaryOpType::Ge),
    //             Op::Le => self.binary_op_num(BinaryOpType::Le),
    //             Op::NotEq => self.binary_op_num(BinaryOpType::NotEquals),
    //
    //             // NOTE: ----- GET LOCAL -----
    //             Op::GetLocal8 | Op::GetLocal16 | Op::GetLocal32 => {
    //                 if let Some(local) = self.read_local(&opcode) {
    //                     self.stack.push(local.clone());
    //                 } else {
    //                     bail!("Unable to call GetLocal*")
    //                 }
    //             }
    //             // NOTE: ----- END GET LOCAL -----
    //
    //             // NOTE: ----- SET LOCAL -----
    //             Op::SetLocal8 | Op::SetLocal16 | Op::SetLocal32 => {}
    //
    //             Op::JumpFalse => {
    //                 if let Some(top) = self.stack.peek_top() {
    //                     if top.is_falsey() {
    //                         self.jump_to(&opcode);
    //                         // let jumpto = opcode
    //                         //     .try_param()
    //                         //     .expect("JumpFalse16 Op has no parameter!!!");
    //                         //
    //                         // frame.ip = jumpto.to_u32() as usize;
    //                     }
    //                 }
    //             }
    //
    //             Op::LongJumpFalse => todo!(),
    //             Op::Jump => {
    //                 // let jumpto = opcode.try_param().expect("Jump16 Op has no parameter!!!");
    //                 self.jump_to(&opcode);
    //                 // frame.ip = jumpto.to_usize();
    //             }
    //             Op::LongJump => todo!(),
    //             Op::JumpTrue => {
    //                 if let Some(top) = self.stack.peek_top() {
    //                     if top.is_truthy() {
    //                         self.jump_to(&opcode);
    //                         // let jumpto = opcode
    //                         //     .try_param()
    //                         //     .expect("JumpTrue16 Op has no parameter!!!");
    //                         //
    //                         // frame.ip = jumpto.to_u32() as usize;
    //                     }
    //                 }
    //             }
    //             Op::LongJumpTrue => todo!(),
    //             Op::NoOp => {}
    //             Op::Call => {
    //                 let nargs = opcode
    //                     .try_param()
    //                     .expect("Op::Call requires paramter and received none!!!")
    //                     .to_u32() as usize;
    //                 // .read_constant(&opcode)
    //                 // .expect(&format!("Failed to read constant from Frame: {:?}", frame));
    //                 // TODO: Fix this. we need to have function in stack memory BEFORE we compile
    //                 // its arguments and then emit Op::Call
    //                 if let Some(callable) = self.stack.peekn(nargs) {
    //                     match callable {
    //                         ZValue::Func(f) => {
    //                             self.call(Rc::clone(&f));
    //                         }
    //                         _ => todo!(),
    //                     }
    //                 }
    //             } // NOTE: ----- END SET LOCAL -----
    //         };
    //     }
    //
    //     let top = if let Some(t) = self.stack.peek_top() {
    //         t.clone()
    //     } else {
    //         ZValue::Nil
    //     };
    //     Ok(top)
    // }
    //
    fn jump_to(&mut self, jump_op: &Opcode) {
        let jumpto = jump_op
            .try_param()
            .expect(&format!("{} has no parameter!!!", jump_op.op().to_string()));
    }

    fn read_constant(&self, opcode: &Opcode) -> Option<&ZValue> {
        let f = self.try_frame()?;
        f.read_constant(opcode)
    }

    fn binary_op_num(&mut self, ty: BinaryOpType) {
        let right = self.stack.expect_pop().expect_float64();
        let left = self.stack.expect_pop().expect_float64();

        let res: ZValue = match ty {
            BinaryOpType::Gt => (left > right).into(),
            BinaryOpType::Lt => (left < right).into(),
            BinaryOpType::Ge => (left >= right).into(),
            BinaryOpType::Le => (left <= right).into(),
            BinaryOpType::And => panic!("No opcode for And"),
            BinaryOpType::Or => panic!("No opcode for Or"),
            BinaryOpType::Equals => (left == right).into(),
            BinaryOpType::NotEquals => (left != right).into(),
            BinaryOpType::BitAnd => todo!(),
            BinaryOpType::BitOr => todo!(),
            BinaryOpType::Xor => todo!(),
            BinaryOpType::Concat => todo!(),
            BinaryOpType::Add => (left + right).into(),
            BinaryOpType::Sub => (left - right).into(),
            BinaryOpType::Mul => (left * right).into(),
            BinaryOpType::Div => (left / right).into(),
        };

        self.stack.push(res);
    }
    // for opcode in self.chunk.iter() {
    // Ok(opcode.offset())
    // vm_exec_opcode(&mut self.stack, &chunk.constants, opcode)?;
    // }

    // pub fn exec_chunks(&mut self, chs: &[Chunk]) -> anyhow::Result<()> {
    //     for c in chs {
    //         self.exec(*c)?;
    //     }
    //     Ok(())
    // }
    //
    // pub fn exec_file(&mut self, path: &str) -> anyhow::Result<ZValue> {
    //     let source = std::fs::read_to_string(path)?;
    //
    //     self.exec_source(&source)
    // }
    //
    pub fn new_frame(&self, func: Rc<FuncChunk>) -> CallFrame {
        let arity = func.arity as isize;
        let beg = self.stack.top_index().unwrap() as isize - arity - 1;
        let start_slot = std::cmp::min(0, beg) as usize;
        CallFrame {
            func: Some(func),
            ip: 0,
            start_slot,
        }
    }

    // pub fn exec_source(&mut self, src: &str) -> anyhow::Result<ZValue> {
    //     let f = self.compile_source(src)?;
    //     let f = Rc::new(f);
    //     self.call(f);
    //     // let cf = self.new_frame(f);
    //     // SAFETY: new_frame gaurantees that CallFrame::func is not None,
    //     // so this is safe.
    //
    //     self.exec()
    // }
    #[inline]
    pub fn frame_memory(&self, frame: &CallFrame) -> &[ZValue] {
        &self.stack[frame.start_slot..]
    }

    //
    // pub fn bytecode(&self) -> &Bytecode {
    //     self.chunk().code()
    // }

    fn read_local(&self, opcode: &Opcode) -> Option<&ZValue> {
        let frame = self
            .try_frame()
            .expect("Cannot read local from empty CallFrame!!!");
        if let Some(param) = opcode.try_param() {
            let slot = param.to_usize();
            let slot = frame.start_slot + slot;
            assert!(slot < self.stack.len());
            let v = &self.stack[slot];
            Some(v)
        } else {
            None
        }
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
                let val = &self.frame_constants()[index];
                if let ZValue::Func(f) = val {
                    let name = f.name();
                    let arity = f.arity();
                    let arity_s = if arity == 0 {
                        String::new()
                    } else {
                        arity.to_string()
                    };
                    let code = f.chunk();
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

    // #[inline]
    // pub fn memfor(&self, frame: &CallFrame) -> &[ZValue] {
    //     &self.stack[frame.mem_slots.clone()]
    // }
}

impl Display for VM {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let d = self.dump();
        write!(f, "{d}")
    }
}

#[derive(Debug, Clone)]
pub struct CallFrame {
    // TODO: For now this is an Rc, but when i implement my
    // dynanuc stack for the VM, this will changed since the entire Func will live on the stack
    func: Option<Rc<FuncChunk>>,
    ip: usize,
    /// index offset from top of stack that
    /// this call frames variables live at.
    start_slot: usize,
}

impl Default for CallFrame {
    fn default() -> Self {
        Self::zeroed()
    }
}

impl CallFrame {
    pub fn zeroed() -> Self {
        Self {
            func: None,
            ip: 0,
            start_slot: 0,
        }
    }

    pub fn code(&self) -> &Chunk {
        self.try_code().expect("Cannot get code from empty Chunk!")
    }

    pub fn try_code(&self) -> Option<&Chunk> {
        if let Some(c) = self.func.as_ref() {
            Some(&c.chunk)
        } else {
            None
        }
    }

    /// Unconditional jump to location index in bytecode.
    /// panics if location out of range.
    pub fn jump_to(&mut self, location: usize) {
        assert!(
            location < self.bytecode_len(),
            "Access violation! Got: {location} when bytecode length is: {}!",
            self.bytecode_len()
        );
        self.ip = location;
    }

    pub fn try_constants(&self) -> Option<&[ZValue]> {
        let c = self.try_code()?;
        Some(&c.constants)
    }

    pub fn constants(&self) -> &[ZValue] {
        let c = self.code();
        &c.constants
    }

    pub fn read_constant(&self, opcode: &Opcode) -> Option<&ZValue> {
        let param = opcode.try_param()?;
        let id = param.to_usize();
        let cs = self.try_constants()?;
        Some(&cs[id])
    }

    pub fn bytecode_len(&self) -> usize {
        if let Some(c) = self.try_code() {
            c.len()
        } else {
            0
        }
    }

    /// Gets opcode at current self.ip, then increments self.ip to the next opcode.
    pub fn next_opcode(&mut self) -> Option<Opcode> {
        let opcode = self.opcode_at(self.ip).ok()?;
        self.ip += opcode.offset();
        Some(opcode)
    }

    pub fn opcode_at(&self, index: usize) -> anyhow::Result<Opcode> {
        if let Some(f) = self.func.as_ref() {
            let opcode = self.code().opcode_at(index).context(format!(
                "Opcode index out of range! Got: {index} but chunk length is only {}!",
                self.code().len()
            ))?;
            Ok(opcode)
        } else {
            bail!("Tried to get Opcode from an Option<Func> that is None!!")
        }
    }
}

impl Display for CallFrame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = if let Some(func) = &self.func {
            let name = func.nam;
            let arity = func.arity as usize;
            let stack_mem = self.start_slot;
            let ip = self.ip;
            format!("CallFrame :: fn {name}/{arity}\n\t=> Call Frame Indicies: {stack_mem:?}\n\t=> ip: {ip}")
        } else {
            String::from("CallFrame::func == None")
        };
        write!(f, "{s}")
    }
}
