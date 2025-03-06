use crate::sir;

enum Flow {
    Return(Value),
    Break,
}

pub struct Interpreter {
    sir: sir::Program,
}

impl Interpreter {
    pub fn new(sir: sir::Program) -> Self {
        Self { sir }
    }

    pub fn run(&self, body: sir::BodyId) {
        let body = &self.sir.bodies[body];

        let mut frame = Frame {
            locals: vec![Value::None; body.locals.len()],
        };

        match self.eval_block(&mut frame, &body.block) {
            Ok(()) => panic!("expected return"),
            Err(Flow::Return(value)) => println!("{:?}", value),
            Err(Flow::Break) => panic!("unexpected break"),
        }
    }

    fn eval_block(&self, frame: &mut Frame, block: &sir::Block) -> Result<(), Flow> {
        for stmt in &block.stmts {
            self.eval_stmt(frame, stmt)?;
        }

        match block.term {
            sir::Term::Return(ref value) => Err(Flow::Return(self.eval_value(frame, value))),
            sir::Term::Break => Err(Flow::Break),
            sir::Term::Exit => Ok(()),
        }
    }

    fn eval_stmt(&self, frame: &mut Frame, stmt: &sir::Stmt) -> Result<(), Flow> {
        match stmt {
            sir::Stmt::Assign(place, value) => {
                let value = self.eval_value(frame, value);
                self.assign(frame, place, value);
                Ok(())
            }

            sir::Stmt::Loop(block) => loop {
                match self.eval_block(frame, block) {
                    Err(Flow::Return(value)) => break Err(Flow::Return(value)),
                    Err(Flow::Break) => break Ok(()),
                    Ok(()) => continue,
                }
            },

            sir::Stmt::Match {
                target,
                default,
                cases,
            } => {
                let value = self.eval_place(frame, target);

                let Value::Union { ty, value } = value else {
                    panic!("expected union, got {:?}", value);
                };

                for case in cases {
                    if ty == case.ty.clone() {
                        frame.locals[case.local.0] = *value;

                        return self.eval_block(frame, &case.block);
                    }
                }

                self.eval_block(frame, default)
            }
        }
    }

    fn eval_value(&self, frame: &mut Frame, value: &sir::Value) -> Value {
        match value {
            sir::Value::Use(operand) => self.eval_operand(frame, operand),

            sir::Value::Tuple(operands) => {
                let values = operands
                    .iter()
                    .map(|operand| self.eval_operand(frame, operand))
                    .collect();

                Value::Tuple(values)
            }

            sir::Value::Record(fields) => {
                let values = fields
                    .iter()
                    .map(|(name, operand)| (*name, self.eval_operand(frame, operand)))
                    .collect();

                Value::Record(values)
            }

            sir::Value::Promote {
                input,
                variants,
                operand,
            } => {
                let value = self.eval_operand(frame, operand);

                if variants.len() == 1 {
                    return value;
                }

                Value::Union {
                    ty: input.clone(),
                    value: Box::new(value),
                }
            }

            sir::Value::Coerce { operand, .. } => self.eval_operand(frame, operand),

            sir::Value::Call(func, value) => {
                let func = self.eval_operand(frame, func);
                let value = self.eval_operand(frame, value);

                let Value::Func {
                    body,
                    mut captures,
                    missing,
                } = func
                else {
                    panic!("expected function, got {:?}", func);
                };

                captures.push(value);

                if missing > 1 {
                    return Value::Func {
                        body,
                        captures,
                        missing: missing - 1,
                    };
                }

                let body = &self.sir.bodies[body];
                let mut frame = Frame {
                    locals: vec![Value::None; body.locals.len()],
                };

                for (i, capture) in captures.iter().enumerate() {
                    frame.locals[i] = capture.clone();
                }

                match self.eval_block(&mut frame, &body.block) {
                    Err(Flow::Return(value)) => value,
                    _ => panic!("expected return"),
                }
            }

            sir::Value::Binary(op, lhs, rhs) => {
                let lhs = self.eval_operand(frame, lhs);
                let rhs = self.eval_operand(frame, rhs);

                match op {
                    mir::BinaryOp::Add => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs + rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Sub => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs - rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Mul => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs * rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Div => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs / rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Mod => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs % rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::BAnd => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs & rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::BOr => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs | rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::BXor => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs ^ rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::LShl => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs << rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::LShr => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs >> rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Eq => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => self.create_bool(lhs == rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Ne => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => self.create_bool(lhs != rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Lt => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => self.create_bool(lhs < rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Le => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => self.create_bool(lhs <= rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Gt => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => self.create_bool(lhs > rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Ge => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => self.create_bool(lhs >= rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::FAdd => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs + rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::FSub => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs - rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::FMul => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs * rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::FDiv => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs / rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::FMod => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs % rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::FEq => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => self.create_bool(lhs == rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::FNe => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => self.create_bool(lhs != rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::FLt => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => self.create_bool(lhs < rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::FLe => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => self.create_bool(lhs <= rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::FGt => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => self.create_bool(lhs > rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::FGe => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => self.create_bool(lhs >= rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::And | mir::BinaryOp::Or => {
                        todo!("Implement runtime booleans (type ids)")
                    }
                }
            }

            mir::Value::Unary(op, operand) => match op {
                mir::UnaryOp::Neg => match self.eval_operand(frame, operand) {
                    Value::Int(value) => Value::Int(-value),
                    value => panic!("expected integer, got {:?}", value),
                },

                mir::UnaryOp::BNot => match self.eval_operand(frame, operand) {
                    Value::Int(value) => Value::Int(!value),
                    value => panic!("expected integer, got {:?}", value),
                },

                mir::UnaryOp::FNeg => match self.eval_operand(frame, operand) {
                    Value::Float(value) => Value::Float(-value),
                    value => panic!("expected float, got {:?}", value),
                },

                mir::UnaryOp::Not => todo!("Implement runtime booleans (type ids)"),

                sir::UnaryOp::Deref => match self.eval_operand(frame, operand) {
                    Value::Ref(value) => *value,
                    value => panic!("expected reference, got {:?}", value),
                },
            },

            sir::Value::Closure { body, captures, .. } => {
                let captures = captures
                    .iter()
                    .map(|capture| self.eval_operand(frame, capture))
                    .collect();

                Value::Func {
                    body: *body,
                    captures,
                    missing: self.sir.bodies[*body].arguments,
                }
            }
        }
    }

    fn create_bool(&self, value: bool) -> Value {
        match value {
            true => Value::Union {
                ty: self.sir.types[sir::Ty::True],
                value: Box::new(Value::None),
            },

            false => Value::Union {
                ty: self.sir.types[sir::Ty::False],
                value: Box::new(Value::None),
            },
        }
    }

    fn eval_operand(&self, frame: &mut Frame, operand: &sir::Operand) -> Value {
        match operand {
            sir::Operand::Place(place) => self.eval_place(frame, place),
            sir::Operand::Const(const_) => self.eval_const(const_),
        }
    }

    fn eval_place(&self, frame: &mut Frame, place: &sir::Place) -> Value {
        let mut value = frame.locals[place.local.0].clone();

        for proj in &place.proj {
            match proj {
                sir::Proj::Field(name) => {
                    match value {
                        Value::Record(ref values) => {
                            for (field_name, field_value) in values {
                                if field_name == name {
                                    value = field_value.clone();
                                    break;
                                }
                            }
                        }
                        _ => panic!("expected record, got {:?}", value),
                    };
                }

                sir::Proj::Tuple(index) => {
                    match value {
                        Value::Tuple(values) => value = values[*index].clone(),
                        _ => panic!("expected tuple, got {:?}", value),
                    };
                }

                sir::Proj::Index(_) => todo!(),

                sir::Proj::Deref => {
                    match value {
                        Value::Ref(new_value) => value = *new_value,
                        _ => panic!("expected union, got {:?}", value),
                    };
                }
            }
        }

        value
    }

    fn eval_const(&self, const_: &sir::Const) -> Value {
        match const_ {
            sir::Const::None => Value::None,
            sir::Const::Int(int) => Value::Int(*int),
            sir::Const::Float(float) => Value::Float(*float),
            sir::Const::String(string) => Value::String(String::from(*string)),
        }
    }

    fn assign(&self, frame: &mut Frame, place: &sir::Place, value: Value) {
        fn recurse<'a>(
            target: &mut Value,
            mut projs: impl Iterator<Item = &'a sir::Proj>,
            value: Value,
        ) {
            let Some(proj) = projs.next() else {
                *target = value;
                return;
            };

            match proj {
                sir::Proj::Field(name) => {
                    match target {
                        Value::Record(ref mut values) => {
                            for (field_name, field_value) in values {
                                if field_name == name {
                                    recurse(field_value, projs, value);
                                    return;
                                }
                            }

                            panic!("field not found: {:?}", name);
                        }
                        _ => panic!("expected record, got {:?}", target),
                    };
                }
                sir::Proj::Tuple(idx) => {
                    match target {
                        Value::Tuple(ref mut values) => recurse(&mut values[*idx], projs, value),
                        _ => panic!("expected tuple, got {:?}", target),
                    };
                }
                sir::Proj::Index(_) => todo!(),
                sir::Proj::Deref => {
                    match target {
                        Value::Ref(target) => recurse(target, projs, value),
                        _ => panic!("expected reference, got {:?}", target),
                    };
                }
            }
        }

        recurse(&mut frame.locals[place.local.0], place.proj.iter(), value);
    }
}

struct Frame {
    locals: Vec<Value>,
}

#[derive(Clone, Debug)]
enum Value {
    None,

    Int(i64),

    Float(f64),

    String(String),

    Union {
        ty: sir::Tid,
        value: Box<Value>,
    },

    Record(Vec<(&'static str, Value)>),

    Tuple(Vec<Value>),

    List(Vec<Value>),

    Func {
        body: sir::BodyId,
        captures: Vec<Value>,
        missing: usize,
    },

    Ref(Box<Value>),
}
