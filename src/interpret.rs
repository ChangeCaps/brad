use crate::mir;

enum Flow {
    Return(Value),
    Break(Value),
}

pub struct Interpreter {
    mir: mir::Program,
}

impl Interpreter {
    pub fn new(mir: mir::Program) -> Self {
        Self { mir }
    }

    pub fn run(&self, body: mir::BodyId, generics: Vec<mir::Ty>) {
        let body = &self.mir.bodies[body];

        let mut frame = Frame {
            generics,
            locals: vec![Value::None; body.locals.len()],
        };

        match self.eval_block(&mut frame, &body.block) {
            Ok(()) => {}
            Err(Flow::Return(value)) => println!("{:?}", value),
            Err(Flow::Break(value)) => println!("{:?}", value),
        }
    }

    fn eval_block(&self, frame: &mut Frame, block: &mir::Block) -> Result<(), Flow> {
        for stmt in &block.stmts {
            self.eval_stmt(frame, stmt)?;
        }

        match block.term {
            mir::Term::Return(ref value) => Err(Flow::Return(self.eval_value(frame, value))),
            mir::Term::Exit => Ok(()),
        }
    }

    fn eval_stmt(&self, frame: &mut Frame, stmt: &mir::Stmt) -> Result<(), Flow> {
        match stmt {
            mir::Stmt::Assign(place, value) => {
                let value = self.eval_value(frame, value);
                self.assign(frame, place, value);
                Ok(())
            }

            mir::Stmt::Match {
                target,
                default,
                cases,
            } => {
                let value = self.eval_place(frame, target);

                let Value::Union { ty, value } = value else {
                    panic!("expected union, got {:?}", value);
                };

                for case in cases {
                    if ty == case.ty {
                        frame.locals[case.local.0] = *value;

                        return self.eval_block(frame, &case.block);
                    }
                }

                self.eval_block(frame, default)
            }
        }
    }

    fn eval_value(&self, frame: &mut Frame, value: &mir::Value) -> Value {
        match value {
            mir::Value::Use(operand) => self.eval_operand(frame, operand),

            mir::Value::Tuple(operands) => {
                let values = operands
                    .iter()
                    .map(|operand| self.eval_operand(frame, operand))
                    .collect();

                Value::Tuple(values)
            }

            mir::Value::Record(fields) => {
                let values = fields
                    .iter()
                    .map(|(name, operand)| (*name, self.eval_operand(frame, operand)))
                    .collect();

                Value::Record(values)
            }

            mir::Value::Promote { input, operand, .. } => {
                let value = self.eval_operand(frame, operand);

                Value::Union {
                    ty: input.clone().specialize(&frame.generics),
                    value: Box::new(value),
                }
            }

            mir::Value::Coerce { operand, .. } => self.eval_operand(frame, operand),

            mir::Value::Call(func, value) => {
                let func = self.eval_operand(frame, func);
                let value = self.eval_operand(frame, value);

                let Value::Func {
                    body,
                    generics,
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
                        generics,
                        captures,
                        missing: missing - 1,
                    };
                }

                let body = &self.mir.bodies[body];
                let mut frame = Frame {
                    generics,
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

            mir::Value::Binary(op, lhs, rhs) => {
                let lhs = self.eval_operand(frame, lhs);
                let rhs = self.eval_operand(frame, rhs);

                match op {
                    mir::BinaryOp::Addi => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs + rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Subi => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs - rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Muli => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs * rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Divi => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs / rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Modi => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs % rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::BitAndi => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs & rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::BitOri => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs | rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::BitXori => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs ^ rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Shli => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs << rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Shri => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs >> rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Eqi => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => self.create_bool(lhs == rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Nei => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => self.create_bool(lhs != rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Lti => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => self.create_bool(lhs < rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Lei => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => self.create_bool(lhs <= rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Gti => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => self.create_bool(lhs > rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Gei => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => self.create_bool(lhs >= rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Addf => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs + rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Subf => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs - rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Mulf => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs * rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Divf => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs / rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Modf => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs % rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Eqf => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => self.create_bool(lhs == rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Nef => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => self.create_bool(lhs != rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Ltf => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => self.create_bool(lhs < rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Lef => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => self.create_bool(lhs <= rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Gtf => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => self.create_bool(lhs > rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::Gef => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => self.create_bool(lhs >= rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    mir::BinaryOp::And | mir::BinaryOp::Or => todo!(),
                }
            }

            mir::Value::Unary(op, operand) => match op {
                mir::UnaryOp::Negi => match self.eval_operand(frame, operand) {
                    Value::Int(value) => Value::Int(-value),
                    value => panic!("expected integer, got {:?}", value),
                },

                mir::UnaryOp::BitNoti => match self.eval_operand(frame, operand) {
                    Value::Int(value) => Value::Int(!value),
                    value => panic!("expected integer, got {:?}", value),
                },

                mir::UnaryOp::Negf => match self.eval_operand(frame, operand) {
                    Value::Float(value) => Value::Float(-value),
                    value => panic!("expected float, got {:?}", value),
                },

                mir::UnaryOp::Not => todo!(),

                mir::UnaryOp::Deref => match self.eval_operand(frame, operand) {
                    Value::Ref(value) => *value,
                    value => panic!("expected reference, got {:?}", value),
                },
            },

            mir::Value::Closure {
                body,
                captures,
                generics,
                ..
            } => {
                let captures = captures
                    .iter()
                    .map(|capture| self.eval_operand(frame, capture))
                    .collect();

                Value::Func {
                    body: *body,
                    generics: generics.clone(),
                    captures,
                    missing: self.mir.bodies[*body].arguments,
                }
            }
        }
    }

    fn create_bool(&self, value: bool) -> Value {
        match value {
            true => Value::Union {
                ty: mir::Ty::True,
                value: Box::new(Value::None),
            },

            false => Value::Union {
                ty: mir::Ty::False,
                value: Box::new(Value::None),
            },
        }
    }

    fn eval_operand(&self, frame: &mut Frame, operand: &mir::Operand) -> Value {
        match operand {
            mir::Operand::Place(place) => self.eval_place(frame, place),
            mir::Operand::Const(const_) => self.eval_const(const_),
        }
    }

    fn eval_place(&self, frame: &mut Frame, place: &mir::Place) -> Value {
        let mut value = frame.locals[place.local.0].clone();

        for proj in &place.proj {
            match proj {
                mir::Proj::Field(name) => {
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

                mir::Proj::Tuple(index) => {
                    match value {
                        Value::Tuple(values) => value = values[*index].clone(),
                        _ => panic!("expected tuple, got {:?}", value),
                    };
                }

                mir::Proj::Index(_) => todo!(),

                mir::Proj::Deref => {
                    match value {
                        Value::Ref(new_value) => value = *new_value,
                        _ => panic!("expected union, got {:?}", value),
                    };
                }
            }
        }

        value
    }

    fn eval_const(&self, const_: &mir::Const) -> Value {
        match const_ {
            mir::Const::None => Value::None,
            mir::Const::Int(int) => Value::Int(*int),
            mir::Const::Float(float) => Value::Float(*float),
            mir::Const::String(string) => Value::String(String::from(*string)),
        }
    }

    fn assign(&self, frame: &mut Frame, place: &mir::Place, value: Value) {
        fn recurse<'a>(
            target: &mut Value,
            mut projs: impl Iterator<Item = &'a mir::Proj>,
            value: Value,
        ) {
            let Some(proj) = projs.next() else {
                *target = value;
                return;
            };

            match proj {
                mir::Proj::Field(name) => {
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
                mir::Proj::Tuple(idx) => {
                    match target {
                        Value::Tuple(ref mut values) => recurse(&mut values[*idx], projs, value),
                        _ => panic!("expected tuple, got {:?}", target),
                    };
                }
                mir::Proj::Index(_) => todo!(),
                mir::Proj::Deref => {
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
    generics: Vec<mir::Ty>,
    locals: Vec<Value>,
}

#[derive(Clone, Debug)]
enum Value {
    None,

    Int(i64),

    Float(f64),

    String(String),

    Union {
        ty: mir::Ty,
        value: Box<Value>,
    },

    Record(Vec<(&'static str, Value)>),

    Tuple(Vec<Value>),

    List(Vec<Value>),

    Func {
        body: mir::BodyId,
        generics: Vec<mir::Ty>,
        captures: Vec<Value>,
        missing: usize,
    },

    Ref(Box<Value>),
}
