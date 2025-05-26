use super::sir;

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

    pub fn run(&self, body: sir::Bid) {
        let body = &self.sir.bodies[body];

        let mut frame = Frame {
            locals: vec![Value::None; body.locals.len()],
        };

        match self.eval_block(&mut frame, body.block.as_ref().unwrap()) {
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
            Some(sir::Term::Return(ref value)) => Err(Flow::Return(self.eval_value(frame, value))),
            Some(sir::Term::Break) => Err(Flow::Break),
            None => Ok(()),
        }
    }

    fn eval_stmt(&self, frame: &mut Frame, stmt: &sir::Stmt) -> Result<(), Flow> {
        match stmt {
            sir::Stmt::Drop(_) => Ok(()),

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
                    if ty == case.ty {
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

            sir::Value::Ref(_) => todo!(),

            sir::Value::List(operands) => {
                let values = operands
                    .iter()
                    .map(|operand| self.eval_operand(frame, operand))
                    .collect();

                Value::List(values)
            }

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
                variant: input,
                variants,
                operand,
            } => {
                let value = self.eval_operand(frame, operand);

                if variants.len() == 1 {
                    return value;
                }

                Value::Union {
                    ty: *input,
                    value: Box::new(value),
                }
            }

            sir::Value::Coerce { operand, .. } => self.eval_operand(frame, operand),

            sir::Value::Call(func, value) => {
                let func = self.eval_place(frame, func);
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

                if body.is_extern {
                    if let Some(name) = body
                        .attrs
                        .attributes
                        .iter()
                        .filter(|a| a.name == "link" && a.value.is_some())
                        .map(|a| a.value.clone().unwrap().to_string())
                        .collect::<Vec<_>>()
                        .first()
                    {
                        match name.as_str() {
                            "brad_print" => {
                                if let Value::String(string) = &captures[0] {
                                    println!("{}", string);
                                } else {
                                    panic!("expected string, got {:?}", captures[0]);
                                }

                                return Value::None;
                            }

                            "brad_str_concat" => {
                                if let (Value::String(lhs), Value::String(rhs)) =
                                    (&captures[0], &captures[1])
                                {
                                    return Value::String(format!("{}{}", lhs, rhs));
                                } else {
                                    panic!(
                                        "expected strings, got {:?} and {:?}",
                                        captures[0], captures[1]
                                    );
                                }
                            }

                            _ => {}
                        };

                        println!("call external function: {}", name);
                    }

                    todo!(
                        "call external function: {:?} that was not implemented",
                        body
                    );
                }

                let mut frame = Frame {
                    locals: vec![Value::None; body.locals.len()],
                };

                for (i, capture) in captures.iter().enumerate() {
                    frame.locals[i] = capture.clone();
                }

                match self.eval_block(&mut frame, body.block.as_ref().unwrap()) {
                    Err(Flow::Return(value)) => value,
                    _ => panic!("expected return"),
                }
            }

            sir::Value::Binary(op, lhs, rhs) => {
                let lhs = self.eval_operand(frame, lhs);
                let rhs = self.eval_operand(frame, rhs);

                match op {
                    sir::BinaryOp::Add => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs + rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    sir::BinaryOp::Sub => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs - rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    sir::BinaryOp::Mul => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs * rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    sir::BinaryOp::Div => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs / rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    sir::BinaryOp::Mod => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs % rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    sir::BinaryOp::BAnd => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs & rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    sir::BinaryOp::BOr => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs | rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    sir::BinaryOp::BXor => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs ^ rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    sir::BinaryOp::LShl => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs << rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    sir::BinaryOp::LShr => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs >> rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    sir::BinaryOp::Eq => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => self.create_bool(lhs == rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    sir::BinaryOp::Ne => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => self.create_bool(lhs != rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    sir::BinaryOp::Lt => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => self.create_bool(lhs < rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    sir::BinaryOp::Le => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => self.create_bool(lhs <= rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    sir::BinaryOp::Gt => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => self.create_bool(lhs > rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    sir::BinaryOp::Ge => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => self.create_bool(lhs >= rhs),
                        (lhs, rhs) => panic!("expected integers, got {:?} and {:?}", lhs, rhs),
                    },

                    sir::BinaryOp::FAdd => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs + rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    sir::BinaryOp::FSub => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs - rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    sir::BinaryOp::FMul => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs * rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    sir::BinaryOp::FDiv => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs / rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    sir::BinaryOp::FMod => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs % rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    sir::BinaryOp::FLt => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => self.create_bool(lhs < rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    sir::BinaryOp::FLe => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => self.create_bool(lhs <= rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    sir::BinaryOp::FGt => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => self.create_bool(lhs > rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    sir::BinaryOp::FGe => match (lhs, rhs) {
                        (Value::Float(lhs), Value::Float(rhs)) => self.create_bool(lhs >= rhs),
                        (lhs, rhs) => panic!("expected floats, got {:?} and {:?}", lhs, rhs),
                    },

                    sir::BinaryOp::And | sir::BinaryOp::Or => {
                        todo!("Implement runtime booleans (type ids)")
                    }
                }
            }

            sir::Value::Unary(op, operand) => match op {
                sir::UnaryOp::Neg => match self.eval_operand(frame, operand) {
                    Value::Int(value) => Value::Int(-value),
                    value => panic!("expected integer, got {:?}", value),
                },

                sir::UnaryOp::BNot => match self.eval_operand(frame, operand) {
                    Value::Int(value) => Value::Int(!value),
                    value => panic!("expected integer, got {:?}", value),
                },

                sir::UnaryOp::FNeg => match self.eval_operand(frame, operand) {
                    Value::Float(value) => Value::Float(-value),
                    value => panic!("expected float, got {:?}", value),
                },

                sir::UnaryOp::Not => todo!("Implement runtime booleans (type ids)"),
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
            sir::Operand::Load(place) => self.eval_place(frame, place),
            sir::Operand::Const(const_, _) => self.eval_const(const_),
        }
    }

    fn eval_place(&self, frame: &mut Frame, place: &sir::Place) -> Value {
        let mut value = frame.locals[place.local.0].clone();

        for (proj, _) in &place.proj {
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
            mut projs: impl Iterator<Item = &'a (sir::Proj, sir::Tid)>,
            value: Value,
        ) {
            let Some((proj, _)) = projs.next() else {
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
        body: sir::Bid,
        captures: Vec<Value>,
        missing: usize,
    },

    Ref(Box<Value>),
}
