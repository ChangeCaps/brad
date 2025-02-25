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

    pub fn run(&self, body: mir::BodyId) {
        let body = &self.mir.bodies[body];

        let mut frame = Frame {
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
            self.eval_stmt(frame, stmt);
        }

        match block.term {
            mir::Term::Return(ref value) => Err(Flow::Return(self.eval_value(frame, value))),
            mir::Term::Exit => Ok(()),
        }
    }

    fn eval_stmt(&self, frame: &mut Frame, stmt: &mir::Stmt) {
        match stmt {
            mir::Stmt::Assign(place, value) => {
                let value = self.eval_value(frame, value);
                self.assign(frame, place, value);
            }

            mir::Stmt::Match(place, block, vec) => {
                todo!()
            }
        }
    }

    fn eval_value(&self, frame: &mut Frame, value: &mir::Value) -> Value {
        match value {
            mir::Value::Use(operand) => self.eval_operand(frame, operand),

            mir::Value::Record(operands) => {
                let values = operands
                    .iter()
                    .map(|operand| self.eval_operand(frame, operand))
                    .collect();

                Value::Record(values)
            }

            mir::Value::Promote(tid, _, operand) => {
                let value = self.eval_operand(frame, operand);
                Value::Union(*tid, Box::new(value))
            }

            mir::Value::Coerce(_, _, operand) => self.eval_operand(frame, operand),

            mir::Value::Call(func, value) => {
                let func = self.eval_operand(frame, func);
                let value = self.eval_operand(frame, value);

                match func {
                    Value::Func(body, captures) => {
                        let body = &self.mir.bodies[body];
                        let mut frame = Frame {
                            locals: vec![Value::None; body.locals.len()],
                        };

                        for (i, capture) in captures.iter().enumerate() {
                            frame.locals[i] = capture.clone();
                        }

                        frame.locals[captures.len()] = value;

                        match self.eval_block(&mut frame, &body.block) {
                            Err(Flow::Return(value)) => value,
                            _ => panic!("expected return"),
                        }
                    }
                    _ => panic!("expected function, got {:?}", func),
                }
            }

            mir::Value::Binary(binary_op, operand, operand1) => todo!(),
            mir::Value::Unary(unary_op, operand) => todo!(),

            mir::Value::Closure(body, captures, _) => {
                let captures = captures
                    .iter()
                    .map(|capture| self.eval_operand(frame, capture))
                    .collect();

                Value::Func(*body, captures)
            }
        }
    }

    fn eval_operand(&self, frame: &mut Frame, operand: &mir::Operand) -> Value {
        match operand {
            mir::Operand::Place(place) => self.eval_place(frame, place),
            mir::Operand::Const(const_) => self.eval_const(const_),
        }
    }

    fn eval_place(&self, frame: &mut Frame, place: &mir::Place) -> Value {
        let mut value = frame.locals[place.local.index()].clone();

        for proj in &place.proj {
            match proj {
                mir::Proj::Field(_) => todo!(),
                mir::Proj::Index(local) => todo!(),
                mir::Proj::Deref => todo!(),
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
        //let mut value = value;

        //for proj in &place.proj {
        //    match proj {
        //        mir::Proj::Field(_) => todo!(),
        //        mir::Proj::Index(local) => todo!(),
        //        mir::Proj::Deref => todo!(),
        //    }
        //}

        frame.locals[place.local.index()] = value;
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
    Union(mir::Tid, Box<Value>),
    Record(Vec<Value>),
    Func(mir::BodyId, Vec<Value>),
}
