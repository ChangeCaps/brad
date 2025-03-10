use crate::attribute::Attribute;
use crate::mir;
use crate::mir::Const;
use std::io::Write;

const INDENT_SIZE: usize = 4;

pub struct Formatter<'a, W: Write> {
    indent: usize,
    writer: W,
    program: &'a mir::Program,
}

type Result = std::io::Result<()>;

/// Store absolute and relative idx
pub enum LocalKind {
    Capture(usize, usize),
    Argument(usize, usize),
    Local(usize, usize),
}

impl LocalKind {
    pub fn name(&self) -> &str {
        match self {
            LocalKind::Capture(_, _) => "cap",
            LocalKind::Argument(_, _) => "arg",
            LocalKind::Local(_, _) => "loc",
        }
    }

    pub fn format_absolute(&self) -> String {
        match self {
            LocalKind::Capture(i, _) | LocalKind::Argument(i, _) | LocalKind::Local(i, _) => {
                format!("local({}) ; {}", i, self.name())
            }
        }
    }

    pub fn format_relative(&self) -> String {
        match self {
            LocalKind::Capture(_, i) | LocalKind::Argument(_, i) | LocalKind::Local(_, i) => {
                format!("{}({})", self.name(), i)
            }
        }
    }
}

impl<'a, W: Write> Formatter<'a, W> {
    pub fn new(writer: W, program: &'a mir::Program) -> Self {
        Self {
            indent: 0,
            writer,
            program,
        }
    }
    fn indent(&mut self, f: impl FnOnce(&mut Self) -> Result) -> Result {
        self.indent += 1;
        let result = f(self);
        self.indent -= 1;
        result
    }

    fn write_indent(&mut self) -> Result {
        self.write_indents(1)
    }

    fn write_indents(&mut self, n: usize) -> Result {
        write!(
            self.writer,
            "{}",
            format!("\n{}", " ".repeat(INDENT_SIZE * self.indent)).repeat(n)
        )
    }

    pub fn format_program(&mut self) -> Result {
        for (_, named) in self.program.types.iter() {
            write!(
                self.writer,
                "type {} = {}",
                named.name,
                self.program.types.format(&named.ty)
            )?;

            self.write_indents(2)?;
        }

        for (i, (bid, body)) in self.program.bodies.iter().enumerate() {
            if i > 0 {
                write!(self.writer, "\n\n")?;
            }

            for attribute in &body.attrs.attributes {
                self.format_attribute(attribute)?;
            }

            let name = body
                .name
                .clone()
                .unwrap_or_else(|| format!("body_{}", bid.0));

            if body.is_extern {
                write!(self.writer, "extern ")?;
            }

            write!(self.writer, "fn {}", name)?;

            write!(
                self.writer,
                " -> {}",
                self.program.types.format(&body.output)
            )?;

            self.indent(|f| {
                for i in 0..body.locals.len() {
                    f.write_indent()?;
                    let local = mir::Local(i);
                    let local_kind = f.local_kind(body, local);
                    let local_ty = &body.locals[local];
                    write!(
                        f.writer,
                        "{}: {}",
                        local_kind.format_relative(),
                        f.program.types.format(local_ty)
                    )?;
                }

                if let Some(block) = &body.block {
                    f.format_block(body, block)?;
                }

                Ok(())
            })?;

            writeln!(self.writer)?;
        }

        Ok(())
    }

    fn format_attribute(&mut self, attribute: &Attribute) -> Result {
        write!(self.writer, "#[{}", attribute.name)?;

        if let Some(value) = &attribute.value {
            write!(self.writer, " = \"{}\"", value)?;
        }

        writeln!(self.writer, "]")
    }

    fn format_block(&mut self, body: &mir::Body, block: &mir::Block) -> Result {
        for stmt in &block.stmts {
            self.write_indent()?;
            self.format_stmt(body, stmt)?;
        }

        self.write_indent()?;

        if let Some(term) = &block.term {
            self.format_term(body, term)?;
        }

        Ok(())
    }

    fn local_kind(&mut self, body: &mir::Body, mir::Local(i): mir::Local) -> LocalKind {
        match () {
            // captures
            _ if i < body.captures => LocalKind::Capture(i, i),
            // arguments
            _ if i < (body.captures + body.arguments) => LocalKind::Argument(i, i - body.captures),
            // locals
            _ => LocalKind::Local(i, i - body.captures - body.arguments),
        }
    }

    fn format_stmt(&mut self, body: &mir::Body, stmt: &mir::Stmt) -> Result {
        match stmt {
            mir::Stmt::Drop(local) => {
                write!(self.writer, "drop ")?;
                let local_kind = self.local_kind(body, *local);
                write!(self.writer, "{}", local_kind.format_relative())
            }
            mir::Stmt::Assign(place, value) => {
                if place.is_mutable {
                    write!(self.writer, "mut ")?;
                }
                self.format_place(body, place)?;
                write!(self.writer, " = ")?;
                self.format_value(body, value)
            }
            mir::Stmt::Loop(block) => {
                write!(self.writer, "loop")?;
                self.indent(|f| f.format_block(body, block))
            }
            mir::Stmt::Match {
                target,
                cases,
                default,
            } => {
                write!(self.writer, "match ")?;
                self.format_place(body, target)?;
                write!(self.writer, " {{")?;

                self.indent(|f| {
                    for case in cases {
                        f.write_indent()?;
                        write!(f.writer, "| {} ", f.program.types.format(&case.ty))?;
                        let local_kind = f.local_kind(body, case.local);
                        write!(f.writer, "as {}", local_kind.format_relative())?;
                        write!(f.writer, " => ")?;
                        f.indent(|g| g.format_block(body, &case.block))?;
                    }

                    f.write_indent()?;
                    write!(f.writer, "_ => ")?;
                    f.indent(|g| g.format_block(body, default))
                })?;

                self.write_indents(1)?;
                write!(self.writer, "}}")
            }
        }
    }

    fn format_term(&mut self, body: &mir::Body, term: &mir::Term) -> Result {
        match term {
            mir::Term::Return(value) => {
                write!(self.writer, "return ")?;
                self.format_value(body, value)
            }
            mir::Term::Break => {
                write!(self.writer, "break")
            }
        }
    }

    fn format_place(&mut self, body: &mir::Body, place: &mir::Place) -> Result {
        let local = place.local;
        let local_kind = self.local_kind(body, local);

        write!(self.writer, "{}", local_kind.format_relative())?;

        for (proj, _) in &place.proj {
            match proj {
                mir::Proj::Field(name) => {
                    write!(self.writer, ".{}", name)?;
                }
                mir::Proj::Tuple(index) => {
                    write!(self.writer, ".{}", index)?;
                }
                mir::Proj::Index(operand) => {
                    write!(self.writer, "[")?;
                    self.format_operand(body, operand)?;
                    write!(self.writer, "]")?;
                }
                mir::Proj::Deref => {
                    todo!()
                }
            };
        }

        Ok(())
    }

    fn format_operand(&mut self, body: &mir::Body, operand: &mir::Operand) -> Result {
        match operand {
            mir::Operand::Load(place) => {
                write!(self.writer, "load ")?;
                self.format_place(body, place)
            }
            mir::Operand::Const(r#const, _) => {
                write!(self.writer, "const(",)?;
                match r#const {
                    Const::None => write!(self.writer, "none")?,
                    Const::Int(v) => write!(self.writer, "{}", v)?,
                    Const::Float(v) => write!(self.writer, "{}", v)?,
                    Const::String(v) => write!(self.writer, "{:?}", v)?,
                };

                write!(self.writer, ")")
            }
        }
    }

    fn format_value(&mut self, body: &mir::Body, value: &mir::Value) -> Result {
        match value {
            mir::Value::Use(operand) => {
                write!(self.writer, "use ")?;
                self.format_operand(body, operand)
            }
            mir::Value::Tuple(operands) => {
                write!(self.writer, "tuple(")?;
                for (i, operand) in operands.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    self.format_operand(body, operand)?;
                }
                write!(self.writer, ")")
            }
            mir::Value::Record(record) => {
                write!(self.writer, "record(")?;
                for (i, (name, operand)) in record.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    write!(self.writer, "{}: ", name)?;
                    self.format_operand(body, operand)?;
                }
                write!(self.writer, ")")
            }
            mir::Value::Promote { .. } => todo!(),
            mir::Value::Coerce { .. } => todo!(),
            mir::Value::Call(place, operand) => {
                self.format_place(body, place)?;
                write!(self.writer, " = call ")?;
                self.format_operand(body, operand)
            }
            mir::Value::Binary(op, lhs, rhs) => {
                self.format_operand(body, lhs)?;
                write!(self.writer, " {} ", op)?;
                self.format_operand(body, rhs)
            }
            mir::Value::Unary(op, lhs) => {
                write!(self.writer, "{}", op)?;
                self.format_operand(body, lhs)
            }
            mir::Value::Closure {
                body: bid,
                generics,
                captures,
            } => {
                write!(self.writer, "closure({}) ", bid.0)?;
                write!(self.writer, "generics(")?;
                for (i, ty) in generics.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    write!(self.writer, "{}", self.program.types.format(ty))?;
                }
                write!(self.writer, ") ")?;
                write!(self.writer, "captures(")?;
                for (i, operand) in captures.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    self.format_operand(body, operand)?;
                }
                write!(self.writer, ")")
            }
        }
    }
}
