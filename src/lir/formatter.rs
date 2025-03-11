use std::io::{self, Write};

use super::{Bid, Program, Tid, Ty};

pub struct Formatter<W> {
    writer: W,
}

impl<W> Formatter<W>
where
    W: Write,
{
    pub fn new(writer: W) -> Self {
        Self { writer }
    }

    pub fn format(&mut self, program: &Program) -> io::Result<()> {
        write!(self.writer, "Types===========")?;
        self.writeln()?;
        for i in 0..program.types.len() {
            self.format_tid(program, Tid(i as u32), "  ", "  ")?;
            self.writeln()?;
        }

        for i in 0..program.bodies.len() {
            self.format_body(
                |fmt| {
                    write!(fmt.writer, "  ")?;
                    Ok(())
                },
                program,
                Bid(i as u32),
            )?;
        }

        Ok(())
    }

    fn writeln(&mut self) -> io::Result<()> {
        writeln!(self.writer)?;
        Ok(())
    }

    fn format_tid(
        &mut self,
        program: &Program,
        Tid(tid): Tid,
        indent_first: &str,
        indent_other: &str,
    ) -> io::Result<()> {
        write!(self.writer, "{}", indent_first)?;
        write!(self.writer, "{}: ", tid)?;
        match program.types[Tid(tid)].clone() {
            Ty::Int => {
                write!(self.writer, "Int")?;
                self.writeln()?;
            }
            Ty::Float => {
                write!(self.writer, "Float")?;
                self.writeln()?;
            }
            Ty::Str => {
                write!(self.writer, "Str")?;
                self.writeln()?;
            }
            Ty::False => {
                write!(self.writer, "False")?;
                self.writeln()?;
            }
            Ty::True => {
                write!(self.writer, "True")?;
                self.writeln()?;
            }
            Ty::Bool => {
                write!(self.writer, "Bool")?;
                self.writeln()?;
            }
            Ty::Never => {
                write!(self.writer, "Never")?;
                self.writeln()?;
            }
            Ty::Empty => {
                write!(self.writer, "Empty")?;
                self.writeln()?;
            }
            Ty::Ptr(Tid(id)) => todo!(),
            Ty::Ref(Tid(id)) => {
                write!(self.writer, "Ref({})", tid)?;
                self.writeln()?;

                self.format_tid(
                    program,
                    Tid(id),
                    &[indent_other, "╚═"].join(""),
                    &[indent_other, "  "].join(""),
                )?;
            }
            Ty::List(Tid(id)) => {
                write!(self.writer, "List({})", tid)?;
                self.writeln()?;

                self.format_tid(
                    program,
                    Tid(id),
                    &[indent_other, "╚═"].join(""),
                    &[indent_other, "  "].join(""),
                )?;
            }
            Ty::Func(Tid(arg), Tid(ret)) => {
                write!(self.writer, "Func({} -> {})", arg, ret)?;
                self.writeln()?;

                self.format_tid(
                    program,
                    Tid(arg),
                    &[indent_other, "╠═"].join(""),
                    &[indent_other, "║ "].join(""),
                )?;

                self.format_tid(
                    program,
                    Tid(ret),
                    &[indent_other, "╚═"].join(""),
                    &[indent_other, "  "].join(""),
                )?;
            }
            Ty::Tuple(tids) => {
                write!(self.writer, "Tuple(")?;
                for (i, Tid(id)) in tids.iter().enumerate() {
                    if i == 0 {
                        write!(self.writer, "{}", id)?;
                    } else {
                        write!(self.writer, ", {}", id)?;
                    }
                }
                write!(self.writer, ")")?;
                self.writeln()?;
                for (i, Tid(id)) in tids.iter().enumerate() {
                    if i + 1 == tids.len() {
                        self.format_tid(
                            program,
                            Tid(id.clone()),
                            &[indent_other, "╚═"].join(""),
                            &[indent_other, "  "].join(""),
                        )?;
                    } else {
                        self.format_tid(
                            program,
                            Tid(id.clone()),
                            &[indent_other, "╠═"].join(""),
                            &[indent_other, "║ "].join(""),
                        )?;
                    }
                }
            }
            Ty::Record(items) => {
                write!(self.writer, "Record(")?;
                for (i, (name, Tid(id))) in items.iter().enumerate() {
                    if i == 0 {
                        write!(self.writer, "{}: {}", name, id)?;
                    } else {
                        write!(self.writer, ", {}: {}", name, id)?;
                    }
                }
                write!(self.writer, ")")?;
                self.writeln()?;
                for (i, (name, Tid(id))) in items.iter().enumerate() {
                    if i + 1 == items.len() {
                        self.format_tid(
                            program,
                            Tid(id.clone()),
                            &[indent_other, "╚═"].join(""),
                            &[indent_other, "  "].join(""),
                        )?;
                    } else {
                        self.format_tid(
                            program,
                            Tid(id.clone()),
                            &[indent_other, "╠═"].join(""),
                            &[indent_other, "║ "].join(""),
                        )?;
                    }
                }
            }
            Ty::Union(tids) => {
                write!(self.writer, "Union(")?;
                for (i, Tid(id)) in tids.iter().enumerate() {
                    if i == 0 {
                        write!(self.writer, "{}", id)?;
                    } else {
                        write!(self.writer, " | {}", id)?;
                    }
                }
                write!(self.writer, ")")?;
                self.writeln()?;
                for (i, Tid(id)) in tids.iter().enumerate() {
                    if i + 1 == tids.len() {
                        self.format_tid(
                            program,
                            Tid(id.clone()),
                            &[indent_other, "╚═"].join(""),
                            &[indent_other, "  "].join(""),
                        )?;
                    } else {
                        self.format_tid(
                            program,
                            Tid(id.clone()),
                            &[indent_other, "╠═"].join(""),
                            &[indent_other, "║ "].join(""),
                        )?;
                    }
                }
            }
            Ty::Named(_, tid) => todo!(),
        };

        // TODO ::
        Ok(())
    }

    fn format_type(
        &mut self,
        f: impl Fn(&mut Self) -> io::Result<()>,
        program: &Program,
        ty: &Ty,
    ) -> io::Result<()> {
        todo!()
    }

    fn format_body(
        &mut self,
        f: impl Fn(&mut Self) -> io::Result<()>,
        program: &Program,
        bid: Bid,
    ) -> io::Result<()> {
        // TODO ::
        Ok(())
    }
}
