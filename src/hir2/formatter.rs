use crate::hir2;
use solve::Type;

const INDENT_SIZE: usize = 4;

pub struct Formatter<'a, W: std::io::Write> {
    indent: usize,
    writer: W,
    program: &'a hir2::Program,
}

type Result = std::io::Result<()>;

impl<'a, W: std::io::Write> Formatter<'a, W> {
    pub fn new(writer: W, program: &'a hir2::Program) -> Self {
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

    fn write_to_string(
        &self,
        f: impl FnOnce(&mut Formatter<'a, std::io::Cursor<Vec<u8>>>) -> Result,
    ) -> std::io::Result<String> {
        let cursor = std::io::Cursor::new(Vec::new());

        let mut formatter = Formatter {
            indent: self.indent,
            writer: cursor,
            program: self.program,
        };

        f(&mut formatter)?;
        let vec = formatter.writer.into_inner();
        let string = String::from_utf8(vec).unwrap();
        Ok(string)
    }

    pub fn format(&mut self) -> Result {
        for (module_id, module) in self.program.modules.iter() {
            let default_name = format!("module{}", module_id.0);

            let name = module
                .name
                .as_ref()
                .map_or(default_name.as_str(), |name| name.as_str());

            writeln!(self.writer, "module {} {{\n", name)?;
            self.indent(|f| f.format_module(module))?;
            self.write_indents(1)?;
            writeln!(self.writer, "}}\n")?;
        }

        Ok(())
    }

    fn format_module(&mut self, module: &hir2::Module) -> Result {
        for (name, (body_id, vis)) in module.bodies.iter() {
            let body = &self.program[*body_id];
            self.write_indent()?;
            self.format_vis(vis)?;
            write!(self.writer, "fn {} ", name)?;

            for arg in &body.input {
                todo!()
            }
        }

        Ok(())
    }

    fn format_expr(&mut self, body: &hir2::Expr) -> Result {
        todo!();
    }

    fn format_vis(&mut self, vis: &hir2::Vis) -> Result {
        match vis {
            hir2::Vis::Pub => write!(self.writer, "pub"),
            hir2::Vis::Priv => Ok(()),
        }
    }

    fn format_binding(&mut self, binding: &hir2::Binding) -> Result {
        todo!()
    }

    fn format_ty(&mut self, ty: &Type) -> Result {
        todo!()
    }
}
