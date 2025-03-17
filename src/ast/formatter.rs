use crate::ast::{Binding, Decl, Expr, Func, Generics, Module, Pattern, Ty};
use std::{io, io::Write};

const INDENT_SIZE: usize = 4;

pub struct Formatter<W: Write> {
    indent: usize,
    writer: W,
}

type Result = io::Result<()>;

impl<W: Write> Formatter<W> {
    pub fn new(writer: W) -> Self {
        Formatter { indent: 0, writer }
    }

    fn indent(&mut self, f: impl FnOnce(&mut Self) -> Result) -> Result {
        self.indent += 1;
        let result = f(self);
        self.indent -= 1;
        result
    }

    fn write_indent(&mut self) -> Result {
        write!(self.writer, "\n{}", " ".repeat(INDENT_SIZE * self.indent))
    }

    pub fn format_module(&mut self, module: &Module) -> Result {
        for decl in &module.decls {
            self.format_decl(decl)?;
        }

        Ok(())
    }

    pub fn format_decl(&mut self, decl: &Decl) -> Result {
        match decl {
            Decl::Func(func_decl) => {
                writeln!(self.writer)?;

                if func_decl.is_extern {
                    write!(self.writer, "extern ")?;
                }

                write!(self.writer, "fn")?;
                write!(self.writer, " {}", func_decl.name)?;

                if let Some(generics) = &func_decl.generics {
                    self.format_generics(generics)?;
                }

                for arg in &func_decl.args {
                    match &arg.ty {
                        Some(ty) => {
                            write!(self.writer, " (")?;
                            self.format_binding(&arg.binding)?;
                            write!(self.writer, ": ")?;
                            self.format_ty(ty)?;
                            write!(self.writer, ")")?
                        }
                        None => self.format_binding(&arg.binding)?,
                    };
                }

                if let Some(ty) = &func_decl.output {
                    write!(self.writer, " -> ")?;
                    self.format_ty(ty)?;
                };

                if let Func {
                    body: Some(body),
                    is_extern: false,
                    ..
                } = func_decl
                {
                    self.format_expr(body)?
                };

                writeln!(self.writer)
            }
            Decl::Type(type_decl) => {
                write!(self.writer, "type {}", type_decl.name)?;

                if let Some(generics) = &type_decl.generics {
                    self.format_generics(generics)?;
                }

                if let Some(ty) = &type_decl.ty {
                    write!(self.writer, " = ")?;
                    self.format_ty(ty)?;
                }

                Ok(())
            }
            Decl::Alias(alias_decl) => {
                write!(self.writer, "alias {}", alias_decl.name)?;

                if let Some(generics) = &alias_decl.generics {
                    self.format_generics(generics)?;
                }

                write!(self.writer, " = ")?;
                self.format_ty(&alias_decl.ty)?;
                writeln!(self.writer)
            }
            Decl::Import(import_decl) => writeln!(self.writer, "import {}", import_decl.path),
        }
    }

    pub fn format_expr(&mut self, expr: &Expr) -> Result {
        match expr {
            Expr::Literal(literal) => write!(self.writer, "{}", literal),
            Expr::List(list_expr) => {
                write!(self.writer, "[")?;
                for (i, expr) in list_expr.items.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    self.format_expr(expr)?;
                }
                write!(self.writer, "]")
            }
            Expr::Record(record_expr) => {
                write!(self.writer, "{{")?;
                for (i, field) in record_expr.fields.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, "; ")?;
                    }
                    write!(self.writer, "{}: ", field.name)?;
                    self.format_expr(&field.value)?;
                }
                write!(self.writer, "}}")
            }
            Expr::Tuple(tuple_expr) => {
                write!(self.writer, "(")?;
                for (i, expr) in tuple_expr.items.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    self.format_expr(expr)?;
                }
                write!(self.writer, ")")
            }
            Expr::Path(path) => write!(self.writer, "{}", path),
            Expr::Index(index_expr) => {
                self.format_expr(&index_expr.target)?;
                write!(self.writer, "[")?;
                self.format_expr(&index_expr.index)?;
                write!(self.writer, "]")
            }
            Expr::Field(field_expr) => {
                self.format_expr(&field_expr.target)?;
                write!(self.writer, ".{}", field_expr.name)
            }
            Expr::Unary(unary_expr) => {
                write!(self.writer, "{}", unary_expr.op)?;
                self.format_expr(&unary_expr.expr)
            }
            Expr::Binary(binary_expr) => {
                self.format_expr(&binary_expr.lhs)?;
                write!(self.writer, " {} ", binary_expr.op)?;
                self.format_expr(&binary_expr.rhs)
            }
            Expr::Call(call_expr) => {
                write!(self.writer, "(")?;
                self.format_expr(&call_expr.target)?;

                write!(self.writer, " (")?;
                self.format_expr(&call_expr.input)?;
                write!(self.writer, "))")
            }
            Expr::Assign(assign_expr) => {
                self.format_expr(&assign_expr.target)?;
                write!(self.writer, " = ")?;
                self.format_expr(&assign_expr.value)
            }
            Expr::Ref(ref_expr) => {
                write!(self.writer, "ref ")?;
                self.format_expr(&ref_expr.target)
            }
            Expr::Match(match_expr) => {
                write!(self.writer, "match ")?;
                self.format_expr(&match_expr.target)?;
                for arm in &match_expr.arms {
                    self.write_indent()?;
                    write!(self.writer, "| ")?;
                    self.format_pattern(&arm.pattern)?;
                    write!(self.writer, " => ")?;
                    self.format_expr(&arm.expr)?;
                }
                Ok(())
            }
            Expr::Loop(loop_expr) => {
                write!(self.writer, "loop ")?;
                self.format_expr(&loop_expr.body)
            }
            Expr::Break(break_expr) => {
                write!(self.writer, "break")?;
                if let Some(expr) = &break_expr.value {
                    write!(self.writer, " ")?;
                    self.format_expr(expr)?;
                }

                Ok(())
            }
            Expr::Let(let_expr) => {
                write!(self.writer, "let ")?;
                self.format_binding(&let_expr.binding)?;
                write!(self.writer, " = ")?;
                self.format_expr(&let_expr.value)
            }
            Expr::Block(block_expr) => {
                write!(self.writer, " {{")?;
                self.indent(|f| {
                    for expr in &block_expr.exprs {
                        f.write_indent()?;
                        f.format_expr(expr)?;
                    }

                    Ok(())
                })?;

                if !block_expr.exprs.is_empty() {
                    self.write_indent()?;
                }

                write!(self.writer, "}}")
            }
        }
    }

    pub fn format_binding(&mut self, binding: &Binding) -> Result {
        match binding {
            Binding::Wild { .. } => write!(self.writer, "_"),
            Binding::Bind { name, mutable, .. } => write!(
                self.writer,
                "{}{}",
                if *mutable { "mut " } else { "" },
                name
            ),
            Binding::Tuple { bindings, .. } => {
                write!(self.writer, "(")?;
                for (i, binding) in bindings.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    self.format_binding(binding)?;
                }
                write!(self.writer, ")")
            }
        }
    }

    pub fn format_ty(&mut self, ty: &Ty) -> Result {
        match ty {
            Ty::Wild(_) => write!(self.writer, "_"),
            Ty::Int(_) => write!(self.writer, "int"),
            Ty::Float(_) => write!(self.writer, "float"),
            Ty::Str(_) => write!(self.writer, "str"),
            Ty::True(_) => write!(self.writer, "true"),
            Ty::False(_) => write!(self.writer, "false"),
            Ty::None(_) => write!(self.writer, "none"),
            Ty::Never(_) => write!(self.writer, "!"),
            Ty::Generic(generic) => write!(self.writer, "'{}", generic.name),
            Ty::Path(path) => write!(self.writer, "{}", path),
            Ty::Ref { ty, .. } => {
                write!(self.writer, "ref ")?;
                self.format_ty(ty)
            }
            Ty::Func { input, output, .. } => {
                self.format_ty(input)?;
                write!(self.writer, " -> ")?;
                self.format_ty(output)
            }
            Ty::List { ty, .. } => {
                write!(self.writer, "[")?;
                self.format_ty(ty)?;
                write!(self.writer, "]")
            }
            Ty::Tuple { tys, .. } => {
                for (i, ty) in tys.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, " * ")?;
                    }
                    self.format_ty(ty)?;
                }

                Ok(())
            }
            Ty::Union { tys, .. } => {
                for (i, ty) in tys.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, " | ")?;
                    }
                    self.format_ty(ty)?;
                }
                Ok(())
            }
            Ty::Record { fields, .. } => {
                write!(self.writer, "{{")?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, "; ")?;
                    }
                    write!(self.writer, "{}: ", field.name)?;
                    self.format_ty(&field.ty)?;
                }
                write!(self.writer, "}}")
            }
        }
    }

    fn format_generics(&mut self, generics: &Generics) -> Result {
        write!(self.writer, "<")?;

        for (i, generic) in generics.generics.iter().enumerate() {
            if i > 0 {
                write!(self.writer, ", ")?;
            }
            write!(self.writer, "{}", generic.name)?;
        }

        write!(self.writer, ">")?;

        Ok(())
    }

    fn format_pattern(&mut self, pattern: &Pattern) -> Result {
        match pattern {
            Pattern::Ty { ty, binding, .. } => {
                self.format_ty(ty)?;

                if let Some(binding) = binding {
                    write!(self.writer, " ")?;
                    self.format_binding(binding)?;
                }

                Ok(())
            }
        }
    }
}
