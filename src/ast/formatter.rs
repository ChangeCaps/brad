use crate::ast::{Binding, CallExpr, Decl, Expr, Func, Generics, Module, Pattern, TupleExpr, Ty};
use clap::Args;
use std::{io, io::Write};

const INDENT_SIZE: usize = 4;

pub struct Formatter<W: Write> {
    indent: usize,
    call_depth: usize,
    writer: W,
    opts: FormatterOptions,
}

type Result = io::Result<()>;

#[derive(Args, Default, Clone)]
pub struct FormatterOptions {
    #[arg(short, long, default_value = "false")]
    exclude_types: bool,
}

impl<W: Write> Formatter<W> {
    pub fn new(writer: W, opts: FormatterOptions) -> Self {
        Formatter {
            indent: 0,
            call_depth: 0,
            writer,
            opts,
        }
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

    fn inside_call(&mut self, f: impl FnOnce(&mut Self) -> Result) -> Result {
        self.call_depth += 1;
        let result = f(self);
        self.call_depth -= 1;
        result
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
                    match (&arg.ty, self.opts.exclude_types) {
                        (Some(ty), false) => {
                            write!(self.writer, " (")?;
                            self.format_binding(&arg.binding)?;
                            write!(self.writer, ": ")?;
                            self.format_ty(ty)?;
                            write!(self.writer, ")")?;
                        }
                        _ => {
                            write!(self.writer, " ")?;

                            if let Binding::Bind { mutable: true, .. } = &arg.binding {
                                write!(self.writer, "(")?;
                                self.format_binding(&arg.binding)?;
                                write!(self.writer, ")")?
                            } else {
                                self.format_binding(&arg.binding)?;
                            }
                        }
                    };
                }

                if !self.opts.exclude_types {
                    if let Some(ty) = &func_decl.output {
                        write!(self.writer, " -> ")?;
                        self.format_ty(ty)?;
                    };
                }

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

                writeln!(self.writer)
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
                        write!(self.writer, "; ")?;
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
            Expr::Tuple(TupleExpr { items, .. }) => {
                if !items.is_empty() {
                    write!(self.writer, "(")?;
                }
                for (i, expr) in items.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    self.format_expr(expr)?;
                }
                if !items.is_empty() {
                    write!(self.writer, ")")?;
                }
                Ok(())
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
                self.format_expr(&unary_expr.target)
            }
            Expr::Binary(binary_expr) => {
                self.format_expr(&binary_expr.lhs)?;
                write!(self.writer, " {} ", binary_expr.op)?;
                self.format_expr(&binary_expr.rhs)
            }
            Expr::Call(CallExpr { target, input, .. }) => {
                if self.call_depth > 0 {
                    write!(self.writer, "(")?;
                }

                self.format_expr(target)?;

                write!(self.writer, " ")?;

                self.inside_call(|f| f.format_expr(input))?;

                if self.call_depth > 0 {
                    write!(self.writer, ")")?;
                }

                Ok(())
            }
            Expr::Lambda(_) => todo!(),
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
                    self.format_expr(&arm.body)?;
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

                if !self.opts.exclude_types {
                    if let Some(ty) = &let_expr.ty {
                        write!(self.writer, ": ")?;
                        self.format_ty(ty)?;
                    }
                }

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
                if tys.len() > 1 {
                    write!(self.writer, "(")?;
                }

                for (i, ty) in tys.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, " * ")?;
                    }
                    self.format_ty(ty)?;
                }
                if tys.len() > 1 {
                    write!(self.writer, ")")?;
                }

                Ok(())
            }
            Ty::Union { tys, .. } => {
                if tys.len() > 1 {
                    write!(self.writer, "(")?;
                }

                for (i, ty) in tys.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, " | ")?;
                    }
                    self.format_ty(ty)?;
                }

                if tys.len() > 1 {
                    write!(self.writer, ")")?;
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

        for (i, generic) in generics.params.iter().enumerate() {
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
                    write!(self.writer, " as ")?;
                    self.format_binding(binding)?;
                }

                Ok(())
            }
        }
    }
}
