use std::io::{self, Write};

use crate::hir2 as hir;

const LUA_PRELUDE: &str = include_str!("prelude.lua");

pub fn codegen(code: &mut impl Write, hir: &hir::Program) -> io::Result<()> {
    code.write_all(LUA_PRELUDE.as_bytes())?;

    for (_, body) in hir.bodies.iter() {
        codegen_body(code, hir, body).unwrap();
    }

    Ok(())
}

fn codegen_body(code: &mut impl Write, hir: &hir::Program, body: &hir::Body) -> io::Result<()> {
    writeln!(code, "M['{}'] = function()", body.name)?;

    for i in 0..body.input.len() {
        writeln!(code, "  return function(a{i})")?;
    }

    let mut codegen = Codegen {
        hir,
        body,
        code,
        level: 0,
        loop_tmp: None,
        next_tmp: 0,
    };

    match body.expr {
        Some(ref expr) => {
            for i in 0..body.locals.len() {
                writeln!(codegen.code, "  local l{i}")?;
            }

            for (i, input) in body.input.iter().enumerate() {
                codegen.binding(&input.binding, &format!("a{i}"))?;
            }

            let value = codegen.expr(expr)?;

            writeln!(code, "  return {}", value)?;
        }

        None => {
            assert!(body.is_extern);

            let function = if let Some(intrinsic) = body.attrs.find_value("intrinsic") {
                match intrinsic {
                    "debug::format" => "brad_debug_format",
                    _ => panic!("unsupported intrinsic: {}", intrinsic),
                }
            } else if let Some(link) = body.attrs.find_value("link") {
                link
            } else {
                panic!("extern function without link attribute")
            };

            write!(codegen.code, "  return {function}(")?;

            for i in 0..body.input.len() {
                if i > 0 {
                    write!(codegen.code, ", ")?;
                }

                write!(codegen.code, "a{i}")?;
            }

            writeln!(codegen.code, ")")?;
        }
    };

    for _ in 0..body.input.len() {
        writeln!(code, "  end")?;
    }

    writeln!(code, "end")?;

    Ok(())
}

struct Codegen<'a, W> {
    hir: &'a hir::Program,
    body: &'a hir::Body,

    code: &'a mut W,

    level: usize,

    loop_tmp: Option<String>,
    next_tmp: usize,
}

impl<W: Write> Codegen<'_, W> {
    fn tmp(&mut self) -> String {
        let tmp = self.next_tmp;
        self.next_tmp += 1;

        format!("t{}", tmp)
    }

    fn binding(&mut self, binding: &hir::Binding, value: &str) -> io::Result<()> {
        match binding {
            hir::Binding::Wild { .. } => {}

            hir::Binding::Bind { local, .. } => writeln!(
                self.code,
                "  {}{} = {}",
                "l".repeat(self.level + 1),
                local.0,
                value,
            )?,

            hir::Binding::Tuple { bindings, .. } => {
                let tmp = self.tmp();

                writeln!(self.code, "  local {} = {}", tmp, value)?;

                for (i, binding) in bindings.iter().enumerate() {
                    let field = format!("{}[{}]", tmp, i + 1);

                    self.binding(binding, &field)?;
                }
            }
        }

        Ok(())
    }

    fn expr(&mut self, expr: &hir::Expr) -> io::Result<String> {
        Ok(match expr.kind {
            hir::ExprKind::Int(value) => format!("make_int({value})"),
            hir::ExprKind::Float(value) => format!("make_float({value})"),
            hir::ExprKind::ZeroSize(tag) => format!("make_tag('{}')", tag.name),
            hir::ExprKind::Local(id) => format!("{}{}", "l".repeat(self.level + 1), id.0),

            hir::ExprKind::String(value) => {
                let value = value
                    .replace("\\", "\\\\")
                    .replace("\n", "\\n")
                    .replace("\t", "\\t")
                    .replace("\r", "\\r")
                    .replace("\"", "\\\"");

                format!("make_str('{value}')")
            }

            hir::ExprKind::Tag(tag, ref value) => {
                let value = self.expr(value)?;
                format!("add_tag('{}', {})", tag.name, value)
            }

            hir::ExprKind::Func(body) => {
                format!("M['{}']()", self.hir[body].name)
            }

            hir::ExprKind::Array(ref items) => {
                let items = items
                    .iter()
                    .map(|item| self.expr(item))
                    .collect::<io::Result<Vec<_>>>()?;

                format!("make_array({})", items.join(", "))
            }

            hir::ExprKind::Tuple(ref items) => {
                let items = items
                    .iter()
                    .map(|item| self.expr(item))
                    .collect::<io::Result<Vec<_>>>()?;

                format!("make_tuple({})", items.join(", "))
            }

            hir::ExprKind::Record(ref fields) => {
                let fields = fields
                    .iter()
                    .map(|field| {
                        let value = self.expr(&field.value)?;
                        Ok(format!("['{}'] = {}", field.name, value))
                    })
                    .collect::<Result<Vec<_>, io::Error>>()?;

                format!("make_record({{ {} }})", fields.join(", "))
            }

            hir::ExprKind::Index(ref target, ref index) => {
                let target = self.expr(target)?;
                let index = self.expr(index)?;

                format!("{}[{}]", target, index)
            }

            hir::ExprKind::Field(ref target, field) => {
                let target = self.expr(target)?;

                format!("{}['{}']", target, field)
            }

            hir::ExprKind::Unary(op, ref target) => {
                let target = self.expr(target)?;

                match op {
                    hir::UnaryOp::Neg | hir::UnaryOp::BitNot => {
                        let op = match op {
                            hir::UnaryOp::Neg => "-",
                            hir::UnaryOp::BitNot => "~",
                            _ => unreachable!(),
                        };

                        format!("make_int({}({}.value))", op, target)
                    }

                    hir::UnaryOp::Not => format!("make_bool(not has_tag({}, 'true'))", target),

                    hir::UnaryOp::Deref => todo!(),
                }
            }

            hir::ExprKind::Binary(op, ref lhs, ref rhs) => {
                let lhs = self.expr(lhs)?;
                let rhs = self.expr(rhs)?;

                match op {
                    hir::BinaryOp::Add
                    | hir::BinaryOp::Sub
                    | hir::BinaryOp::Mul
                    | hir::BinaryOp::Div
                    | hir::BinaryOp::Mod
                    | hir::BinaryOp::BitAnd
                    | hir::BinaryOp::BitOr
                    | hir::BinaryOp::BitXor
                    | hir::BinaryOp::Shl
                    | hir::BinaryOp::Shr => {
                        let op = match op {
                            hir::BinaryOp::Add => "+",
                            hir::BinaryOp::Sub => "-",
                            hir::BinaryOp::Mul => "*",
                            hir::BinaryOp::Div => "/",
                            hir::BinaryOp::Mod => "%",
                            hir::BinaryOp::BitAnd => "&",
                            hir::BinaryOp::BitOr => "|",
                            hir::BinaryOp::BitXor => "~",
                            hir::BinaryOp::Shl => "<<",
                            hir::BinaryOp::Shr => ">>",
                            _ => unreachable!(),
                        };

                        format!("make_int(({}.value) {} ({}.value))", lhs, op, rhs)
                    }

                    hir::BinaryOp::Lt
                    | hir::BinaryOp::Le
                    | hir::BinaryOp::Gt
                    | hir::BinaryOp::Ge => {
                        let op = match op {
                            hir::BinaryOp::Lt => "<",
                            hir::BinaryOp::Le => "<=",
                            hir::BinaryOp::Gt => ">",
                            hir::BinaryOp::Ge => ">=",
                            _ => unreachable!(),
                        };

                        format!("make_bool(({}.value) {} ({}.value))", lhs, op, rhs)
                    }

                    hir::BinaryOp::And | hir::BinaryOp::Or => {
                        let op = match op {
                            hir::BinaryOp::And => "and",
                            hir::BinaryOp::Or => "or",
                            _ => unreachable!(),
                        };

                        format!(
                            "make_bool(has_tag({}, 'true') {} has_tag({}, 'true'))",
                            lhs, op, rhs
                        )
                    }

                    hir::BinaryOp::Eq | hir::BinaryOp::Ne => {
                        let op = match op {
                            hir::BinaryOp::Eq => "==",
                            hir::BinaryOp::Ne => "~=",
                            _ => unreachable!(),
                        };

                        format!("make_bool({} {} {})", lhs, op, rhs)
                    }
                }
            }

            hir::ExprKind::Call(ref target, ref input) => {
                let target = self.expr(target)?;
                let input = self.expr(input)?;

                format!("{}({})", target, input)
            }

            hir::ExprKind::Lambda {
                ref captures,
                ref args,
                ref locals,
                ref body,
            } => {
                let tmp = self.tmp();
                write!(self.code, "  local {} = function(a0)", tmp)?;

                let mut codegen = Codegen {
                    hir: self.hir,
                    body: self.body,
                    code: self.code,
                    level: self.level + 1,
                    loop_tmp: None,
                    next_tmp: 0,
                };

                for i in 1..args.len() {
                    writeln!(codegen.code, "  return function(a{})", i)?;
                }

                for i in 0..locals.len() {
                    writeln!(
                        codegen.code,
                        "  local {}{}",
                        "l".repeat(codegen.level + 1),
                        i,
                    )?;
                }

                for (i, binding) in args.iter().enumerate() {
                    codegen.binding(binding, &format!("a{}", i))?;
                }

                for capture in captures {
                    writeln!(
                        codegen.code,
                        "  {}{} = {}{}",
                        "l".repeat(codegen.level + 1),
                        capture.inner.index(),
                        "l".repeat(codegen.level),
                        capture.outer.index(),
                    )?;
                }

                let value = codegen.expr(body)?;

                writeln!(self.code, "  return {}", value)?;

                for _ in 0..args.len() {
                    writeln!(self.code, "  end")?;
                }

                tmp
            }

            hir::ExprKind::Assign(ref target, ref value) => {
                let target = self.expr(target)?;
                let value = self.expr(value)?;

                writeln!(self.code, "  {} = {}", target, value)?;
                String::from("make_tag('none')")
            }

            hir::ExprKind::Ref(_) => todo!(),

            hir::ExprKind::Match(ref target, ref body) => {
                let target = self.expr(target)?;

                let trg = self.tmp();
                let res = self.tmp();

                writeln!(self.code, "  local {} = {}", trg, target)?;
                writeln!(self.code, "  local {}", res)?;

                for (i, arm) in body.arms.iter().enumerate() {
                    match arm.pattern {
                        hir::Pattern::Tag {
                            tag, ref binding, ..
                        } => {
                            let if_word = if i == 0 { "if" } else { "elseif" };

                            writeln!(self.code, "  {if_word} has_tag('{}', {trg}) then", tag.name)?;

                            self.binding(binding, &trg)?;
                            let value = self.expr(&arm.body)?;

                            writeln!(self.code, "  {} = {}", res, value)?;
                        }
                    }
                }

                match body.default {
                    Some(ref default) => {
                        let (binding, body) = default.as_ref();

                        writeln!(self.code, "  else")?;

                        self.binding(binding, &trg)?;
                        let value = self.expr(body)?;

                        writeln!(self.code, "  {} = {}", res, value)?;

                        writeln!(self.code, "  end")?;
                    }

                    None => writeln!(self.code, "  end")?,
                }

                res
            }

            hir::ExprKind::Loop(ref body) => {
                let old_loop_tmp = self.loop_tmp.take();
                self.loop_tmp = Some(self.tmp());

                writeln!(self.code, "  while true do")?;

                let value = self.expr(body)?;
                writeln!(self.code, "  local _ = {}", value)?;

                writeln!(self.code, "  end")?;

                self.loop_tmp = old_loop_tmp;

                String::from("make_tag('none')")
            }

            hir::ExprKind::Break(ref value) => {
                match value {
                    Some(value) => {
                        let value = self.expr(value)?;
                        let tmp = self.loop_tmp.as_ref().unwrap();
                        write!(self.code, "  {tmp} = {value}")?;
                        writeln!(self.code, "  break")?;
                    }

                    None => writeln!(self.code, "  break")?,
                }

                String::from("make_tag('none')")
            }

            hir::ExprKind::Let(ref binding, ref value) => {
                let value = self.expr(value)?;
                self.binding(binding, &value)?;

                value
            }

            hir::ExprKind::Block(ref exprs) => {
                let mut output = None;

                for expr in exprs.iter() {
                    if let Some(value) = output.take() {
                        writeln!(self.code, "  local _ = {}", value)?;
                    }

                    output = Some(self.expr(expr)?);
                }

                output.unwrap_or(String::from("make_tag('none')"))
            }
        })
    }
}
