use std::io::{self, Write};

use colored::{Color, Colorize};

use super::{Diagnostic, Label, Severity, Sources};

pub struct Formatter<'a, W> {
    writer: W,
    sources: &'a Sources,
}

impl<'a, W> Formatter<'a, W>
where
    W: Write,
{
    pub fn new(writer: W, sources: &'a Sources) -> Self {
        Formatter { writer, sources }
    }

    pub fn write(&mut self, diagnostic: &Diagnostic) -> io::Result<()> {
        self.write_header(diagnostic)?;
        self.write_labels(diagnostic)?;

        Ok(())
    }

    fn write_header(&mut self, diagnostic: &Diagnostic) -> io::Result<()> {
        let w = &mut self.writer;

        let color = severity_color(diagnostic.severity);
        let severity = diagnostic.severity.to_string().color(color).bold();

        write!(w, "{}", severity)?;

        if let Some(code) = &diagnostic.code {
            let code = format!("[{}]", code.white()).color(color).bold();
            write!(w, "{}", code)?;
        }

        if let Some(message) = &diagnostic.message {
            writeln!(w, ": {}", message.bold())?;
        } else {
            writeln!(w)?;
        }

        Ok(())
    }

    fn write_labels(&mut self, diagnostic: &Diagnostic) -> io::Result<()> {
        for label in &diagnostic.labels {
            self.write_label(diagnostic, label)?;
        }

        Ok(())
    }

    fn write_label(&mut self, diagnostic: &Diagnostic, label: &Label) -> io::Result<()> {
        let w = &mut self.writer;
        let s = &self.sources[label.span.source];

        let (start_line, start_column) = line_column(&s.content, label.span.start);
        let (end_line, _) = line_column(&s.content, label.span.end);

        let indent = end_line.to_string().len() + 1;

        writeln!(
            w,
            "{}{} {}:{}:{}",
            " ".repeat(indent),
            "-->".blue(),
            s.file,
            start_line,
            start_column,
        )?;

        writeln!(w, "{}{}", " ".repeat(indent + 1), "|".blue())?;

        let mut idx = 0;
        for (i, line) in s.content.lines().enumerate() {
            if i + 1 < start_line {
                idx += line.len() + '\n'.len_utf8();
                continue;
            }

            if i + 1 > end_line {
                break;
            }

            let line_number = (i + 1).to_string();
            write!(
                w,
                " {}{}{}",
                line_number.blue(),
                " ".repeat(indent - line_number.len()),
                "|".blue(),
            )?;

            writeln!(w, " {}", line)?;

            write!(w, "{}{} ", " ".repeat(indent + 1), "|".blue())?;

            let spaces = s.content[idx..label.span.start].chars().count();
            write!(w, "{}", " ".repeat(spaces))?;

            let end = usize::min(label.span.end, idx + line.len());
            let underline = usize::max(1, end - label.span.start);
            let underline = "^".repeat(underline);
            write!(w, "{}", underline.red())?;

            if let Some(message) = &label.message {
                let color = severity_color(diagnostic.severity);
                write!(w, " {}", message.color(color))?;
            }

            writeln!(w)?;

            idx += line.len() + '\n'.len_utf8();
        }

        writeln!(w, "{}{}", " ".repeat(indent + 1), "|".blue())?;

        Ok(())
    }
}

fn severity_color(severity: Severity) -> Color {
    match severity {
        Severity::Error => Color::Red,
        Severity::Warning => Color::Yellow,
        Severity::Note => Color::Blue,
        Severity::Help => Color::Green,
    }
}

fn line_column(s: &str, index: usize) -> (usize, usize) {
    let mut line = 1;
    let mut column = 1;

    for (i, c) in s.chars().enumerate() {
        if i == index {
            break;
        }

        if c == '\n' {
            line += 1;
            column = 1;
        } else {
            column += 1;
        }
    }

    (line, column)
}
