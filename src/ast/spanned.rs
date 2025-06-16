use diagnostic::Span;

/// The `Spanned` trait is used to represent types that have a span.
/// It is also used to reset the spans of the contained types to have structurally equivalent ASTs.
pub trait Spanned {
    fn span(&self) -> Span;
    fn reset_spans(&mut self);
}

impl<T: Spanned> Spanned for Option<T> {
    fn span(&self) -> Span {
        self.as_ref().map_or(Span::default(), |t| t.span())
    }

    fn reset_spans(&mut self) {
        if let Some(t) = self {
            t.reset_spans();
        }
    }
}

impl<T: Spanned> Spanned for Vec<T> {
    fn span(&self) -> Span {
        self.iter()
            .fold(Span::default(), |acc, t| acc.join(t.span()))
    }

    fn reset_spans(&mut self) {
        for t in self {
            t.reset_spans();
        }
    }
}

impl<T: Spanned> Spanned for Box<T> {
    fn span(&self) -> Span {
        self.as_ref().span()
    }

    fn reset_spans(&mut self) {
        self.as_mut().reset_spans();
    }
}
