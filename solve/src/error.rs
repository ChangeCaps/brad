use std::{error, fmt};

#[derive(Clone, Debug)]
pub enum Error<T> {
    Unsatisfiable { span: T },
}

impl<T: fmt::Display> fmt::Display for Error<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Unsatisfiable { span } => write!(f, "Unsatisfiable: {}", span),
        }
    }
}

impl<T> error::Error for Error<T> where T: fmt::Display + fmt::Debug {}
