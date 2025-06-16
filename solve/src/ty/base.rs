use std::fmt;

use super::Type;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// A record type.
pub struct Record {
    /// The fields of the record.
    pub fields: Vec<(&'static str, Type)>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// A tuple type.
pub struct Tuple {
    /// The fields of the tuple.
    pub fields: Vec<Type>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// An array type.
pub struct Array {
    /// The element type of the array.
    pub element: Type,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// A function type.
pub struct Function {
    /// The input type of the function.
    pub input: Type,

    /// The output type of the function.
    pub output: Type,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Base {
    None,

    Record(Record),

    Tuple(Tuple),

    Array(Array),

    Function(Function),
}

impl Base {
    /// Returns `true` if the base is [`None`].
    ///
    /// [`None`]: Base::None
    #[must_use]
    pub fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }

    pub fn complexity(&self) -> usize {
        match self {
            Base::None => 0,
            Base::Record(record) => record.fields.iter().map(|(_, ty)| ty.complexity()).sum(),
            Base::Tuple(tuple) => tuple.fields.iter().map(Type::complexity).sum(),
            Base::Array(array) => array.element.complexity(),
            Base::Function(function) => function.input.complexity() + function.output.complexity(),
        }
    }

    pub fn is_subtype_heuristic(&self, other: &Self) -> bool {
        match (self, other) {
            (Base::Record(this), Base::Record(other)) => {
                for (name, self_ty) in &this.fields {
                    if let Some((_, other_ty)) = other.fields.iter().find(|(n, _)| name == n) {
                        if !self_ty.is_subtype_heuristic(other_ty) {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }

                true
            }

            (Base::Tuple(this), Base::Tuple(other)) => {
                if this.fields.len() != other.fields.len() {
                    return false;
                }

                for (self_field, other_field) in this.fields.iter().zip(&other.fields) {
                    if !self_field.is_subtype_heuristic(other_field) {
                        return false;
                    }
                }

                true
            }

            (Base::Array(this), Base::Array(other)) => {
                this.element.is_subtype_heuristic(&other.element)
            }

            (Base::Function(this), Base::Function(other)) => {
                this.input.is_subtype_heuristic(&other.input)
                    && other.output.is_subtype_heuristic(&this.output)
            }

            (Base::None, Base::None) => true,

            (_, _) => false,
        }
    }

    #[must_use]
    pub fn inter(self, other: Self) -> Self {
        match (self, other) {
            (Base::None, some) | (some, Base::None) => some,

            (Base::Record(mut this), Base::Record(mut other)) => {
                for (name, ty) in this.fields.iter_mut() {
                    if let Some(i) = other.fields.iter().position(|(n, _)| name == n) {
                        let (_, other_ty) = other.fields.remove(i);
                        *ty = ty.take().inter(other_ty);
                    }
                }

                this.fields.extend(other.fields);

                Base::Record(this)
            }

            (Base::Tuple(mut this), Base::Tuple(other)) => {
                if this.fields.len() != other.fields.len() {
                    return Base::None;
                }

                for (field, other_field) in this.fields.iter_mut().zip(other.fields) {
                    *field = field.take().inter(other_field);
                }

                Base::Tuple(this)
            }

            (Base::Array(this), Base::Array(other)) => {
                let element = this.element.inter(other.element);
                Base::Array(Array { element })
            }

            (Base::Function(this), Base::Function(other)) => {
                // function types are contravariant over input and covariant over output
                Base::Function(Function {
                    input: this.input.union(other.input),
                    output: this.output.inter(other.output),
                })
            }

            (_, _) => Base::None,
        }
    }

    #[must_use]
    pub fn union(self, other: Self) -> Self {
        match (self, other) {
            (Base::None, some) | (some, Base::None) => some,

            (Base::Record(mut this), Base::Record(mut other)) => {
                this.fields.retain_mut(|(name, ty)| {
                    match other.fields.iter().position(|(n, _)| name == n) {
                        Some(i) => {
                            let (_, other_ty) = other.fields.remove(i);
                            *ty = ty.take().union(other_ty);
                            true
                        }
                        None => false,
                    }
                });

                Base::Record(this)
            }

            (Base::Tuple(mut this), Base::Tuple(other)) => {
                if this.fields.len() != other.fields.len() {
                    return Base::None;
                }

                for (self_field, other_field) in this.fields.iter_mut().zip(other.fields) {
                    *self_field = self_field.take().union(other_field);
                }

                Base::Tuple(this)
            }

            (Base::Array(this), Base::Array(other)) => {
                let element = this.element.union(other.element);
                Base::Array(Array { element })
            }

            (Base::Function(this), Base::Function(other)) => {
                // function types are contravariant over input and covariant over output
                Base::Function(Function {
                    input: this.input.inter(other.input),
                    output: this.output.union(other.output),
                })
            }

            (_, _) => Base::None,
        }
    }
}

impl fmt::Display for Record {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fields = self
            .fields
            .iter()
            .map(|(name, ty)| format!("{name}: {ty}"))
            .collect::<Vec<_>>()
            .join("; ");

        write!(f, "{{ {fields} }}")
    }
}

impl fmt::Display for Tuple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fields = self
            .fields
            .iter()
            .map(Type::to_string)
            .collect::<Vec<_>>()
            .join(", ");

        write!(f, "({fields})")
    }
}

impl fmt::Display for Array {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]", self.element)
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} -> {})", self.input, self.output)
    }
}

impl fmt::Display for Base {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Base::None => write!(f, ""),
            Base::Record(record) => write!(f, "{}", record),
            Base::Tuple(tuple) => write!(f, "{}", tuple),
            Base::Array(array) => write!(f, "{}", array),
            Base::Function(function) => write!(f, "{}", function),
        }
    }
}
