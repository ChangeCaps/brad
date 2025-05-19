use std::fmt;

use super::Type;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Base {
    None,

    /// A record type.
    Record {
        /// The fields of the record.
        fields: Vec<(&'static str, Type)>,
    },

    /// A tuple type.
    Tuple {
        /// The fields of the tuple.
        fields: Vec<Type>,
    },

    /// An array type.
    Array {
        /// The element type of the array.
        element: Type,
    },

    /// A function type.
    Function {
        /// The input type of the function.
        input: Type,

        /// The output type of the function.
        output: Type,
    },
}

impl Base {
    /// Returns `true` if the base is [`None`].
    ///
    /// [`None`]: Base::None
    #[must_use]
    pub fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }

    #[must_use]
    pub fn inter(self, other: Self) -> Self {
        match (self, other) {
            (Base::None, _) | (_, Base::None) => Base::None,

            (
                Base::Record {
                    fields: mut self_fields,
                },
                Base::Record {
                    fields: mut other_fields,
                },
            ) => {
                for (name, self_ty) in self_fields.iter_mut() {
                    if let Some(i) = other_fields.iter().position(|(n, _)| name == n) {
                        let (_, other_ty) = other_fields.swap_remove(i);
                        *self_ty = self_ty.take().inter(other_ty);
                    }
                }

                self_fields.extend(other_fields);

                Base::Record {
                    fields: self_fields,
                }
            }

            (
                Base::Tuple {
                    fields: mut self_fields,
                },
                Base::Tuple {
                    fields: other_fields,
                },
            ) => {
                if self_fields.len() != other_fields.len() {
                    return Base::None;
                }

                for (self_field, other_field) in self_fields.iter_mut().zip(other_fields) {
                    *self_field = self_field.take().inter(other_field);
                }

                Base::Tuple {
                    fields: self_fields,
                }
            }

            (
                Base::Array {
                    element: self_element,
                },
                Base::Array {
                    element: other_element,
                },
            ) => Base::Array {
                element: self_element.inter(other_element),
            },

            (
                Base::Function {
                    input: self_input,
                    output: self_output,
                },
                Base::Function {
                    input: other_input,
                    output: other_output,
                },
            ) => {
                // function types are contravariant over input and covariant over output
                Base::Function {
                    input: self_input.union(other_input),
                    output: self_output.inter(other_output),
                }
            }

            (_, _) => Base::None,
        }
    }

    #[must_use]
    pub fn union(self, other: Self) -> Self {
        match (self, other) {
            (Base::None, some) | (some, Base::None) => some,

            (
                Base::Record {
                    fields: mut self_fields,
                },
                Base::Record {
                    fields: mut other_fields,
                },
            ) => {
                self_fields.retain_mut(|(name, self_ty)| {
                    match other_fields.iter().position(|(n, _)| name == n) {
                        Some(i) => {
                            let (_, other_ty) = other_fields.swap_remove(i);
                            *self_ty = self_ty.take().union(other_ty);
                            true
                        }
                        None => false,
                    }
                });

                Base::Record {
                    fields: self_fields,
                }
            }

            (
                Base::Tuple {
                    fields: mut self_fields,
                },
                Base::Tuple {
                    fields: other_fields,
                },
            ) => {
                if self_fields.len() != other_fields.len() {
                    return Base::None;
                }

                for (self_field, other_field) in self_fields.iter_mut().zip(other_fields) {
                    *self_field = self_field.take().union(other_field);
                }

                Base::Tuple {
                    fields: self_fields,
                }
            }

            (
                Base::Array {
                    element: self_element,
                },
                Base::Array {
                    element: other_element,
                },
            ) => Base::Array {
                element: self_element.union(other_element),
            },

            (
                Base::Function {
                    input: self_input,
                    output: self_output,
                },
                Base::Function {
                    input: other_input,
                    output: other_output,
                },
            ) => {
                // function types are contravariant over input and covariant over output
                Base::Function {
                    input: self_input.inter(other_input),
                    output: self_output.union(other_output),
                }
            }

            (_, _) => Base::None,
        }
    }
}

impl fmt::Display for Base {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Base::None => write!(f, ""),

            Base::Record { fields } => {
                let fields = fields
                    .iter()
                    .map(|(name, ty)| format!("{name}: {ty}"))
                    .collect::<Vec<_>>()
                    .join("; ");

                write!(f, "{{ {fields} }}")
            }

            Base::Tuple { fields } => {
                let fields = fields
                    .iter()
                    .map(|ty| ty.to_string())
                    .collect::<Vec<_>>()
                    .join(" * ");

                write!(f, "{fields}")
            }

            Base::Array { element } => {
                let element = element.to_string();

                write!(f, "[{element}]")
            }

            Base::Function { input, output } => {
                let input = input.to_string();
                let output = output.to_string();

                write!(f, "{input} -> {output}")
            }
        }
    }
}
