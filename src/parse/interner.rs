use std::collections::HashSet;

#[derive(Debug, Default)]
pub struct Interner {
    strings: HashSet<&'static str>,
}

impl Interner {
    pub fn new() -> Interner {
        Interner {
            strings: HashSet::new(),
        }
    }

    pub fn intern(&mut self, s: &str) -> &'static str {
        if let Some(&s) = self.strings.get(s) {
            return s;
        }

        let boxed = s.to_string().into_boxed_str();
        let interned = Box::leak(boxed);
        self.strings.insert(interned);
        interned
    }
}
