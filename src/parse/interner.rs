use std::collections::HashSet;

#[derive(Debug, Clone, Default)]
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

    /// # Safety
    /// This function may only be called when the interner is not used anymore.
    pub unsafe fn clear(&mut self) {
        for s in self.strings.drain() {
            let _ = Box::from_raw(s as *const str as *mut str);
        }
    }
}
