use std::collections::BTreeSet;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Reg(u8);

struct Allocator {
    good: BTreeSet<Reg>,
    bad: BTreeSet<Reg>,
}

impl Allocator {
    fn new() -> Self {
        let mut ret = Self {
            good: BTreeSet::new(),
            bad: BTreeSet::new(),
        };

        for i in 0..4 {
            ret.bad.insert(Reg(i));
        }

        for i in 6..16 {
            ret.good.insert(Reg(i));
        }

        ret
    }

    fn alloc(mut self) -> Option<Reg> {
        match self.good.len() {
            0 => self.bad.pop_first(),
            _ => self.good.pop_first(),
        }
    }

    fn alloc_fixed(mut self, reg: Reg) -> Option<Reg> {
        self.good.take(&reg).or(self.bad.take(&reg))
    }

    fn free(mut self, reg: Reg) {
        if reg.0 < 4 {
            self.bad.insert(reg);
        } else {
            self.good.insert(reg);
        }
    }
}

impl Reg {
    pub fn name(self) -> &'static str {
        match self.0 {
            0 => "rax",
            1 => "rbx",
            2 => "rcx",
            3 => "rdx",
            4 => "rsp",
            5 => "rbp",
            6 => "rsi",
            7 => "rdi",
            8 => "r8",
            9 => "r9",
            10 => "r10",
            11 => "r11",
            12 => "r12",
            13 => "r13",
            14 => "r14",
            15 => "r15",
            _ => "NOT A REG",
        }
    }
}
