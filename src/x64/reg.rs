#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Reg {
    index: u8,
    pub float: bool,
}

impl Reg {
    pub const ALLOC_I: [Reg; 15] = [
        Reg::RAX,
        Reg::RBX,
        Reg::RCX,
        Reg::RDX,
        Reg::RBP,
        Reg::RSI,
        Reg::RDI,
        Reg::R8,
        Reg::R9,
        Reg::R10,
        Reg::R11,
        Reg::R12,
        Reg::R13,
        Reg::R14,
        Reg::R15,
    ];

    pub const ALLOC_F: [Reg; 16] = [
        Reg::XMM0,
        Reg::XMM1,
        Reg::XMM2,
        Reg::XMM3,
        Reg::XMM4,
        Reg::XMM5,
        Reg::XMM6,
        Reg::XMM7,
        Reg::XMM8,
        Reg::XMM9,
        Reg::XMM10,
        Reg::XMM11,
        Reg::XMM12,
        Reg::XMM13,
        Reg::XMM14,
        Reg::XMM15,
    ];

    pub const RAX: Reg = Reg::int(0);
    pub const RBX: Reg = Reg::int(1);
    pub const RCX: Reg = Reg::int(2);
    pub const RDX: Reg = Reg::int(3);
    pub const RSP: Reg = Reg::int(4);
    pub const RBP: Reg = Reg::int(5);
    pub const RSI: Reg = Reg::int(6);
    pub const RDI: Reg = Reg::int(7);
    pub const R8: Reg = Reg::int(8);
    pub const R9: Reg = Reg::int(9);
    pub const R10: Reg = Reg::int(10);
    pub const R11: Reg = Reg::int(11);
    pub const R12: Reg = Reg::int(12);
    pub const R13: Reg = Reg::int(13);
    pub const R14: Reg = Reg::int(14);
    pub const R15: Reg = Reg::int(15);

    pub const XMM0: Reg = Reg::float(0);
    pub const XMM1: Reg = Reg::float(1);
    pub const XMM2: Reg = Reg::float(2);
    pub const XMM3: Reg = Reg::float(3);
    pub const XMM4: Reg = Reg::float(4);
    pub const XMM5: Reg = Reg::float(5);
    pub const XMM6: Reg = Reg::float(6);
    pub const XMM7: Reg = Reg::float(7);
    pub const XMM8: Reg = Reg::float(8);
    pub const XMM9: Reg = Reg::float(9);
    pub const XMM10: Reg = Reg::float(10);
    pub const XMM11: Reg = Reg::float(11);
    pub const XMM12: Reg = Reg::float(12);
    pub const XMM13: Reg = Reg::float(13);
    pub const XMM14: Reg = Reg::float(14);
    pub const XMM15: Reg = Reg::float(15);

    pub const fn int(index: u8) -> Self {
        Self {
            index,
            float: false,
        }
    }

    pub const fn float(index: u8) -> Self {
        Self { index, float: true }
    }
}
