use anyhow::{Result, anyhow};
use bytes::{Buf as _, Bytes};
use std::{
    env::args,
    fmt::{self, Display},
    fs,
    path::Path,
};

fn main() {
    let mut args = args();
    let Some(bin_path) = args.nth(1) else {
        println!("Usage: sim8086 <bin_path>");
        return;
    };
    let program = Program::load_path(&bin_path).unwrap();
    println!("{program}");
}

struct Program {
    instructions: Vec<Inst>,
}

impl Program {
    pub fn load_path(path: impl AsRef<Path>) -> Result<Self> {
        let bin = Bytes::from(fs::read(path)?);
        Self::decode(bin)
    }

    pub fn decode(mut bin: Bytes) -> Result<Self> {
        let mut instructions = Vec::new();

        let mut index = 0;
        while bin.has_remaining() {
            match Inst::read(&mut bin) {
                Ok(inst) => instructions.push(inst),
                Err(err) => return Err(err.context(format!("@ {index}"))),
            }
            index += 1;
        }

        Ok(Self { instructions })
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "bits 16\n")?;

        for inst in &self.instructions {
            write!(f, "{inst}")?;
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Inst {
    Mov(Bop),
    Add(Bop),
}

impl Inst {
    fn read(content: &mut Bytes) -> Result<Self> {
        let op = content.try_get_u8()?;

        // MOV: Register/memory to/from register
        if op >> 2 == 0b100010 {
            Ok(Self::Mov(Bop::read_rm_r(op, content)?))
        // MOV: Immediate to register/memory
        } else if op >> 1 == 0b1100011 {
            Ok(Self::Mov(Bop::read_rm_i(op, content)?))
        // MOV: Immediate to register
        } else if op >> 4 == 0b1011 {
            Self::read_mov_r_i(op, content)
        // MOV: Memory to/from accumulator
        } else if op >> 2 == 0b101000 {
            Self::read_mov_m_acc(op, content)
        // ADD: reg/mem with reg to either
        } else if op >> 6 == 0b0 {
            Ok(Self::Add(Bop::read_rm_r(op, content)?))
        } else if op >> 2 == 0b100000 {
            let s = (op >> 1) & 1;
            let w = op & 1;

            let mod_reg_rm = content.try_get_u8()?;
            let dst = Dst::read_rm(w, mod_reg_rm, content)?;
            let imm = Imm::read(w & !s, content)?;

            match mod_reg_rm >> 3 & 0b111 {
                0b000 => Ok(Self::Add(Bop(dst, Src::Imm(imm)))),
                _ => todo!("{mod_reg_rm:b}"),
            }
        } else {
            anyhow::bail!("TODO: {op:b}");
        }
    }

    fn read_mov_r_i(op_w_reg: u8, content: &mut Bytes) -> Result<Self> {
        let w = (op_w_reg >> 3) & 1;
        let reg = op_w_reg & 0b111;
        let imm = Imm::read(w, content)?;

        Ok(Self::Mov(Bop(
            Dst::Reg(Reg::from_w_reg(w, reg)?),
            Src::Imm(imm),
        )))
    }

    fn read_mov_m_acc(op_dw: u8, content: &mut Bytes) -> Result<Self> {
        let d = (op_dw & 0b10) >> 1;
        let w = op_dw & 1;

        let acc = if w == 0 { Reg::AL } else { Reg::AX };
        let mem = Mem::Addr(content.try_get_u16_le()?);

        let (dst, src) = if d == 0 {
            (Dst::Reg(acc), Src::Mem(mem))
        } else {
            (Dst::Mem(mem), Src::Reg(acc))
        };

        Ok(Self::Mov(Bop(dst, src)))
    }

    // DSL
    pub fn mov(dst: impl Into<Dst>, src: impl Into<Src>) -> Self {
        Self::Mov(Bop(dst.into(), src.into()))
    }

    pub fn add(dst: impl Into<Dst>, src: impl Into<Src>) -> Self {
        Self::Add(Bop(dst.into(), src.into()))
    }
}

impl Display for Inst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Inst::Mov(Bop(dst, src)) => {
                writeln!(f, "mov {dst}, {src}")
            }
            Inst::Add(Bop(dst, src)) => {
                writeln!(f, "add {dst}, {src}")
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
struct Bop(Dst, Src);

impl Bop {
    fn read_rm_r(op_dw: u8, content: &mut Bytes) -> Result<Self> {
        let d = (op_dw & 0b10) >> 1;
        let w = op_dw & 1;

        let mod_reg_rm = content.try_get_u8()?;
        let reg = (mod_reg_rm & 0b111000) >> 3;
        let reg = Reg::from_w_reg(w, reg)?;
        let rm = Dst::read_rm(w, mod_reg_rm, content)?;

        let (dst, src) = if d == 0 {
            (rm, Src::Reg(reg))
        } else {
            (Dst::Reg(reg), rm.into())
        };

        Ok(Self(dst, src))
    }

    fn read_rm_i(op_w: u8, content: &mut Bytes) -> Result<Self> {
        let w = op_w & 1;

        let mod_reg_rm = content.try_get_u8()?;
        let dst = Dst::read_rm(w, mod_reg_rm, content)?;
        let imm = Imm::read(w, content)?;

        Ok(Self(dst, Src::Imm(imm)))
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Dst {
    Reg(Reg),
    Mem(Mem),
}

impl Dst {
    fn read_rm(w: u8, mod_reg_rm: u8, content: &mut Bytes) -> Result<Self> {
        let mod_ = mod_reg_rm >> 6;
        let rm_bits = mod_reg_rm & 0b111;

        let rm = match mod_ {
            0b00 => match rm_bits {
                0b000 => Mem::BX_SI,
                0b001 => Mem::BX_DI,
                0b010 => Mem::BP_SI,
                0b011 => Mem::BP_DI,
                0b100 => Mem::SI,
                0b101 => Mem::DI,
                0b110 => Mem::Addr(content.try_get_u16_le()?),
                0b111 => Mem::BX,
                _ => unreachable!(),
            }
            .into(),
            0b01 | 0b10 => {
                let disp = if mod_ == 0b01 {
                    content.try_get_i8()? as i16
                } else {
                    content.try_get_i16_le()?
                };

                match rm_bits {
                    0b000 => Mem::BX_SI_D(disp),
                    0b001 => Mem::BX_DI_D(disp),
                    0b010 => Mem::BP_SI_D(disp),
                    0b011 => Mem::BP_DI_D(disp),
                    0b100 => Mem::SI_D(disp),
                    0b101 => Mem::DI_D(disp),
                    0b110 => Mem::BP_D(disp),
                    0b111 => Mem::BX_D(disp),
                    _ => unreachable!(),
                }
                .into()
            }
            0b11 => Reg::from_w_reg(w, rm_bits)?.into(),
            _ => anyhow::bail!("TODO mod: {mod_:b}"),
        };

        Ok(rm)
    }
}

impl Display for Dst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Reg(reg) => write!(f, "{reg}"),
            Self::Mem(mem) => write!(f, "{mem}"),
        }
    }
}

impl From<Reg> for Dst {
    fn from(value: Reg) -> Self {
        Self::Reg(value)
    }
}

impl<T: Into<Mem>> From<T> for Dst {
    fn from(value: T) -> Self {
        Self::Mem(value.into())
    }
}

impl TryFrom<Src> for Dst {
    type Error = anyhow::Error;

    fn try_from(value: Src) -> Result<Self, Self::Error> {
        match value {
            Src::Reg(reg) => Ok(Dst::Reg(reg)),
            Src::Mem(mem) => Ok(Dst::Mem(mem)),
            Src::Imm(_) => Err(anyhow!("Immediates are not a valid mov destination")),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Src {
    Imm(Imm),
    Reg(Reg),
    Mem(Mem),
}

impl<T: Into<Imm>> From<T> for Src {
    fn from(value: T) -> Self {
        Self::Imm(value.into())
    }
}

impl From<Reg> for Src {
    fn from(value: Reg) -> Self {
        Self::Reg(value)
    }
}

impl From<Mem> for Src {
    fn from(value: Mem) -> Self {
        Self::Mem(value)
    }
}

impl From<Dst> for Src {
    fn from(value: Dst) -> Self {
        match value {
            Dst::Reg(reg) => Src::Reg(reg),
            Dst::Mem(mem) => Src::Mem(mem),
        }
    }
}

impl Display for Src {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Src::Imm(imm) => write!(f, "{imm}"),
            Src::Reg(reg) => write!(f, "{reg}"),
            Src::Mem(mem) => write!(f, "{mem}"),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Imm {
    Byte(i8),
    Word(i16),
}

impl Imm {
    pub fn read(w: u8, content: &mut Bytes) -> Result<Self> {
        let imm = if w == 0 {
            Self::Byte(content.try_get_i8()?)
        } else {
            Self::Word(content.try_get_i16_le()?)
        };
        Ok(imm)
    }
}

impl Display for Imm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Imm::Byte(byte) => write!(f, "byte {byte}"),
            Imm::Word(word) => write!(f, "word {word}"),
        }
    }
}

impl From<i8> for Imm {
    fn from(value: i8) -> Self {
        Imm::Byte(value)
    }
}

impl From<i16> for Imm {
    fn from(value: i16) -> Self {
        Imm::Word(value)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
#[allow(non_camel_case_types)]
enum Mem {
    Addr(u16),
    BX_SI,
    BX_DI,
    BP_SI,
    BP_DI,
    SI,
    DI,
    BX,
    BX_D(i16),
    BP_D(i16),
    BX_SI_D(i16),
    BX_DI_D(i16),
    BP_SI_D(i16),
    BP_DI_D(i16),
    SI_D(i16),
    DI_D(i16),
}

impl Display for Mem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Addr(addr) => write!(f, "[{addr}]"),
            Self::BX_SI => write!(f, "[bx + si]"),
            Self::BX_DI => write!(f, "[bx + di]"),
            Self::BP_SI => write!(f, "[bp + si]"),
            Self::BP_DI => write!(f, "[bp + di]"),
            Self::SI => write!(f, "[si]"),
            Self::DI => write!(f, "[di]"),
            Self::BX => write!(f, "[bx]"),
            Self::BX_SI_D(d) => write!(f, "[bx + si + {d}]"),
            Self::BX_DI_D(d) => write!(f, "[bx + di + {d}]"),
            Self::BP_SI_D(d) => write!(f, "[bp + si + {d}]"),
            Self::BP_DI_D(d) => write!(f, "[bp + di + {d}]"),
            Self::SI_D(d) => write!(f, "[si + {d}]"),
            Self::DI_D(d) => write!(f, "[di + {d}]"),
            Self::BP_D(d) => write!(f, "[bp + {d}]"),
            Self::BX_D(d) => write!(f, "[bx + {d}]"),
        }
    }
}

#[repr(u8)]
#[allow(dead_code)]
#[derive(Debug, PartialEq, Clone, Copy)]
enum Reg {
    //     W REG
    AL = 0b0_000,
    AX = 0b1_000,
    CL = 0b0_001,
    CX = 0b1_001,
    DL = 0b0_010,
    DX = 0b1_010,
    BL = 0b0_011,
    BX = 0b1_011,
    AH = 0b0_100,
    SP = 0b1_100,
    CH = 0b0_101,
    BP = 0b1_101,
    DH = 0b0_110,
    SI = 0b1_110,
    BH = 0b0_111,
    DI = 0b1_111,
}

impl Reg {
    pub fn from_w_reg(w: u8, reg: u8) -> Result<Self> {
        let as_u8 = (w << 3) | reg;

        if as_u8 > Reg::DI as u8 {
            Err(anyhow!("Invalid w/register combination {w:b} {reg:b}"))
        } else {
            Ok(unsafe { std::mem::transmute(as_u8) })
        }
    }
}

impl Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Reg::AL => write!(f, "al"),
            Reg::AX => write!(f, "ax"),
            Reg::CL => write!(f, "cl"),
            Reg::CX => write!(f, "cx"),
            Reg::DL => write!(f, "dl"),
            Reg::DX => write!(f, "dx"),
            Reg::BL => write!(f, "bl"),
            Reg::BX => write!(f, "bx"),
            Reg::AH => write!(f, "ah"),
            Reg::SP => write!(f, "sp"),
            Reg::CH => write!(f, "ch"),
            Reg::BP => write!(f, "bp"),
            Reg::DH => write!(f, "dh"),
            Reg::SI => write!(f, "si"),
            Reg::BH => write!(f, "bh"),
            Reg::DI => write!(f, "di"),
        }
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use std::{
        io::{Read, Write},
        process::{Command, Stdio},
    };

    #[test]
    fn test_multiple_inst() {
        let original = assemble(indoc! {"
            mov al, dl
            mov cx, bx
        "});
        let program = Program::decode(original).unwrap();

        assert_eq!(program.instructions.len(), 2);
        assert_eq!(program.instructions[0], Inst::mov(Reg::AL, Reg::DL));
        assert_eq!(program.instructions[1], Inst::mov(Reg::CX, Reg::BX));
    }

    #[test]
    fn test_r_r() {
        assert_inst_eq("mov cx, bx", Inst::mov(Reg::CX, Reg::BX));
    }

    #[test]
    fn test_r_i() {
        assert_inst_eq("mov ax, 12", Inst::mov(Reg::AX, 12i16));
    }

    #[test]
    fn test_r_m_no_disp() {
        assert_insts_eq(
            indoc! {"
                mov cx, [bx+si]
                mov cx, [bx+di]
                mov cx, [bp+si]
                mov cx, [bp+di]
                mov cx, [si]
                mov cx, [di]
                mov cx, [75]
                mov cx, [bx]
                mov [bx+si], cx
                mov [bx+di], cx
                mov [bp+si], cx
                mov [bp+di], cx
                mov [si], cx
                mov [di], cx
                mov [75], cx
                mov [bx], cx
            "},
            &[
                Inst::mov(Reg::CX, Mem::BX_SI),
                Inst::mov(Reg::CX, Mem::BX_DI),
                Inst::mov(Reg::CX, Mem::BP_SI),
                Inst::mov(Reg::CX, Mem::BP_DI),
                Inst::mov(Reg::CX, Mem::SI),
                Inst::mov(Reg::CX, Mem::DI),
                Inst::mov(Reg::CX, Mem::Addr(75)),
                Inst::mov(Reg::CX, Mem::BX),
                Inst::mov(Mem::BX_SI, Reg::CX),
                Inst::mov(Mem::BX_DI, Reg::CX),
                Inst::mov(Mem::BP_SI, Reg::CX),
                Inst::mov(Mem::BP_DI, Reg::CX),
                Inst::mov(Mem::SI, Reg::CX),
                Inst::mov(Mem::DI, Reg::CX),
                Inst::mov(Mem::Addr(75), Reg::CX),
                Inst::mov(Mem::BX, Reg::CX),
            ],
        );
    }

    #[test]
    fn test_r_m_disp_8() {
        assert_insts_eq(
            indoc! {"
                mov ax, [bx + si + 8]
                mov bx, [bx + di + 12]
                mov cx, [bp + si + 4]
                mov dx, [bp + di + 7]
                mov ah, [si + 3]
                mov al, [di + 1]
                mov bl, [bp + 9]
                mov bh, [bx + 5]
            "},
            &[
                Inst::mov(Reg::AX, Mem::BX_SI_D(8)),
                Inst::mov(Reg::BX, Mem::BX_DI_D(12)),
                Inst::mov(Reg::CX, Mem::BP_SI_D(4)),
                Inst::mov(Reg::DX, Mem::BP_DI_D(7)),
                Inst::mov(Reg::AH, Mem::SI_D(3)),
                Inst::mov(Reg::AL, Mem::DI_D(1)),
                Inst::mov(Reg::BL, Mem::BP_D(9)),
                Inst::mov(Reg::BH, Mem::BX_D(5)),
            ],
        );
    }

    #[test]
    fn test_rm_i() {
        assert_insts_eq(
            indoc! {"
            mov [si], byte 12
            mov [si], word 12
            mov [si], byte -12
            mov [si], word -12
        "},
            &[
                Inst::mov(Mem::SI, 12i8),
                Inst::mov(Mem::SI, 12i16),
                Inst::mov(Mem::SI, -12i8),
                Inst::mov(Mem::SI, -12i16),
            ],
        );
    }

    #[test]
    fn test_m_r_disp_8() {
        assert_insts_eq(
            indoc! {"
                mov [bx + si + 8], ax
                mov [bx + di + 12], bx
                mov [bp + si + 4], cx
                mov [bp + di + 7], dx
                mov [si + 3], ah
                mov [di + 1], al
                mov [bp + 9], bl
                mov [bx + 5], bh
                mov [bx + si - 8], ax
                mov [bx + di - 12], bx
                mov [bp + si - 4], cx
                mov [bp + di - 7], dx
                mov [si - 3], ah
                mov [di - 1], al
                mov [bp - 9], bl
                mov [bx - 5], bh
            "},
            &[
                Inst::mov(Mem::BX_SI_D(8), Reg::AX),
                Inst::mov(Mem::BX_DI_D(12), Reg::BX),
                Inst::mov(Mem::BP_SI_D(4), Reg::CX),
                Inst::mov(Mem::BP_DI_D(7), Reg::DX),
                Inst::mov(Mem::SI_D(3), Reg::AH),
                Inst::mov(Mem::DI_D(1), Reg::AL),
                Inst::mov(Mem::BP_D(9), Reg::BL),
                Inst::mov(Mem::BX_D(5), Reg::BH),
                Inst::mov(Mem::BX_SI_D(-8), Reg::AX),
                Inst::mov(Mem::BX_DI_D(-12), Reg::BX),
                Inst::mov(Mem::BP_SI_D(-4), Reg::CX),
                Inst::mov(Mem::BP_DI_D(-7), Reg::DX),
                Inst::mov(Mem::SI_D(-3), Reg::AH),
                Inst::mov(Mem::DI_D(-1), Reg::AL),
                Inst::mov(Mem::BP_D(-9), Reg::BL),
                Inst::mov(Mem::BX_D(-5), Reg::BH),
            ],
        );
    }

    #[test]
    fn test_r_m_disp_16() {
        assert_insts_eq(
            indoc! {"
                mov ax, [bx + si + 1000]
                mov bx, [bx + di + 2000]
                mov cx, [bp + si + 3000]
                mov dx, [bp + di + 4000]
                mov sp, [si + 5000]
                mov bp, [di + 6000]
                mov si, [bp + 7000]
                mov di, [bx + 8000]
                mov ax, [bx + si - 1000]
                mov bx, [bx + di - 2000]
                mov cx, [bp + si - 3000]
                mov dx, [bp + di - 4000]
                mov sp, [si - 5000]
                mov bp, [di - 6000]
                mov si, [bp - 7000]
                mov di, [bx - 8000]
            "},
            &[
                Inst::mov(Reg::AX, Mem::BX_SI_D(1000)),
                Inst::mov(Reg::BX, Mem::BX_DI_D(2000)),
                Inst::mov(Reg::CX, Mem::BP_SI_D(3000)),
                Inst::mov(Reg::DX, Mem::BP_DI_D(4000)),
                Inst::mov(Reg::SP, Mem::SI_D(5000)),
                Inst::mov(Reg::BP, Mem::DI_D(6000)),
                Inst::mov(Reg::SI, Mem::BP_D(7000)),
                Inst::mov(Reg::DI, Mem::BX_D(8000)),
                Inst::mov(Reg::AX, Mem::BX_SI_D(-1000)),
                Inst::mov(Reg::BX, Mem::BX_DI_D(-2000)),
                Inst::mov(Reg::CX, Mem::BP_SI_D(-3000)),
                Inst::mov(Reg::DX, Mem::BP_DI_D(-4000)),
                Inst::mov(Reg::SP, Mem::SI_D(-5000)),
                Inst::mov(Reg::BP, Mem::DI_D(-6000)),
                Inst::mov(Reg::SI, Mem::BP_D(-7000)),
                Inst::mov(Reg::DI, Mem::BX_D(-8000)),
            ],
        );
    }

    #[test]
    fn test_m_r_disp_16() {
        assert_insts_eq(
            indoc! {"
                mov [bx + si + 1000], ax
                mov [bx + di + 2000], bx
                mov [bp + si + 3000], cx
                mov [bp + di + 4000], dx
                mov [si + 5000], sp
                mov [di + 6000], bp
                mov [bp + 7000], si
                mov [bx + 8000], di
                mov [bx + si - 1000], ax
                mov [bx + di - 2000], bx
                mov [bp + si - 3000], cx
                mov [bp + di - 4000], dx
                mov [si - 5000], sp
                mov [di - 6000], bp
                mov [bp - 7000], si
                mov [bx - 8000], di
            "},
            &[
                Inst::mov(Mem::BX_SI_D(1000), Reg::AX),
                Inst::mov(Mem::BX_DI_D(2000), Reg::BX),
                Inst::mov(Mem::BP_SI_D(3000), Reg::CX),
                Inst::mov(Mem::BP_DI_D(4000), Reg::DX),
                Inst::mov(Mem::SI_D(5000), Reg::SP),
                Inst::mov(Mem::DI_D(6000), Reg::BP),
                Inst::mov(Mem::BP_D(7000), Reg::SI),
                Inst::mov(Mem::BX_D(8000), Reg::DI),
                Inst::mov(Mem::BX_SI_D(-1000), Reg::AX),
                Inst::mov(Mem::BX_DI_D(-2000), Reg::BX),
                Inst::mov(Mem::BP_SI_D(-3000), Reg::CX),
                Inst::mov(Mem::BP_DI_D(-4000), Reg::DX),
                Inst::mov(Mem::SI_D(-5000), Reg::SP),
                Inst::mov(Mem::DI_D(-6000), Reg::BP),
                Inst::mov(Mem::BP_D(-7000), Reg::SI),
                Inst::mov(Mem::BX_D(-8000), Reg::DI),
            ],
        );
    }

    #[test]
    fn test_add() {
        assert_insts_eq(
            indoc! {"
                add cx, ax
                add ax, cx
                add ax, [75]
                add ax, [bp+si+10]
                add [bp+si+10], cx
                add cx, 42
                add cx, -42
                add cx, 30000
                add cx, -30000
            "},
            &[
                Inst::add(Reg::CX, Reg::AX),
                Inst::add(Reg::AX, Reg::CX),
                Inst::add(Reg::AX, Mem::Addr(75)),
                Inst::add(Reg::AX, Mem::BP_SI_D(10)),
                Inst::add(Mem::BP_SI_D(10), Reg::CX),
                Inst::add(Reg::CX, 42i8),
                Inst::add(Reg::CX, -42i8),
                Inst::add(Reg::CX, 30_000i16),
                Inst::add(Reg::CX, -30_000i16),
            ],
        );
    }

    #[test]
    fn test_0037() {
        assert_roundtrip("mov cx, bx");
    }

    #[test]
    fn test_0038() {
        assert_roundtrip(indoc! {"
            mov cx, bx
            mov ch, ah
            mov dx, bx
            mov si, bx
            mov bx, di
            mov al, cl
            mov ch, ch
            mov bx, ax
            mov bx, si
            mov sp, di
            mov bp, ax
        "});
    }

    #[test]
    fn test_0039() {
        assert_roundtrip(indoc! {"
            bits 16

            ; Register-to-register
            mov si, bx
            mov dh, al

            ; 8-bit immediate-to-register
            mov cl, 12
            mov ch, -12

            ; 16-bit immediate-to-register
            mov cx, 12
            mov cx, -12
            mov dx, 3948
            mov dx, -3948

            ; Source address calculation
            mov al, [bx + si]
            mov bx, [bp + di]
            mov dx, [bp]

            ; Source address calculation plus 8-bit displacement
            mov ah, [bx + si + 4]

            ; Source address calculation plus 16-bit displacement
            mov al, [bx + si + 4999]

            ; Dest address calculation
            mov [bx + di], cx
            mov [bp + si], cl
            mov [bp], ch
        "});
    }

    #[test]
    fn test_0040() {
        assert_roundtrip(indoc! {"
            bits 16

            ; Signed displacements
            mov ax, [bx + di - 37]
            mov [si - 300], cx
            mov dx, [bx - 32]

            ; Explicit sizes
            mov [bp + di], byte 7
            mov [di + 901], word 347

            ; Direct address
            mov bp, [5]
            mov bx, [3458]

            ; Memory-to-accumulator test
            mov ax, [2555]
            mov ax, [16]

            ; Accumulator-to-memory test
            mov [2554], ax
            mov [15], ax
        "});
    }

    use super::*;

    #[track_caller]
    fn assert_roundtrip(source: &str) {
        let assembled = assemble(source);
        let disassembled = Program::decode(assembled.clone()).unwrap().to_string();
        let reassembled = assemble(&disassembled);
        assert_bin_eq(assembled, reassembled);
    }

    #[track_caller]
    fn assert_inst_eq(source: &str, instruction: Inst) {
        let assembled = assemble(source);
        let program = Program::decode(assembled.clone()).unwrap();

        assert_eq!(program.instructions.len(), 1);
        assert_eq!(program.instructions[0], instruction);

        let reassembled = assemble(&program.to_string());
        assert_bin_eq(assembled, reassembled);
    }

    #[track_caller]
    fn assert_insts_eq(source: &str, instructions: &[Inst]) {
        let assembled = assemble(source);
        let program = Program::decode(assembled.clone()).unwrap();

        assert_eq!(program.instructions.len(), instructions.len());

        for (index, instruction) in instructions.iter().enumerate() {
            assert_eq!(&program.instructions[index], instruction);
        }

        let reassembled = assemble(&program.to_string());
        assert_bin_eq(assembled, reassembled);
    }

    #[track_caller]
    fn assert_bin_eq(a: Bytes, b: Bytes) {
        if a != b {
            pretty_assertions::assert_eq!(disassemble(a), disassemble(b));
        }
    }

    fn assemble(source: &str) -> Bytes {
        let mut src_file = tempfile::NamedTempFile::new().unwrap();
        write!(&mut src_file, "bits 16\n{source}").unwrap();

        let mut out_file = tempfile::NamedTempFile::new().unwrap();

        Command::new("nasm")
            .stdin(Stdio::null())
            .stderr(Stdio::inherit())
            .stdout(Stdio::inherit())
            .arg(src_file.path())
            .arg("-o")
            .arg(out_file.path())
            .spawn()
            .unwrap()
            .wait_with_output()
            .unwrap();

        let mut buf = Vec::new();
        out_file.read_to_end(&mut buf).unwrap();
        Bytes::from(buf)
    }

    fn disassemble(bin: Bytes) -> String {
        let mut bin_file = tempfile::NamedTempFile::new().unwrap();
        bin_file.write_all(&bin).unwrap();

        let output = Command::new("ndisasm")
            .stdin(Stdio::null())
            .stderr(Stdio::inherit())
            .stdout(Stdio::piped())
            .arg(bin_file.path())
            .spawn()
            .unwrap()
            .wait_with_output()
            .unwrap();

        String::from_utf8(output.stdout).unwrap()
    }
}
