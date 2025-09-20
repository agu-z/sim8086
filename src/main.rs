use anyhow::{Result, anyhow};
use bytes::{Buf as _, Bytes};
use std::{
    env::args,
    fmt::{self, Display},
    fs, mem,
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
    instructions: Vec<Instruction>,
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
            match Instruction::read(&mut bin) {
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

#[derive(Debug, PartialEq)]
enum Instruction {
    Mov { dst: Reg, src: Src },
}

impl Instruction {
    fn read(content: &mut Bytes) -> Result<Self> {
        let op = content.try_get_u8()?;

        if op >> 2 == 0b100010 {
            Self::read_mov_rm_r(op, content)
        } else if op >> 4 == 0b1011 {
            Self::read_mov_r_i(op, content)
        } else {
            anyhow::bail!("TODO: {op:b}");
        }
    }

    fn read_mov_rm_r(op_dw: u8, content: &mut Bytes) -> Result<Self> {
        let d = (op_dw & 0b10) >> 1;
        let w = op_dw & 1;

        let mod_reg_rm = content.try_get_u8()?;

        let mod_ = (mod_reg_rm & 0b11000000) >> 6;

        if mod_ != 0b11 {
            anyhow::bail!("TODO mod: {mod_:b}");
        }

        let reg = (mod_reg_rm & 0b111000) >> 3;
        let rm = mod_reg_rm & 0b111;

        let mut dst = Reg::from_w_reg(w, reg)?;
        let mut src = Reg::from_w_reg(w, rm)?;
        if d == 0 {
            mem::swap(&mut dst, &mut src);
        }

        Ok(Self::Mov {
            dst,
            src: Src::Reg(src),
        })
    }

    fn read_mov_r_i(op_w_reg: u8, content: &mut Bytes) -> Result<Self> {
        let w = (op_w_reg >> 3) & 1;
        let reg = op_w_reg & 0b111;
        let imm = if w == 1 {
            content.try_get_u16_le()?
        } else {
            content.try_get_u8()? as u16
        };

        Ok(Self::Mov {
            dst: Reg::from_w_reg(w, reg)?,
            src: Src::Imm(imm),
        })
    }

    // DSL
    pub fn mov(dst: Reg, src: impl Into<Src>) -> Self {
        Self::Mov {
            dst: dst,
            src: src.into(),
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Mov { dst, src } => {
                writeln!(f, "mov {dst}, {src}")
            }
        }
    }
}

#[derive(Debug, PartialEq)]
enum Src {
    Reg(Reg),
    Imm(u16),
}

impl From<Reg> for Src {
    fn from(value: Reg) -> Self {
        Self::Reg(value)
    }
}

impl From<u16> for Src {
    fn from(value: u16) -> Self {
        Self::Imm(value)
    }
}

impl Display for Src {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Src::Reg(reg) => write!(f, "{reg}"),
            Src::Imm(imm) => write!(f, "{imm}"),
        }
    }
}

#[repr(u8)]
#[allow(dead_code)]
#[derive(Debug, PartialEq)]
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
        assert_eq!(program.instructions[0], Instruction::mov(Reg::AL, Reg::DL));
        assert_eq!(program.instructions[1], Instruction::mov(Reg::CX, Reg::BX));
    }

    #[test]
    fn test_r_r() {
        assert_inst_eq("mov cx, bx", Instruction::mov(Reg::CX, Reg::BX));
    }

    #[test]
    fn test_r_i() {
        assert_inst_eq("mov ax, 12", Instruction::mov(Reg::AX, 12));
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

    use super::*;

    fn assert_roundtrip(source: &str) {
        let assembled = assemble(source);
        let disassembled = Program::decode(assembled.clone()).unwrap().to_string();
        let reassembled = assemble(&disassembled);
        assert_eq!(assembled, reassembled);
    }

    fn assert_inst_eq(source: &str, instruction: Instruction) {
        let original = assemble(source);
        let program = Program::decode(original).unwrap();

        assert_eq!(program.instructions.len(), 1);
        assert_eq!(program.instructions[0], instruction);
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
}
