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

        while bin.has_remaining() {
            let inst = Instruction::read(&mut bin)?;
            instructions.push(inst);
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
    Mov { dst: Register, src: Register },
}

impl Instruction {
    fn read(content: &mut Bytes) -> Result<Self> {
        let op_dw = content.try_get_u8()?;

        if op_dw & 0b111111_00 != 0b100010_00 {
            todo!("{}", op_dw);
        }

        let d = (op_dw & 0b10) >> 1;
        let w = op_dw & 1;

        let mod_reg_rm = content.try_get_u8()?;

        let mod_ = (mod_reg_rm & 0b11000000) >> 6;

        if mod_ != 0b11 {
            todo!("mod: {mod_:b}");
        }

        let reg = (mod_reg_rm & 0b111000) >> 3;
        let rm = mod_reg_rm & 0b111;

        let mut dst = Register::from_w_reg(w, reg)?;
        let mut src = Register::from_w_reg(w, rm)?;
        if d == 0 {
            mem::swap(&mut dst, &mut src);
        }

        Ok(Self::Mov { dst, src })
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

#[repr(u8)]
#[allow(dead_code)]
#[derive(Debug, PartialEq)]
enum Register {
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

impl Register {
    pub fn from_w_reg(w: u8, reg: u8) -> Result<Self> {
        let as_u8 = (w << 3) | reg;

        if as_u8 > Register::DI as u8 {
            Err(anyhow!("Invalid w/register combination {w:b} {reg:b}"))
        } else {
            Ok(unsafe { std::mem::transmute(as_u8) })
        }
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Register::AL => write!(f, "al"),
            Register::AX => write!(f, "ax"),
            Register::CL => write!(f, "cl"),
            Register::CX => write!(f, "cx"),
            Register::DL => write!(f, "dl"),
            Register::DX => write!(f, "dx"),
            Register::BL => write!(f, "bl"),
            Register::BX => write!(f, "bx"),
            Register::AH => write!(f, "ah"),
            Register::SP => write!(f, "sp"),
            Register::CH => write!(f, "ch"),
            Register::BP => write!(f, "bp"),
            Register::DH => write!(f, "dh"),
            Register::SI => write!(f, "si"),
            Register::BH => write!(f, "bh"),
            Register::DI => write!(f, "di"),
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
    fn test_single_inst() {
        let original = assemble("mov cx, bx");
        let program = Program::decode(original).unwrap();

        assert_eq!(program.instructions.len(), 1);
        assert_eq!(
            program.instructions[0],
            Instruction::Mov {
                dst: Register::CX,
                src: Register::BX
            }
        );
    }

    #[test]
    fn test_multiple_inst() {
        let original = assemble(indoc! {"
            mov al, dl
            mov cx, bx
        "});
        let program = Program::decode(original).unwrap();

        assert_eq!(program.instructions.len(), 2);
        assert_eq!(
            program.instructions[0],
            Instruction::Mov {
                dst: Register::AL,
                src: Register::DL
            }
        );
        assert_eq!(
            program.instructions[1],
            Instruction::Mov {
                dst: Register::CX,
                src: Register::BX
            }
        );
    }

    #[test]
    fn test_roundtrip() {
        let original = assemble(indoc! {"
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
        let disassembled = Program::decode(original.clone()).unwrap().to_string();
        let reassembled = assemble(&disassembled);

        assert_eq!(original, reassembled);
    }

    use super::*;

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
