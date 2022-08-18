use core::fmt;

use crate::Res;

use super::context::{Address, Register};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operand {
    Register(Register),
    Literal(u8),
    Address(Address),
}
impl Operand {
    pub fn register(idx: u8) -> Self {
        Self::Register(Register(idx))
    }
    pub fn literal(val: u8) -> Self {
        Self::Literal(val)
    }
    pub fn address(addr: u8) -> Self {
        Self::Address(Address(addr))
    }
}
impl From<Register> for Operand {
    fn from(val: Register) -> Self {
        Operand::Register(val)
    }
}
impl From<u8> for Operand {
    fn from(val: u8) -> Self {
        Operand::Literal(val)
    }
}
impl From<Address> for Operand {
    fn from(val: Address) -> Self {
        Operand::Address(val)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Mov { dst: Register, src: Register },
    Set { dst: Register, value: u8 },
    Str { dst: Address, src: Register },
    Load { dst: Register, addr: Address },
    Add { dst: Register, src: Register },
    Sub { dst: Register, src: Register },
}
impl Op {
    /**
        Emit the correct opcode for a move/set/load/store instruction
        depending on the operands. If the combination of operands is not
        sopported, then this function may panic.
        We can emit opcodes for the following combinations:
        - register to register (`MOV`)
        - literal to register (`SET`)
        - register to address (`STR`)
        - address to register (`LOAD`)
    */
    pub fn move_value(dst: Operand, src: Operand) -> Res<Option<Self>> {
        let op = match dst {
            Operand::Register(dst) => return Self::set_register(dst, src),
            Operand::Literal(_) => return Err(Box::from("Cannot use a literal as destination")),
            Operand::Address(dst) => match src {
                Operand::Register(src) => Self::Str { dst, src },
                Operand::Literal(_) => {
                    return Err(Box::from(
                        "Cannot use a literal as source when the destination is an address",
                    ));
                }
                Operand::Address(_) => {
                    return Err(Box::from(
                        "Cannot use an address as source when the destination is an address",
                    ));
                }
            },
        };

        return Ok(Some(op));
    }

    pub fn set_register(dst: Register, src: Operand) -> Res<Option<Op>> {
        let op = match src {
            Operand::Register(src) if dst == src => return Ok(None),
            Operand::Register(src) => Self::Mov { dst, src },
            Operand::Literal(value) => Self::Set { dst, value },
            Operand::Address(addr) => Self::Load { dst, addr },
        };
        return Ok(Some(op));
    }

    pub fn emit_nop() -> Self {
        Self::Mov {
            dst: Register(7),
            src: Register(7),
        }
    }
}
impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Mov { dst, src } if dst == src => write!(f, "MOV {dst}, {src} ; NOP"),
            Self::Mov { dst, src } => write!(f, "MOV {dst}, {src}"),
            Self::Set { dst, value: src } => write!(f, "SET {dst}, {src}"),
            Self::Str { dst, src } => write!(f, "STR [{dst}], {src}"),
            Self::Load { dst, addr: src } => write!(f, "LOAD {dst}, [{src}]"),
            Self::Add { dst, src } => write!(f, "ADD {dst}, {src}"),
            Self::Sub { dst, src } => write!(f, "SUB {dst}, {src}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_op_nop() {
        let (dst, src) = match Op::emit_nop() {
            Op::Mov { dst, src } => (dst, src),
            _ => panic!("Expected MOV between registers"),
        };
        assert_eq!(dst, src);
    }

    #[test]
    fn test_op_emit_set() {
        let tests: Vec<(Operand, Operand, Option<Op>)> = vec![
            (Register(0).into(), Register(0).into(), None),
            (
                Register(0).into(),
                Register(1).into(),
                Some(Op::Mov {
                    dst: Register(0),
                    src: Register(1),
                }),
            ),
            (
                Address(10).into(),
                Register(0).into(),
                Some(Op::Str {
                    dst: Address(10),
                    src: Register(0),
                }),
            ),
        ];

        for (dst, src, expected) in tests {
            let actual = Op::move_value(dst, src).unwrap();
            assert_eq!(actual, expected);
        }
    }

    #[test]
    #[should_panic]
    fn test_op_emit_set_invalid() {
        let dst = Address(0);
        let src = Address(0);
        let op = Op::move_value(dst.into(), src.into()).unwrap();
    }

    #[test]
    fn test_op_emit_set_same_reg_optimization() {
        let dst = Register(0).into();
        let src = Register(0).into();
        let op = Op::move_value(src, dst);
        assert!(op.is_ok());
        let op = op.unwrap();
        assert!(op.is_none());
    }
}
