//! Opcode definition and interpreter for 6502 assembly.
//!
//! References:
//! - [NMOS 6502 Opcodes](http://www.6502.org/tutorials/6502opcodes.html)
//! - [The Registers](https://web.archive.org/web/20210626024532/http://www.obelisk.me.uk/6502/registers.html)
//! - [Easy 6502](http://skilldrick.github.io/easy6502)

pub mod interp;
pub mod opcode;

pub use interp::*;
pub use opcode::*;

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
