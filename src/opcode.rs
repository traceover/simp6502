use int_enum::IntEnum;

#[derive(Copy, Clone, Debug, Eq, PartialEq, IntEnum)]
#[repr(u8)]
pub enum Opcode {
    // ADC (ADd with Carry)
    ADCImm = 0x69,
    ADCZpg = 0x65,
    ADCZpx = 0x75,
    ADCAbs = 0x6d,
    ADCAbx = 0x7d,
    ADCAby = 0x79,
    ADCInx = 0x61,
    ADCIny = 0x71,

    // AND (bitwise AND with accumulator)
    ANDImm = 0x29,
    ANDZpg = 0x25,
    ANDZpx = 0x35,
    ANDAbs = 0x2d,
    ANDAbx = 0x3d,
    ANDAby = 0x39,
    ANDInx = 0x21,
    ANDIny = 0x31,

    // ASL (Arithmetic Shift Left)
    ASLAcc = 0x0a,
    ASLZpg = 0x06,
    ASLZpx = 0x16,
    ASLAbs = 0x0e,
    ASLAbx = 0x1e,

    // BIT (test BITs)
    BITZpg = 0x24,
    BITAbs = 0x2c,

    // Branch Instructions
    BPL = 0x10,
    BMI = 0x30,
    BVC = 0x50,
    BVS = 0x70,
    BCC = 0x90,
    BCS = 0xB0,
    BNE = 0xD0,
    BEQ = 0xF0,

    // BRK (BReaK)
    BRK = 0x00,

    // CMP (CoMPare accumulator)
    CMPImm = 0xc9,
    CMPZpg = 0xc5,
    CMPZpx = 0xd5,
    CMPAbs = 0xcd,
    CMPAbx = 0xdd,
    CMPAby = 0xd9,
    CMPInx = 0xc1,
    CMPIny = 0xd1,

    // CPX (ComPare X register)
    CPXImm = 0xe0,
    CPXZpg = 0xe4,
    CPXAbs = 0xec,

    // CPY (ComPare Y register)
    CPYImm = 0xc0,
    CPYZpg = 0xc4,
    CPYAbs = 0xcc,

    // DEC (DECrement memory)
    DECZpg = 0xc6,
    DECZpx = 0xd6,
    DECAbs = 0xce,
    DECAbx = 0xde,

    // EOR (bitwise Exclusive OR)
    EORImm = 0x49,
    EORZpg = 0x45,
    EORZpx = 0x55,
    EORAbs = 0x4d,
    EORAbx = 0x5d,
    EORAby = 0x59,
    EORInx = 0x41,
    EORIny = 0x51,

    // Flag (Processor Status) Instructions
    CLC = 0x18, // CLear Carry
    SEC = 0x38, // SEt Carry
    CLI = 0x58, // CLear Interrupt
    SEI = 0x78, // SEt Interrupt
    CLV = 0xb8, // CLear oVerflow
    CLD = 0xd8, // CLear Decimal
    SED = 0xf8, // SEt Decimal

    // INC (INCrement memory)
    INCZpg = 0xe6,
    INCZpx = 0xf6,
    INCAbs = 0xee,
    INCAbx = 0xfe,

    // JMP (JuMP)
    JMPAbs = 0x4c,
    JMPInd = 0x6c,

    // JSR (Jump to SubRoutine)
    JSR = 0x20,

    // LDA (LoaD Accumulator)
    LDAImm = 0xa9,
    LDAZpg = 0xa5,
    LDAZpx = 0xb5,
    LDAAbs = 0xad,
    LDAAbx = 0xbd,
    LDAAby = 0xb9,
    LDAInx = 0xa1,
    LDAIny = 0xb1,

    // LDX (LoaD X register)
    LDXImm = 0xa2,
    LDXZpg = 0xa6,
    LDXZpy = 0xb6,
    LDXAbs = 0xae,
    LDXAby = 0xbe,

    // LDY (LoaD Y register)
    LDYImm = 0xa0,
    LDYZpg = 0xa4,
    LDYZpx = 0xb4,
    LDYAbs = 0xac,
    LDYAbx = 0xbc,

    // LSR (Logical Shift Right)
    LSRAcc = 0x4a,
    LSRZpg = 0x46,
    LSRZpx = 0x56,
    LSRAbs = 0x4e,
    LSRAbx = 0x5e,

    // NOP (No OPeration)
    NOP = 0xea,

    // ORA (bitwise OR with Accumulator)
    ORAImm = 0x09,
    ORAZpg = 0x05,
    ORAZpx = 0x15,
    ORAAbs = 0x0d,
    ORAAbx = 0x1d,
    ORAAby = 0x19,
    ORAInx = 0x01,
    ORAIny = 0x11,

    // Register Instructions
    TAX = 0xaa,
    TXA = 0x8a,
    DEX = 0xca,
    INX = 0xe8,
    TAY = 0xa8,
    TYA = 0x98,
    DEY = 0x88,
    INY = 0xc8,

    // ROL (ROtate Left)
    ROLAcc = 0x2a,
    ROLZpg = 0x26,
    ROLZpx = 0x36,
    ROLAbs = 0x2e,
    ROLAbx = 0x3e,

    // ROR (ROtate Right)
    RORAcc = 0x6a,
    RORZpg = 0x66,
    RORZpx = 0x76,
    RORAbs = 0x6e,
    RORAbx = 0x7e,

    // RTI (ReTurn from Interrupt)
    RTI = 0x40,

    // RTS (ReTurn from Subroutine)
    RTS = 0x60,

    // SBC (SuBtract with Carry)
    SBCImm = 0xe9,
    SBCZpg = 0xe5,
    SBCZpx = 0xf5,
    SBCAbs = 0xed,
    SBCAbx = 0xfd,
    SBCAby = 0xf9,
    SBCInx = 0xe1,
    SBCIny = 0xf1,

    // STA (STore Accumulator)
    STAZpg = 0x85,
    STAZpx = 0x95,
    STAAbs = 0x8d,
    STAAbx = 0x9d,
    STAAby = 0x99,
    STAInx = 0x81,
    STAIny = 0x91,

    // Stack Instructions
    TXS = 0x9a, // Transfer X to Stack ptr
    TSX = 0xba, // Transfer Stack ptr to X
    PHA = 0x48, // PusH Accumulator
    PLA = 0x68, // PuLl Accumulator
    PHP = 0x08, // PusH Processor status
    PLP = 0x28, // PuLl Processor status

    // STX (STore X register)
    STXZpg = 0x86,
    STXZpy = 0x96,
    STXAbs = 0x8e,

    // STY (STore Y register)
    STYZpg = 0x84,
    STYZpx = 0x94,
    STYAbs = 0x8c,
}
