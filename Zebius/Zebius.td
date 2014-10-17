include "llvm/Target/Target.td"

// Registers
class ZebiusReg<bits<5> num, string n> : Register<n> {
  field bits<4> num;
  let Namespace = "Zebius";
}

// GPR's
def R0 : ZebiusReg< 0, "R0">, DwarfRegNum<[0]>;
def R1   : ZebiusReg< 1, "R1">,   DwarfRegNum<[1]>;
def R2   : ZebiusReg< 2, "R2">,   DwarfRegNum<[2]>;
def R3   : ZebiusReg< 3, "R3">,   DwarfRegNum<[3]>;
def R4   : ZebiusReg< 4, "R4">,   DwarfRegNum<[4]>;
def R5   : ZebiusReg< 5, "R5">,   DwarfRegNum<[5]>;
def R6   : ZebiusReg< 6, "R6">,   DwarfRegNum<[6]>;
def R7   : ZebiusReg< 7, "R7">,   DwarfRegNum<[7]>;
def R8   : ZebiusReg< 8, "R8">,   DwarfRegNum<[8]>;
def R9   : ZebiusReg< 9, "R9">,   DwarfRegNum<[9]>;
def R10   : ZebiusReg< 10, "R10">,  DwarfRegNum<[10]>;
def R11   : ZebiusReg< 11, "R11">,  DwarfRegNum<[11]>;
def R12   : ZebiusReg< 12, "R12">,  DwarfRegNum<[12]>;
def R13   : ZebiusReg< 13, "R13">,  DwarfRegNum<[13]>;
def R14   : ZebiusReg< 14, "R14">,  DwarfRegNum<[14]>;
def R15   : ZebiusReg< 15, "R15">,  DwarfRegNum<[15]>;

// global base register
def GBR   : ZebiusReg< 0, "GBR">,  DwarfRegNum<[16]>;

// status register
def SR    : ZebiusReg< 0, "SR">,  DwarfRegNum<[16]>;

// procedure register
def PR    : ZebiusReg< 0, "PR">,  DwarfRegNum<[16]>;

// program counter
def PC    : ZebiusReg< 0, "PC">,  DwarfRegNum<[16]>;

def FR0 : ZebiusReg< 0, "FR0">, DwarfRegNum<[0]>;
def FR1   : ZebiusReg< 1, "FR1">,   DwarfRegNum<[1]>;
def FR2   : ZebiusReg< 2, "FR2">,   DwarfRegNum<[2]>;
def FR3   : ZebiusReg< 3, "FR3">,   DwarfRegNum<[3]>;
def FR4   : ZebiusReg< 4, "FR4">,   DwarfRegNum<[4]>;
def FR5   : ZebiusReg< 5, "FR5">,   DwarfRegNum<[5]>;
def FR6   : ZebiusReg< 6, "FR6">,   DwarfRegNum<[6]>;
def FR7   : ZebiusReg< 7, "FR7">,   DwarfRegNum<[7]>;
def FR8   : ZebiusReg< 8, "FR8">,   DwarfRegNum<[8]>;
def FR9   : ZebiusReg< 9, "FR9">,   DwarfRegNum<[9]>;
def FR10   : ZebiusReg< 10, "FR10">,  DwarfRegNum<[10]>;
def FR11   : ZebiusReg< 11, "FR11">,  DwarfRegNum<[11]>;
def FR12   : ZebiusReg< 12, "FR12">,  DwarfRegNum<[12]>;
def FR13   : ZebiusReg< 13, "FR13">,  DwarfRegNum<[13]>;
def FR14   : ZebiusReg< 14, "FR14">,  DwarfRegNum<[14]>;
def FR15   : ZebiusReg< 15, "FR15">,  DwarfRegNum<[15]>;


def IntRegs : RegisterClass<"Sample", [i32], 32, (add
  R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15)>;

def FloatRegs : RegisterClass<"Sample", [f32], 32, (add
  FR0, FR1, FR2, FR3, FR4, FR5, FR6, FR7, FR8, FR9, FR10, FR11, FR12, FR13, FR14, FR15)>;

class Format<bits<3> val> {
  bits<3> Value = val;
}

def Pseudo    : Format<0>;
def FormDisp  : Format<1>;
def FormTwoRegs  : Format<2>;
def FormReg2  : Format<3>;
def FormReg3  : Format<4>;


// Generic Zebius Format
class ZebiusInst<dag outs, dag ins, string asmstr, list<dag> pattern, InstrItinClass itin, Format f>
      : Instruction
{
  field bits<16> Inst;
  Format Form = f;

  bits<4> Opcode = 0;

  let Namespace = "Zebius";
  let Size = 2;
  let Inst{15-12} = Opcode;
  let OutOperandList = outs;
  let InOperandList  = ins;
  let AsmString   = asmstr;
  let Pattern     = pattern;
  let Itinerary   = itin;

  bits<3> FormBits = Form.Value;

  let DecoderNamespace = "Sample";

  field bits<32> SoftFail = 0;
}

class ZebiusInstDisp<bits<4> op, dag outs, dag ins, string asmstr, list<dag> pattern, InstrItinClass itin>
      : ZebiusInst<outs, ins, asmstr, pattern, itin, FormDisp>
{
  bits<4> rn;
  bits<8> imm;

  let Opcode = op;

  let Inst{11-8} = rn;
  let Inst{7-0}  = imm;
}
class ZebiusInstTwoRegs<bits<4> op, dag outs, dag ins, string asmstr, list<dag> pattern, InstrItinClass itin>
      : ZebiusInst<outs, ins, asmstr, pattern, itin, FormTwoRegs>
{
  bits<4> rn;
  bits<4> rm;
  bits<4> app; // appendix

  let Opcode = op;

  let Inst{11-8} = rn;
  let Inst{7-4}  = rm;
  let Inst{3-0}  = app;
}

