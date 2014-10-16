include "llvm/Target/Target.td"

// Registers
class ZebiusReg<bits<5> num, string n> : Register<n> {
  field bits<4> num;
  let Namespace = "Zebius";
}

// GPR's
def R0 : SampleReg< 0, "R0">, DwarfRegNum<[0]>;
def R1   : SampleReg< 1, "R1">,   DwarfRegNum<[1]>;
def R2   : SampleReg< 2, "R2">,   DwarfRegNum<[2]>;
def R3   : SampleReg< 3, "R3">,   DwarfRegNum<[3]>;
def R4   : SampleReg< 4, "R4">,   DwarfRegNum<[4]>;
def R5   : SampleReg< 5, "R5">,   DwarfRegNum<[5]>;
def R6   : SampleReg< 6, "R6">,   DwarfRegNum<[6]>;
def R7   : SampleReg< 7, "R7">,   DwarfRegNum<[7]>;
def R8   : SampleReg< 8, "R8">,   DwarfRegNum<[8]>;
def R9   : SampleReg< 9, "R9">,   DwarfRegNum<[9]>;
def R10   : SampleReg< 10, "R10">,  DwarfRegNum<[10]>;
def R11   : SampleReg< 11, "R11">,  DwarfRegNum<[11]>;
def R12   : SampleReg< 12, "R12">,  DwarfRegNum<[12]>;
def R13   : SampleReg< 13, "R13">,  DwarfRegNum<[13]>;
def R14   : SampleReg< 14, "R14">,  DwarfRegNum<[14]>;
def R15   : SampleReg< 15, "R15">,  DwarfRegNum<[15]>;

// global base register
def GBR   : SampleReg< 0, "GBR">,  DwarfRegNum<[16]>;

// status register
def SR    : SampleReg< 0, "SR">,  DwarfRegNum<[16]>;

// procedure register
def PR    : SampleReg< 0, "PR">,  DwarfRegNum<[16]>;

// program counter
def PC    : SampleReg< 0, "PC">,  DwarfRegNum<[16]>;

def FR0 : SampleReg< 0, "FR0">, DwarfRegNum<[0]>;
def FR1   : SampleReg< 1, "FR1">,   DwarfRegNum<[1]>;
def FR2   : SampleReg< 2, "FR2">,   DwarfRegNum<[2]>;
def FR3   : SampleReg< 3, "FR3">,   DwarfRegNum<[3]>;
def FR4   : SampleReg< 4, "FR4">,   DwarfRegNum<[4]>;
def FR5   : SampleReg< 5, "FR5">,   DwarfRegNum<[5]>;
def FR6   : SampleReg< 6, "FR6">,   DwarfRegNum<[6]>;
def FR7   : SampleReg< 7, "FR7">,   DwarfRegNum<[7]>;
def FR8   : SampleReg< 8, "FR8">,   DwarfRegNum<[8]>;
def FR9   : SampleReg< 9, "FR9">,   DwarfRegNum<[9]>;
def FR10   : SampleReg< 10, "FR10">,  DwarfRegNum<[10]>;
def FR11   : SampleReg< 11, "FR11">,  DwarfRegNum<[11]>;
def FR12   : SampleReg< 12, "FR12">,  DwarfRegNum<[12]>;
def FR13   : SampleReg< 13, "FR13">,  DwarfRegNum<[13]>;
def FR14   : SampleReg< 14, "FR14">,  DwarfRegNum<[14]>;
def FR15   : SampleReg< 15, "FR15">,  DwarfRegNum<[15]>;


def IntRegs : RegisterClass<"Sample", [i32], 32, (add
  R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15)>;

def FloatRegs : RegisterClass<"Sample", [f32], 32, (add
  FR0, FR1, FR2, FR3, FR4, FR5, FR6, FR7, FR8, FR9, FR10, FR11, FR12, FR13, FR14, FR15)>;

class Format<bits<3> val> {
  bits<3> Value = val;
}

def Pseudo    : Format<0>;
def FormDisp  : Format<1>;
def FormReg1  : Format<2>;
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
  let Inst{7-4}  = imm;
  let Inst{3-0}  = app;
}

