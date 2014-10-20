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


def IntRegs : RegisterClass<"Zebius", [i32], 32, (add
  R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15)>;

def FloatRegs : RegisterClass<"Zebius", [f32], 32, (add
  FR0, FR1, FR2, FR3, FR4, FR5, FR6, FR7, FR8, FR9, FR10, FR11, FR12, FR13, FR14, FR15)>;

class Format<bits<3> val> {
  bits<3> Value = val;
}

def Pseudo    : Format<0>;
def FormDisp  : Format<1>;
def FormTwoRegs  : Format<2>;
def FormReg2  : Format<3>;
def FormReg3  : Format<4>;

//===----------------------------------------------------------------------===//
// Functional units
//===----------------------------------------------------------------------===//

def ALU     : FuncUnit;

//===----------------------------------------------------------------------===//
// Instruction Itinerary classes
//===----------------------------------------------------------------------===//
def IICAlu    : InstrItinClass;
def IICLoad   : InstrItinClass;
def IICStore  : InstrItinClass;
def IICBranch : InstrItinClass;
def IICPseudo : InstrItinClass;

//===----------------------------------------------------------------------===//
// Zebius Generic instruction itineraries.
//===----------------------------------------------------------------------===//

def ZebiusGenericItineraries : ProcessorItineraries<[ALU], [], [
    InstrItinData<IICAlu    , [InstrStage<1,  [ALU]>]>,
    InstrItinData<IICLoad   , [InstrStage<1,  [ALU]>]>,
    InstrItinData<IICStore  , [InstrStage<1,  [ALU]>]>,
    InstrItinData<IICBranch , [InstrStage<1,  [ALU]>]>,
    InstrItinData<IICPseudo , [InstrStage<1,  [ALU]>]>
]>;

//===----------------------------------------------------------------------===//
// Zebius Operand Definitions.
//===----------------------------------------------------------------------===//

// load/storeで利用するオペランド.20bit
// 19-16: CPURegs Operand(base)
// 15-0 : 符号付き16bit整数(offset)
// printMethod: base(offset) 形式で出力
// EncoderMethod: bit列から2つの値を取得
def mem : Operand<i32> {
  let PrintMethod = "printMemOperand";
  let MIOperandInfo = (ops IntRegs, i16imm);
  let EncoderMethod = "getMemEncoding";
}

// 即値ロード用のオペランド. 20bit
// 19-0: 符号付き20bit整数
// EncoderMethod: bit列から符号付き20bit整数を取得
def movetarget : Operand<i32> {
  let EncoderMethod = "getMoveTargetOpValue";
}

def calltarget  : Operand<iPTR> {
  let EncoderMethod = "getCallTargetOpValue";
}

//===----------------------------------------------------------------------===//
// Zebius Complex Pattern Definitions.
//===----------------------------------------------------------------------===//

def addr : ComplexPattern<iPTR, 2, "SelectAddr", [], []>;

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

  let DecoderNamespace = "Zebius";

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

class ZebiusInstArith<bits<4> subop, string asmstr , SDNode OpNode , InstrItinClass itin, RegisterClass RC>
      : ZebiusInstTwoRegs <0x3, ( outs RC : $rn ) , ( ins RC : $rm , RC : $rn ), !strconcat ( asmstr, "\t$rm , $rn" ) , [( set RC : $rn , ( OpNode RC : $rm , RC : $rn ))] , itin >
{
  let app = subop;
}

/*
class Call<bits<8> op, string asmstr>:
  ZebiusInstFormReg0<op, (outs), (ins calltarget:$other, variable_ops),
     !strconcat(asmstr, "\t$other"), [(ZebiusCall imm:$other)],
     IICBranch> {
  let isCall=1;
  let DecoderMethod = "DecodeCallTarget";
}
*/
//def ADD : ZebiusInstArith <0xc , "ADD" , add , IICAlu , IntRegs >;

def LOAD : ZebiusInstTwoRegs<0x6, (outs IntRegs:$rn), (ins mem:$rm), "MOV.L\t$rm, @$rn", [(set IntRegs:$rn, (load addr:$rm))], IICLoad>
{
  let app = 0x2;
}
def STORE : ZebiusInstTwoRegs<0x2, (outs IntRegs:$rn), (ins mem:$rm), "MOV.L\t$rm, @$rn", [(store IntRegs:$rn, addr:$rm)], IICStore>
{
  let app = 0x2;
}

def MOV : ZebiusInstTwoRegs<0x6, (outs IntRegs:$rn), (ins IntRegs:$rm), "MOV\t$rm, $rn", [(set IntRegs:$rn, IntRegs:$rm)], IICAlu>
{
  let app = 0x3;
}

//===----------------------------------------------------------------------===//
// Zebius profiles and nodes
//===----------------------------------------------------------------------===//

// SDTypeProfileはSDNodeの必要条件を定義する。
// 第一引数: 結果Node数
// 第二引数: オペランドNode数
// 第三引数: 制約条件(SDTypeConstraint)

// SDTCisInt<N>: N番目のオペランドはInt型
// SDTCisVT<N, VT>: N番目のオペランドはVT型

// SDNodeは新しいSDNodeを定義する。
// 第一引数: opcode
// 第二引数: 制約条件(SDTypeProfile)
// 第三引数: SDNodeProperty

// SDNodeProperty
// SDNPCommutative : // 可換
// SDNPAssociative : // 結合法則
// SDNPHasChain    : // R/W chain operand and result
// SDNPOutGlue     : // Write a flag result
// SDNPInGlue      : // Read a flag operand
// SDNPOptInGlue   : // Optionally read a flag operand
// SDNPMayStore    : // May write to memory, sets 'mayStore'.
// SDNPMayLoad     : // May read memory, sets 'mayLoad'.
// SDNPSideEffect  : // Sets 'HasUnmodelledSideEffects'.
// SDNPMemOperand  : // Touches memory, has assoc MemOperand
// SDNPVariadic    : // 可変引数を持つ
// SDNPWantRoot    : // ComplexPattern gets the root of match
// SDNPWantParent  : // ComplexPattern gets the parent

def SDT_ZebiusRet          : SDTypeProfile<0, 1, [SDTCisInt<0>]>;
def ZebiusRet : SDNode<"ZebiusISD::Ret", SDT_ZebiusRet, 
                       [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;

def SDT_ZebiusCall      : SDTypeProfile<0, 1, [SDTCisVT<0, iPTR>]>;

def ZebiusCall : SDNode<"ZebiusISD::Call",SDT_ZebiusCall,
                         [SDNPHasChain, SDNPOutGlue, SDNPOptInGlue,
                          SDNPVariadic]>;

def SDT_ZebiusCallSeqStart : SDCallSeqStart<[SDTCisVT<0, i32>]>;
def SDT_ZebiusCallSeqEnd   : SDCallSeqEnd<[SDTCisVT<0, i32>, SDTCisVT<1, i32>]>;

def callseq_start : SDNode<"ISD::CALLSEQ_START", SDT_ZebiusCallSeqStart,
                           [SDNPHasChain, SDNPSideEffect, SDNPOutGlue]>;
def callseq_end   : SDNode<"ISD::CALLSEQ_END", SDT_ZebiusCallSeqEnd,
                           [SDNPHasChain, SDNPSideEffect,
                            SDNPOptInGlue, SDNPOutGlue]>;


def ZebiusInstrInfo : InstrInfo;

def : Processor<"Zebius16", ZebiusGenericItineraries, []>;

def ZebiusAsmWriter : AsmWriter {
  string AsmWriterClassName  = "InstPrinter";
  bit isMCAsmWriter = 1;
}

def Zebius : Target {
  let InstructionSet = ZebiusInstrInfo;
  let AssemblyWriters = [ZebiusAsmWriter];
}
