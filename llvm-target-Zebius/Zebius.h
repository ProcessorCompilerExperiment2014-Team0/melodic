#ifndef TARGET_ZEBIUS_H
#define TARGET_ZEBIUS_H

#include "MCTargetDesc/SampleMCTargetDesc.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Format.h"

namespace llvm {
  class SampleTargetMachine;
  class FunctionPass;

  FunctionPass *createSampleISelDag(SampleTargetMachine &TM);
} // end namespace llvm;

#endif //TARGET_ZEBIUS_H

