//===-- Zebius.h - Top-level interface for Zebius representation ----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the entry points for global functions defined in
// the LLVM Zebius back-end.
// 
//===----------------------------------------------------------------------===//
#ifndef Zebius_TARGETMACHINE_H
#define Zebius_TARGETMACHINE_H

#include "ZebiusFrameLowering.h"
#include "ZebiusInstrInfo.h"
#include "ZebiusISelLowering.h"
#include "ZebiusSelectionDAGInfo.h"
#include "ZebiusRegisterInfo.h"
#include "ZebiusSubtarget.h"
#include "llvm/DataLayout.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetFrameLowering.h"
#include "llvm/Support/Debug.h"

namespace llvm {

class Module;

class ZebiusTargetMachine : public LLVMTargetMachine {
  const DataLayout DL;
  ZebiusSubtarget Subtarget;
  ZebiusInstrInfo InstrInfo;
  ZebiusFrameLowering FrameLowering;
  ZebiusTargetLowering TLInfo;
  ZebiusSelectionDAGInfo TSInfo;

 public:
  ZebiusTargetMachine(const Target &T, StringRef TT,
                      StringRef CPU, StringRef FS, const TargetOptions &Options,
                      Reloc::Model RM, CodeModel::Model CM,
                      CodeGenOpt::Level OL);

  virtual const ZebiusInstrInfo *getInstrInfo() const {
    return &InstrInfo;
  }
  virtual const ZebiusSubtarget *getSubtargetImpl() const {
    return &Subtarget;
  }
  virtual const ZebiusRegisterInfo *getRegisterInfo() const {
    return &InstrInfo.getRegisterInfo();
  }
  virtual const DataLayout *getDataLayout() const {
    return &DL;
  }
  virtual const ZebiusTargetLowering *getTargetLowering() const {
    return &TLInfo;
  }
  virtual const ZebiusFrameLowering *getFrameLowering() const{
    return &FrameLowering;
  }
  virtual const ZebiusSelectionDAGInfo* getSelectionDAGInfo() const {
    return &TSInfo;
  }

  // Pass Pipeline Configuration
  virtual TargetPassConfig *createPassConfig(PassManagerBase &PM);
};
} // end namespace llvm

#endif

