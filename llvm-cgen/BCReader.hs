module BCReader where

import Control.Monad.Except (ExceptT)
import qualified LLVM.General.AST as AST
import LLVM.General.Context
import LLVM.General.Module (File(..), moduleAST, withModuleFromBitcode)

import CPSLift

readBC :: FilePath -> ExceptT String IO AST.Module
readBC filePath = cpsLift withContext $ \ctx ->
  withModuleFromBitcode ctx file $ \cmod -> moduleAST cmod 
   where file = File filePath

