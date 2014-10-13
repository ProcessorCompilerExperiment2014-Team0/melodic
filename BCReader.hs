module BCReader where

import Control.Monad.Except (ExceptT(ExceptT), runExceptT)
import qualified LLVM.General.AST as AST
import LLVM.General.Context
import LLVM.General.Module (File(..), Module, moduleAST, withModuleFromBitcode)

withContextT :: (Context -> ExceptT e IO a) -> ExceptT e IO a
withContextT action = ExceptT $ withContext (runExceptT . action)

readBC :: FilePath -> ExceptT String IO AST.Module
readBC filePath = withContextT $ \ctx ->
  withModuleFromBitcode ctx file $ \cmod -> moduleAST cmod 
   where file = File filePath
