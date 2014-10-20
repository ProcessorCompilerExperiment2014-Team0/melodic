module Id where

import Data.String
import Control.Monad.State

-- Names of variables.
data Id = Id !String
-- Names of top-level functions and/or global arrays.
data LId = LId !String

instance IsString Id where
  fromString = Id
instance IsString LId where
  fromString = LId

type CounterT = StateT Int

genId :: Monad m => String -> CounterT m String
genId str = do
  x <- get
  put (x + 1)
  return $ str ++ "." ++ show x

{-
let rec id_of_typ = function
  | Type.Unit -> "u"
  | Type.Bool -> "b"
  | Type.Int -> "i"
  | Type.Float -> "d"
  | Type.Fun _ -> "f"
  | Type.Tuple _ -> "t"
  | Type.Array _ -> "a" 
  | Type.Var _ -> assert false
let gentmp typ =
  incr counter;
  Printf.sprintf "T%s%d" (id_of_typ typ) !counter
-}

