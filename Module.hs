{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module was derived from a real application that needs a custom JSON
-- parser/validator based on `aeson`.

-- It uses a `JSON` monad transformer to enable some early returns and keep track
-- of some state (see `newtype JSON`).

-- After parsing some input JSON into a Haskell data type (reduced to `Mytype`
-- for this repro) that is 1:1 analogous in structure to the input JSON, the
-- Haskell data type would be traversed a couple times to validate a set of
-- policies. When one of the policies is not fulfilled, a corresponding notification
-- message should mention the relevant path in the input JSON structure;
-- that's what the `jsonStack` in the `JSONState` is good for. When traversing
-- this data type, we want to be able to traverse only into fields that the data
-- type really has, and we use TemplateHaskell to ensure this.

-- For example, when you are in a `JSON Mytype a`, you can use
--   `$(inside 'mytypeValue) $ do ...`
-- to step down into the `mytypeValue` field of the current context
-- (`Mytype ~ context` in `JSON context`); this pushes the
-- path to `mytypeValue` onto the `jsonStack`.
module Module
  ( JSON
  , liftNewStateT
  , runJSONParser
  , inside
  , earlyExit
  , exitWith
  , Mytype(mytypeValue,mytypeValue2)
  , Mytype2(mytype2Value)
  , Notification(..)
  , liftStateT -- Not exporting any of `liftStateT`/`notify`
  , notify     -- fixes the segfault.
  ) where

import           Control.Applicative -- for GHC 7.8 compat
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Except
import           Data.Aeson.Types as X hiding (withObject,withArray)
import           Data.Proxy
import           GHC.TypeLits
import           Language.Haskell.TH


data Notification = Notification deriving Show

-- Monad transformer around `aeson`'s `Parser`:
--   - ExceptT () for early returns (see `earlyExit`)
--   - StateT (JSONState context) to keep track of where in the data
--     structure we are (`context`) and what our path to that was `[()]`.
newtype JSON context a = JSON { runJSON :: ExceptT () (StateT (JSONState context) Parser) a }
  deriving (Monad,Applicative,Functor)

data JSONState context = JSONState
  { jsonStack          :: [String]
  , jsonNotifications  :: [Notification]
  , jsonContext        :: context
  }


data Mytype = Mytype { mytypeValue :: !Double, mytypeValue2 :: !Mytype2 }
  deriving (Eq,Show)

data Mytype2 = Mytype2 { mytype2Value :: !Float }
  deriving (Eq,Show)


pushStack :: String -> [String] -> [String]
pushStack x xs = x : xs -- segfault
-- pushStack x xs =      xs -- No segfault


-- Uses TH to build an expression of type:
--   $(inside 'recordField) :: JSON (recordField record) b -> JSON record b
-- For example:
--   $(inside 'mytypeValue) :: JSON Double b -> JSON Mytype b
inside :: Name -> Q Exp
inside recordField =
  do VarI _ (AppT (AppT ArrowT (ConT _ty)) _) _ <- reify recordField
     let field = stringE (nameBase recordField)
     [|\m ->
         do newContext <- fmap $(varE recordField) getContext
            withCtx $field (liftNewStateT newContext m) |]

getContext :: JSON context context
getContext = JSON (ExceptT (StateT (\s -> return (Right (jsonContext s),s))))


runJSONParser :: JSON () a -> Either String (Either () a)
runJSONParser f =
  case parseEither
         (const (runStateT (runExceptT (runJSON f))
                           (JSONState [] [] ())))
         () of
    Right (result,_) -> return result
    Left err -> Left err

-- | Lift a state action into our monad.
liftStateT :: StateT (JSONState context) Parser a -> JSON context a
liftStateT = JSON . lift

earlyExit :: JSON context a
earlyExit = JSON (throwE ())

notify :: Notification
       -> JSON s ()
notify n =
  liftStateT
    (modify (\s -> s {jsonNotifications = n : jsonNotifications s}))

exitWith :: Notification -> JSON s a
exitWith n = do
  notify n
  earlyExit

liftNewStateT :: newcontext -> JSON newcontext a -> JSON context a
liftNewStateT newcontext m =
  JSON (ExceptT (StateT (\(JSONState stack ns orig) ->
                           do (result,JSONState _ ns' _) <-
                                runStateT (runExceptT (runJSON m))
                                          (JSONState stack ns newcontext)
                              return (result,JSONState stack ns' orig))))

withCtx :: String -> JSON context a -> JSON context a
withCtx field m =
  do s <- liftStateT get
     liftStateT (put s { jsonStack = pushStack field (jsonStack s) })
     v <- m
     s' <- liftStateT get
     liftStateT (put s' { jsonStack = jsonStack s })
     return v
