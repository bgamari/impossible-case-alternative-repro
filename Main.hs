module Main ( main ) where

import           Control.Applicative -- for GHC 7.8 compat
import           Module


main :: IO ()
main = print (runJSONParser (liftNewStateT (error "unused state") fun1))

fun1 :: JSON s Int
fun1 = do
  _ <- liftNewStateT undefined fun2
  return (error "will not even get here")

fun2 :: JSON Mytype (Int,())
fun2 =
  do
     x <-
      (\ m_ahgk
      -> do { newContext_ahgl <- fmap mytypeValue Module.getContext;
              Module.withCtx
                "mytypeValue" (liftNewStateT newContext_ahgl m_ahgk) })
      ((

       -- Impossible case alternative
       (do p <- earlyExit <* error "bad"
           return p)

       -- No Impossible case alternative
       --   output with GHC 7.10: Main: will not even get here
       --   output with GHC 7.8:  Left "" (that also seems wrong)
       -- (both using aeson-0.8.0.2)
       -- (earlyExit <* error "bad")

      ) :: JSON Double Int )

     return (x,())

-- {-# NOINLINE fun2 #-} -- fixes the `Impossible case alternative`
