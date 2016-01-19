{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main
  ) where

import           Control.Applicative -- for GHC 7.8 compat
import           System.Environment (getArgs)
import           Module
import qualified Data.List.NonEmpty as NE

import Debug.Trace

main :: IO ()
main = do
  print (runJSONParser (liftNewStateT (error "unused state") fun1))

fun1 :: JSON Mytype Float
fun1 =
  do
     ints <- liftNewStateT undefined fun2NoTH

     _ <- traceShow "before eval" $ error (show ints)

     undefined

fun2NoTH :: JSON Mytype Int
fun2NoTH = do
  x <-  (\ m_aeOm -> do { newContext_aeOn <- fmap mytypeValue2 Module.getContext;
                         Module.withCtx "mytypeValue2" (liftNewStateT newContext_aeOn m_aeOm) })

        ((\ m_aeOv -> do { newContext_aeOw <- fmap mytype2Value Module.getContext;
                          Module.withCtx "mytype2Value" (liftNewStateT newContext_aeOw m_aeOv) })

         (exitWith Notification)) -- segfaults
  return x

fun2 :: JSON Mytype Int
fun2 = do
  x <- $(inside 'mytypeValue2) ($(inside 'mytype2Value) (exitWith Notification)) -- segfaults
  return x

  -- x <- $(inside 'mytypeValue2) (exitWith Notification)                      -- going only until depth 1: doesn't segfault
  -- return x

  -- $(inside 'mytypeValue2) ($(inside 'mytype2Value) (exitWith Notification)) -- not using do notation: doesn't segfault
