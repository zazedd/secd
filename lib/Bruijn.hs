module Bruijn where

import Control.Monad.Except
import Control.Monad.State
import Data.Char (chr)
import Data.List (nub, union, (\\))
import qualified Data.Map as Map

data Expr a where
  Var :: Int -> Expr a
  Int :: Int -> Expr Int
  Abs :: Expr b -> Expr (a -> b)
  App :: Expr (a -> b) -> Expr a -> Expr b
  Let :: Expr a -> Expr b -> Expr b
  Bop :: BopE -> Expr Int -> Expr Int -> Expr Int
  Op :: OpE -> Expr a -> Expr a
  IfZ :: Expr Int -> Expr a -> Expr a -> Expr a
  FixP :: Expr (a -> a) -> Expr a
  Tup :: Expr a -> Expr b -> Expr (a, b)
  Variant :: (String, Expr a) -> Expr a
  Match :: (Expr a, [(String, Expr a)]) -> Expr a

data BopE
  = Add
  | Mul
  | Sub

bopToOp :: BopE -> (Int -> Int -> Int)
bopToOp op = case op of
  Add -> (+)
  Mul -> (*)
  Sub -> (-)

data OpE
  = Fst
  | Snd

instance Show (Expr Int) where
  show = aux
   where
    aux :: Expr a -> String
    aux t =
      case t of
        Int x -> show x
        Var idx -> "(VAR: " ++ show idx ++ ")"
        Abs e -> "(\\_ -> " ++ aux e ++ ")"
        App e1 e2 -> "(" ++ aux e1 ++ " " ++ aux e2 ++ ")"
        Let e1 e2 -> "let _ = " ++ aux e1 ++ " in " ++ aux e2
        Bop op e1 e2 -> "(" ++ aux e1 ++ show op ++ aux e2 ++ ")"
        Op op e -> "(" ++ show op ++ aux e ++ ")"
        IfZ e1 e2 e3 -> "if " ++ aux e1 ++ " is 0 then " ++ aux e2 ++ " else " ++ aux e3
        FixP e -> "fix of " ++ aux e
        Tup a b -> "(" ++ aux a ++ ", " ++ aux b ++ ")"
        Variant (name, v) -> "<" ++ name ++ " " ++ aux v ++ ">"
        Match (vic, alts) ->
          "match " ++ aux vic ++ " with " ++ showAlts alts ++ ">"
         where
          showAlts [] = ""
          showAlts [(name, e)] = name ++ " -> " ++ aux e
          showAlts ((name, e) : xs) = name ++ " -> " ++ aux e ++ " | " ++ showAlts xs

instance Show BopE where
  show Add = " + "
  show Mul = " * "
  show Sub = " - "

instance Show OpE where
  show Fst = "fst "
  show Snd = "snd "
