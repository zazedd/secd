module Main where

import Control.Monad (forM_)
import Data.List (elemIndex)
import Data.Map (Map, empty, insert, lookup, size)
import Lang

type SECD = (Stack, Env, [Instr], Dump, Store)
type Stack = [Val]
type Env = [Val]
type Dump = [(Stack, Env, [Instr])]
type Closure = ([Instr], Env)
type Store = Map Int Closure
type Symtable = [Ident]

data Instr
  = LD Int
  | LDC Int
  | LDF [Instr]
  | LDRF [Instr]
  | AP
  | AA
  | RTN
  | SEL [Instr] [Instr]
  | JOIN
  | ADD
  | SUB
  | MUL
  | HALT
  deriving (Show)

bopToInstr :: BopE -> Instr
bopToInstr op = case op of
  Add -> ADD
  Mul -> MUL
  Sub -> SUB

stripLDF :: [Instr] -> [Instr]
stripLDF [LDF body] = body
stripLDF instr = instr

compile :: Expr -> Symtable -> [Instr]
compile ex sy = cata alg ex sy ++ [HALT]
 where
  alg :: ExprF (Symtable -> [Instr]) -> Symtable -> [Instr]
  alg t sym = case t of
    Int n -> [LDC n]
    Var s -> case elemIndex s sym of
      Just index -> [LD index]
      Nothing -> fail "oops"
    Bop op e1 e2 -> e1 sym ++ e2 sym ++ [bopToInstr op]
    Abs n e -> [LDF (e sym' ++ [RTN])]
     where
      sym' = n : sym
    App e1 e2 -> e1 sym ++ e2 sym ++ [AP]
    IfZ cond e1 e2 -> cond sym ++ [SEL e1' e2']
     where
      e1' = e1 sym ++ [JOIN]
      e2' = e2 sym ++ [JOIN]
    Let n binding e ->
      binding sym ++ [AA] ++ e sym'
     where
      sym' = n : sym
    FixP n e ->
      let result = stripLDF $ e (n : sym)
       in [LDRF (result ++ [RTN])]

next :: Map k a -> Int
next m = size m + 1

-- (Stack, Env, [Instr], Dump, Store)
-- Store = Map Int ([Instr], Env)
step :: SECD -> Maybe SECD
step (s, e, (LD i) : c, d, m) = Just (e !! i : s, e, c, d, m)
step (s, e, (LDC con) : c, d, m) = Just (VInt con : s, e, c, d, m)
step (VInt v2 : VInt v1 : s, e, ADD : c, d, m) = Just (VInt (v1 + v2) : s, e, c, d, m)
step (VInt v2 : VInt v1 : s, e, SUB : c, d, m) = Just (VInt (v1 - v2) : s, e, c, d, m)
step (VInt v2 : VInt v1 : s, e, MUL : c, d, m) = Just (VInt (v1 * v2) : s, e, c, d, m)
step (s, e, (LDF c') : c, d, m) = Just (Addr a : s, e, c, d, m')
 where
  a = next m
  m' = insert a (c', e) m
step (s, e, (LDRF c') : c, d, m) = Just (Addr a : s, e, c, d, m')
 where
  a = next m
  m' = insert a (c', Addr a : e) m
step (v : Addr a : s, e, AP : c, d, m) = do
  (c', e') <- Data.Map.lookup a m
  Just ([], v : e', c', (s, e, c) : d, m)
step (v : _, _, RTN : _, (s', e', c') : d, m) = Just (v : s', e', c', d, m)
step (VInt n : s, e, (SEL c1 c2) : c, d, m)
  | n == 0 = Just (s, e, c1, ([], [], c) : d, m)
  | otherwise = Just (s, e, c2, ([], [], c) : d, m)
step (s, e, JOIN : _, (_, _, c') : d, m) = Just (s, e, c', d, m)
step (b : s, e, AA : c, d, m) = Just (s, b : e, c, d, m)
step (s, e, HALT : _, d, m) = Just (s, e, [], d, m)
step (_, _, _, _, _) = Nothing

eval :: SECD -> Maybe Val
eval (VInt r : _, _, [], _, _) = Just $ VInt r
eval secd = step secd >>= eval

main :: IO ()
main = do
  let
    fact = fixp "fact" (abst "n" (ifz (var "n") (int 1) (bop Mul (var "n") (app (var "fact") (bop Sub (var "n") (int 1))))))
    tests =
      [ app (abst "x" (var "x")) (int 5)
      , letE "x" (int 5) (app (abst "y" (var "y")) (var "x"))
      , letE "x" (int 30) (app (abst "y" (bop Add (var "y") (int 3))) (var "x"))
      , letE "x" (int 3) (ifz (bop Sub (var "x") (var "x")) (var "x") (int 0))
      , letE "x" (int 3) (ifz (bop Sub (var "x") (int 2)) (var "x") (int 0))
      , app fact (int 5)
      ]
  forM_ tests $
    \a -> do
      print a
      print (compile a [])
      print (eval ([], [], compile a [], [], Data.Map.empty))
