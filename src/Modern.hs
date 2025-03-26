module Main where

import Bruijn
import Control.Monad (forM_)
import Debug.Trace (trace)

data Val
  = VInt Int
  | VClos ([Instr], Env)
  | VFixClos ([Instr], Env) -- recursive closure
  | VFixCClos ([Instr], Env) -- recursive closure, with control point
  | VTuple (Val, Val)
  | VVariant String Val

instance Show Val where
  show v = case v of
    VInt n -> show n
    VClos _ -> "<closure>"
    VFixClos _ -> "<recursive closure>"
    VFixCClos _ -> "<recursive control closure>"
    VTuple (a, b) -> "(" ++ show a ++ ", " ++ show b ++ ")"
    VVariant name v' -> "<" ++ name ++ " " ++ show v' ++ ">"

-- this implementation combines the stack and the dump into one
-- also called the CES machine, for Code pointer, Env, Stack
type CES = (Code, Env, Stack)
type Code = [Instr]
type Stack = [Val]
type Env = [Val]

data Instr
  = LD Int -- load variable
  | LDC Int -- load constant
  | CLO [Instr]
  | FIX [Instr] -- recursive closure
  | FIXC [Instr] -- recursive closure with control point
  | LET
  | ENDLET
  | AP
  | TAP -- tail application
  | RTN
  | IF [Instr] [Instr]
  | ADD
  | MUL
  | SUB
  | VARIANT String
  | MATCH [(String, [Instr])]
  | TUP
  | FST
  | SND
  | HALT
  deriving (Show)

bopToInstr :: BopE -> Instr
bopToInstr op = case op of
  Add -> ADD
  Mul -> MUL
  Sub -> SUB

opToInstr :: OpE -> Instr
opToInstr op = case op of
  Fst -> FST
  Snd -> SND

compileNonTail :: Expr a -> [Instr]
compileNonTail ex = c ex ++ [HALT]
 where
  c :: Expr a -> [Instr]
  c expr = case expr of
    Var n -> [LD n]
    Int n -> [LDC n]
    Abs e -> [CLO (c e ++ [RTN])]
    Let binding e -> c binding ++ [LET] ++ c e ++ [ENDLET]
    App e1 e2 -> c e1 ++ c e2 ++ [AP]
    Bop op e1 e2 -> c e1 ++ c e2 ++ [bopToInstr op]
    Op op e -> c e ++ [opToInstr op]
    IfZ cond e1 e2 -> c cond ++ [IF (c e1 ++ [RTN]) (c e2 ++ [RTN])]
    FixP (Abs (Abs x)) -> [FIX (c x ++ [RTN])]
    FixP (Abs e) -> [FIXC (c e ++ [RTN])]
    FixP _ -> fail "invalid fixpoint"
    Tup a b -> c a ++ c b ++ [TUP]
    Variant (tag, e) -> c e ++ [VARIANT tag]
    Match (vic, alts) ->
      let vic' = c vic
          branches = map (\(name, branch) -> (name, c branch)) alts
       in vic' ++ [MATCH branches]

-- Variant (name, e) -> c e ++ [VARIANT name]
-- Match (e, with) ->

compile :: Expr a -> Either String [Instr]
compile ex = c ex >>= \v -> return $ v ++ [HALT]
 where
  -- regular compilation
  c :: Expr a -> Either String [Instr]
  c = \case
    Var n -> Right [LD n]
    Int n -> Right [LDC n]
    Abs e -> t e >>= \e_t' -> return [CLO e_t'] -- check for tail call here
    Let binding e -> do
      b <- c binding
      e' <- c e
      return $ b ++ [LET] ++ e' ++ [ENDLET]
    App e1 e2 -> do
      e1' <- c e1
      e2' <- c e2
      return $ e1' ++ e2' ++ [AP]
    Bop op e1 e2 -> do
      e1' <- c e1
      e2' <- c e2
      return $ e1' ++ e2' ++ [bopToInstr op]
    Op op e -> do
      e' <- c e
      return $ e' ++ [opToInstr op]
    -- we should check for tail calls for both fixs, and the branches of the if too
    IfZ cond e1 e2 -> do
      cond' <- c cond
      e1' <- t e1
      e2' <- t e2
      return $ cond' ++ [IF e1' e2']
    FixP (Abs (Abs x)) -> t x >>= \x' -> return [FIX x']
    FixP (Abs e) -> t e >>= \e' -> return [FIXC e']
    FixP _ -> Left "Invalid fixpoint"
    Tup a b -> do
      a' <- c a
      b' <- c b
      return $ a' ++ b' ++ [TUP]
    Variant (tag, e) -> do
      e' <- c e
      return $ e' ++ [VARIANT tag]
    Match (vic, alts) -> do
      vic' <- c vic
      branches <-
        mapM
          ( \(name, branch) -> do
              branch' <- c branch
              return (name, branch')
          )
          alts
      return $ vic' ++ [MATCH branches]
  t :: Expr a -> Either String [Instr]
  t expr =
    case expr of
      Let binding e -> res
       where
        res = do
          b' <- c binding
          e' <- t e
          return $ b' ++ [LET] ++ e'
      App e1 e2 -> do
        e1' <- c e1
        e2' <- c e2
        return $ e1' ++ e2' ++ [TAP]
      a -> c a >>= \a' -> return $ a' ++ [RTN]

step :: Int -> CES -> Either String CES
step i secd@(code, _, stack) =
  -- trace ("STACK " ++ show i ++ ": " ++ show stack) $
  -- trace ("STACK SIZE #" ++ show i ++ ": " ++ show (length stack)) $
  -- trace ("CODE " ++ show i ++ ": " ++ show code) $
  case secd of
    -- variables/constants
    (LD n : c, e, s) -> Right (c, e, e !! n : s)
    (LDC n : c, e, s) -> Right (c, e, VInt n : s)
    -- Let
    (LET : c, e, v : s) -> Right (c, v : e, s)
    (ENDLET : c, _ : e, s) -> Right (c, e, s)
    -- closure and recursive closures
    (CLO c' : c, e, s) -> Right (c, e, s')
     where
      closure = VClos (c', e)
      s' = closure : s
    (FIX c' : c, e, s) -> Right (c, e, VFixClos (c', e) : s)
    (FIXC c' : c, e, s) -> Right (c, e, VFixCClos (c', e) : s)
    -- application
    (AP : c, e, v : VClos (c', e') : s) -> Right (c', v : e', VClos (c, e) : s)
    (AP : c, e, v : VFixClos (c', e') : s) -> Right (c', v : VFixClos (c', e') : e', VClos (c, e) : s)
    (AP : c, e, VFixCClos (c', e') : s) -> Right (c', VFixClos (c', e') : e', VClos (c, e) : s)
    -- tail application, we dont bother to push a closure into the stack
    (TAP : _, _, v : VClos (c', e') : s) -> Right (c', v : e', s)
    (TAP : _, _, v : VFixClos (c', e') : s) -> Right (c', v : VFixCClos (c', e') : e', s)
    (TAP : _, _, v : VFixCClos (c', e') : s) -> Right (c', v : VFixClos (c', e') : e', s)
    -- return
    (RTN : _, _, v : VClos (c', e') : s) -> Right (c', e', v : s)
    -- binary operations
    (ADD : c, e, VInt v2 : VInt v1 : s) -> Right (c, e, VInt (v1 + v2) : s)
    (SUB : c, e, VInt v2 : VInt v1 : s) -> Right (c, e, VInt (v1 - v2) : s)
    (MUL : c, e, VInt v2 : VInt v1 : s) -> Right (c, e, VInt (v1 * v2) : s)
    -- tuples
    (TUP : c, e, v2 : v1 : s) -> Right (c, e, VTuple (v1, v2) : s)
    (FST : c, e, VTuple (v1, _) : s) -> Right (c, e, v1 : s)
    (SND : c, e, VTuple (_, v2) : s) -> Right (c, e, v2 : s)
    -- if
    (IF c1 c2 : c, e, VInt n : s) -> Right (if n == 0 then c1 else c2, e, VClos (c, e) : s)
    -- variants
    (VARIANT name : c, e, v : s) -> Right (c, e, VVariant name v : s)
    (MATCH branches : _, e, VVariant name v : s) ->
      case lookup name branches of
        Just c' -> Right (c', v : e, s)
        Nothing -> Left $ "No matching branch for tag: " ++ name
    -- end
    (HALT : _, e, s) -> Right ([], e, s)
    (_, _, _) -> Left "Unrecognized instruction, or wrong stack/environment"

eval :: Int -> CES -> Either String Val
eval _ ([], _, VInt i : _) = Right $ VInt i
eval i ces = step i ces >>= eval (i + 1)

makeCES :: [Instr] -> CES
makeCES c = (c, [], [])

main :: IO ()
main = do
  let
    fact :: Expr (Int -> Int)
    fact = FixP (Abs (Abs (IfZ (Var 0) (Int 1) (Bop Mul (Var 0) (App (Var 1) (Bop Sub (Var 0) (Int 1)))))))
    fact_tail :: Expr (Int -> Int -> Int)
    fact_tail =
      FixP
        ( Abs -- f
            ( Abs -- n
                ( Abs -- acc
                    ( IfZ
                        (Var 1) -- if n == 0, then
                        (Var 0) -- acc, else
                        (App (App (Var 2) (Bop Sub (Var 1) (Int 1))) (Bop Mul (Var 0) (Var 1))) -- f (n - 1) (acc * n)
                    )
                )
            )
        )
    fact_tail_pair :: Expr ((Int -> Int) -> (Int -> Int))
    fact_tail_pair =
      FixP
        ( Abs -- f
            ( Abs $ -- (n, acc)
                Let (Op Fst (Var 0)) $ -- n
                  Let
                    (Op Snd (Var 1)) -- acc
                    ( IfZ
                        (Var 1) -- if n == 0, then
                        (Var 0) -- acc, else
                        (Var 3 `App` Tup (Bop Sub (Var 1) (Int 1)) (Bop Mul (Var 0) (Var 1))) -- f (n - 1, acc * n)
                    )
            )
        )
    recursive_sum =
      FixP
        ( Abs -- f
            ( Abs -- n
                ( IfZ
                    (Var 0) -- if n == 0, then
                    (Int 0) -- acc, else
                    (Bop Add (Bop Mul (Var 0) (Var 0)) (App (Var 1) (Bop Sub (Var 0) (Int 1))))
                )
            )
        )
    len =
      FixP
        ( Abs
            ( Abs
                ( Abs
                    ( Match
                        ( Var 1 -- pattern–match on the list
                        ,
                          [ ("Nil", Var 1) -- if the list is empty, return the accumulator
                          ,
                            ( "Cons"
                            , App
                                ( App
                                    (Var 3)
                                    (Op Snd (Var 0)) -- recursively call the function with tail
                                )
                                (Bop Add (Var 1) (Int 1)) -- and accumulator + 1
                            )
                          ]
                        )
                    )
                )
            )
        )
    tests =
      [ App (Abs (Var 0)) (Int 5)
      , Let (Int 5) (App (Abs (Var 0)) (Var 0))
      , Let (Int 30) (App (Abs (Bop Add (Var 0) (Int 3))) (Var 0))
      , Let (Int 3) (IfZ (Bop Sub (Var 0) (Var 0)) (Var 0) (Int 0))
      , Let (Int 3) (IfZ (Bop Sub (Var 0) (Int 1)) (Var 0) (Int 0))
      , Let
          (Int 10)
          ( Let
              (Bop Add (Var 0) (Int 20))
              ( Let
                  (Bop Mul (Var 0) (Int 2))
                  ( App (Abs (Bop Add (Var 0) (Int 1))) (Var 0)
                  )
              )
          )
      , Let fact (App (Var 0) (Int 5))
      , Let fact_tail (App (App (Var 0) (Int 5)) (Int 1))
      , Let fact_tail_pair $ App (Var 0) (Tup (Int 5) (Int 1))
      , recursive_sum `App` Int 2
      , Match
          ( Variant ("Some", Int 42)
          ,
            [ ("Some", Var 0)
            , ("None", Int 0)
            ]
          )
      , -- length of 3-element-long list, should = 3
        ( len
            `App` Variant
              ( "Cons"
              , Tup
                  (Int 1)
                  ( Variant
                      ( "Cons"
                      , Tup
                          (Int 2)
                          ( Variant
                              ( "Cons"
                              , Tup
                                  (Int 3)
                                  (Variant ("Nil", Int 0))
                              )
                          )
                      )
                  )
              )
        )
          `App` Int 0
      ]
  forM_ tests $
    \a -> do
      print a
      print (eval 0 (makeCES (compileNonTail a)))
      print (compile a >>= \c -> return $ eval 0 (makeCES c))
