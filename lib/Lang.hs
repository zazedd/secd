module Lang where

type Ident = String

data ExprF a where
  Var :: Ident -> ExprF a
  Int :: Int -> ExprF a
  Abs :: Ident -> a -> ExprF a
  App :: a -> a -> ExprF a
  Let :: Ident -> a -> a -> ExprF a
  Bop :: BopE -> a -> a -> ExprF a
  IfZ :: a -> a -> a -> ExprF a
  FixP :: Ident -> a -> ExprF a
  deriving (Functor)

data BopE
  = Add
  | Mul
  | Sub

newtype Fix f = Fix {_unFix :: f (Fix f)}

type Expr = Fix ExprF

var :: Ident -> Expr
var n = Fix $ Var n

int :: Int -> Expr
int i = Fix $ Int i

abst :: Ident -> Expr -> Expr
abst n e = Fix $ Abs n e

app :: Expr -> Expr -> Expr
app e1 e2 = Fix $ App e1 e2

letE :: Ident -> Expr -> Expr -> Expr
letE s binding e = Fix $ Let s binding e

bop :: BopE -> Expr -> Expr -> Expr
bop op e1 e2 = Fix $ Bop op e1 e2

ifz :: Expr -> Expr -> Expr -> Expr
ifz cond e1 e2 = Fix $ IfZ cond e1 e2

fixp :: Ident -> Expr -> Expr
fixp n e = Fix $ FixP n e

data Val
  = VInt Int
  | Addr Int
  deriving (Eq, Show)

bopToOp :: BopE -> (Int -> Int -> Int)
bopToOp op = case op of
  Add -> (+)
  Mul -> (*)
  Sub -> (-)

extractInt :: Maybe Val -> Maybe Int
extractInt v = do
  val <- v
  case val of
    VInt i -> Just i
    _ -> Nothing

instance Show Expr where
  show = cata alg
   where
    alg t = case t of
      (Var s) -> s
      (Int i) -> show i
      (Abs s e) -> "(\\" ++ s ++ " -> " ++ e ++ ")"
      (App e1 e2) -> "(" ++ e1 ++ " " ++ e2 ++ ")"
      (Let s binding e) -> "let " ++ s ++ " = " ++ binding ++ " in " ++ e
      (Bop op a b) -> a ++ show op ++ b
      (IfZ cond a b) -> "if " ++ cond ++ " is 0 then " ++ a ++ " else " ++ b
      (FixP n e) -> "fix " ++ n ++ " = " ++ e

instance Show BopE where
  show Add = " + "
  show Mul = " * "
  show Sub = " - "

-- catamorphism/paramorphism operator

cata :: (Functor f) => (f a -> a) -> Fix f -> a
cata alg (Fix f) = alg $ fmap (cata alg) f

para :: (Functor f) => (f (Fix f, b) -> b) -> Fix f -> b
para alg (Fix f) = alg $ fmap (\v -> (v, para alg v)) f
