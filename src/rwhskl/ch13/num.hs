-- 我们支持的操作符
data Op = Plus | Minus | Mul | Div | Pow
  deriving (Eq, Show)

{- 核心符号操作类型（core symbolic manipulation type） -}
data SymbolicManip a
  = Number a -- Simple number, such as 5
  | Symbol String -- A symbol, such as x
  | BinaryArith Op (SymbolicManip a) (SymbolicManip a)
  | UnaryArith String (SymbolicManip a)
  deriving (Eq)

{- SymbolicManip 是 Num 的实例。定义 SymbolicManip 实现 Num 的函数。如(+)等。 -}
instance Num a => Num (SymbolicManip a) where
  a + b = BinaryArith Plus a b
  a - b = BinaryArith Minus a b
  a * b = BinaryArith Mul a b
  negate a = BinaryArith Mul (Number (-1)) a
  abs a = UnaryArith "abs" a
  signum _ = error "signum is unimplemented"
  fromInteger i = Number (fromInteger i)

{- 定义 SymbolicManip 为 Fractional 实例 -}
instance (Fractional a) => Fractional (SymbolicManip a) where
  a / b = BinaryArith Div a b
  recip a = BinaryArith Div (Number 1) a
  fromRational r = Number (fromRational r)

{- 定义 SymbolicManip 为 Floating 实例 -}
instance (Floating a) => Floating (SymbolicManip a) where
  pi = Symbol "pi"
  exp a = UnaryArith "exp" a
  log a = UnaryArith "log" a
  sqrt a = UnaryArith "sqrt" a
  a ** b = BinaryArith Pow a b
  sin a = UnaryArith "sin" a
  cos a = UnaryArith "cos" a
  tan a = UnaryArith "tan" a
  asin a = UnaryArith "asin" a
  acos a = UnaryArith "acos" a
  atan a = UnaryArith "atan" a
  sinh a = UnaryArith "sinh" a
  cosh a = UnaryArith "cosh" a
  tanh a = UnaryArith "tanh" a
  asinh a = UnaryArith "asinh" a
  acosh a = UnaryArith "acosh" a
  atanh a = UnaryArith "atanh" a

{- 使用常规代数表示法，把 SymbolicManip 转换为字符串 -}
prettyShow :: (Show a, Num a) => SymbolicManip a -> String
-- 显示字符或符号
prettyShow (Number x) = show x
prettyShow (Symbol x) = x
prettyShow (BinaryArith op a b) =
  let pa = simpleParen a
      pb = simpleParen b
      pop = op2str op
   in pa ++ pop ++ pb
prettyShow (UnaryArith opstr a) =
  opstr ++ "(" ++ show a ++ ")"

op2str :: Op -> String
op2str Plus = "+"
op2str Minus = "-"
op2str Mul = "*"
op2str Div = "/"
op2str Pow = "**"

{- 在需要的地方添加括号。这个函数比较保守，有时候不需要也会加。
Haskell 在构建 SymbolicManip 的时候已经处理好优先级了。-}
simpleParen :: (Show a, Num a) => SymbolicManip a -> String
simpleParen (Number x) = prettyShow (Number x)
simpleParen (Symbol x) = prettyShow (Symbol x)
simpleParen x@(BinaryArith _ _ _) = "(" ++ prettyShow x ++ ")"
simpleParen x@(UnaryArith _ _) = prettyShow x

{- 调用 prettyShow 函数显示 SymbolicManip 值 -}
instance (Show a, Num a) => Show (SymbolicManip a) where
  show a = prettyShow a