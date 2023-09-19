-- data Expr = Val Int | Add Expr Expr
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- eval :: Expr -> Int
-- eval (Val x) = x
-- eval (Add x y) = eval x + eval y

-- render :: Expr -> String
-- render (Val x) = show x
-- render (Add x y) = "(" ++ render x ++ "+" ++ render y ++ ")"

newtype Expr f = In (f (Expr f ))
newtype Val e = Val Int
type IntExpr = Expr Val

data Add e = Add e e
type AddExpr = Expr Add

data (f :+: g) e = Inl (f e) | Inr (g e)

instance Functor Val where
    fmap f (Val x) = Val x
instance Functor Add where
    fmap f (Add e1 e2) = Add (f e1) (f e2)
instance (Functor f , Functor g) => Functor (f :+: g) where
    fmap f (Inl e1) = Inl (fmap f e1)
    fmap f (Inr e2) = Inr (fmap f e2)

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f ) t)

class Functor f => Eval f where
    evalAlgebra :: f Int -> Int

instance Eval Val where
    evalAlgebra (Val x) = x
instance Eval Add where
    evalAlgebra (Add x y) = x + y

instance (Eval f ,Eval g) => Eval (f :+: g) where
    evalAlgebra (Inl x) = evalAlgebra x
    evalAlgebra (Inr y) = evalAlgebra y

eval :: Eval f => Expr f -> Int
eval = foldExpr evalAlgebra

-- val :: Int -> Expr Val
-- val x = In (Val x)

infixl 6 ⊕
-- (⊕) :: Expr Add -> Expr Add -> Expr Add
-- x ⊕ y = In (Add x y)

-- (⊕) :: (Add :≺: f ) => Expr f -> Expr f -> Expr f
-- val :: (Val :≺: f ) ⇒ Int → Expr f

inject :: (g :≺: f ) => g (Expr f ) -> Expr f
inject = In . inj
val :: (Val :≺: f ) => Int -> Expr f
val x = inject (Val x)
(⊕) :: (Add :≺: f ) => Expr f -> Expr f -> Expr f
x ⊕ y = inject (Add x y)

class (Functor sub, Functor sup) => sub :≺: sup where
    inj :: sub a -> sup a
instance Functor f => f :≺: f where
    inj = id
instance (Functor f , Functor g) => f :≺: (f :+: g) where
    inj = Inl
instance (Functor f , Functor g, Functor h,f :≺: g) => f :≺: (h :+: g) where
    inj = Inr . inj

addExample :: Expr (Val :+: Add)
addExample = In (Inr (Add (In (Inl (Val 118))) (In (Inl (Val 1219)))))

-- x :: Expr (Add :+: Val)
-- x = val 30000 ⊕ val 1330 ⊕ val 7
-- addExample2 = In (Inr (Add (In (Val 30000)) (In (Val 1330))))
