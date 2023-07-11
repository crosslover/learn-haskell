type InfoP a =
  Int -> -- path to directory entry
  Int -> -- permissions
  Maybe Integer -> -- file size (Nothing if not file)
  Int -> -- last modified
  a

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k w x y z = f w x y z == k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k