module Test1 where

class Blob a where
  vlurk :: a -> a
  vlurk = id

instance Blob Int where
  vlurk _ = 48

main :: IO ()
main = return (2 + 3)

f :: Bool -> IO Int
f True = return 1
f False = return (0+0+0*2)

id :: a -> a
id a = let
         b = a
       in c
         where
           c = let d = b in if True then d else b

-- Are there comments in the AST?
forgotTheType z = "forgotTheType"

data Oen o = Wan | To o

ejwuusl :: Oen Int -> Oen String
ejwuusl Wan     = To "Wessaqqhan"
ejwuusl (To x) | x /= 27 = Wan

constructornr x =
  case x of
    Wan -> (id . id) 0
    _   -> 1


f :: Bool -> IO Int
f True = return 3
f False = return 17

test :: Int -> Int -> Int
test x = \x -> 3

type Tree a = Nil

f :: Tree a -> Int
f a = case [2] of
    (a:as) -> 1
    _      -> 2

type Test = String
type Test = Int