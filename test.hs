import Data.List
import Control.Monad

solveRPN :: (Num a, Read a) => String -> a
solveRPN = head . foldl foldingFunction [] . words
    where   foldingFunction (x:y:ys) "*" = (x * y):ys
            foldingFunction (x:y:ys) "+" = (x + y):ys
            foldingFunction (x:y:ys) "-" = (x - y):ys
            foldingFunction xs numStr = read numStr:xs


type Birds = Int
type Pole = (Birds,Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left,right)
    | abs (left + n - right) < 4 = Just (left + n, right)
    | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left,right)
    | abs (right + n - left) < 4 = Just (left, right + n)
    | otherwise = Nothing

x -: f = f x

banana :: Pole -> Maybe Pole
banana _ = Nothing

data Assertion a = Pass | Fail a deriving (Show)

instance Functor Assertion where
    fmap f Pass = Pass
    fmap f (Fail a) = Fail (f a)

instance Monoid a => Monoid (Assertion a) where
    mempty = Pass

instance Semigroup a => Semigroup (Assertion a) where
    Pass <> Pass = Pass
    Pass <> s = s
    s <> Pass = s
    Fail s1 <> Fail s2 = Fail (s1 <> s2)

assertEqual :: (Eq b) => a -> b -> b -> Assertion a
assertEqual msg val1 val2
    | val1 == val2 = Pass
    | otherwise = Fail msg
