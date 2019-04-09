import Data.List
import Control.Monad
import Test.HUnit

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


test1 = TestCase (assertEqual "for (foo 3)," (1,2) (1,2))
tests = TestList [TestLabel "test1" test1]
