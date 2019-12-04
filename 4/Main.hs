import Data.List (group)

main :: IO ()
main = do
  let result = length . filter isValid $ [136818..685979]
  print result
  let result2 = length . filter isValid2 $ [136818..685979]
  print result2

isValid :: Int -> Bool
isValid x = twoAdjacentSame ds && digitsNonDecreasing ds
  where
    ds = toDigits x

isValid2 :: Int -> Bool
isValid2 x = twoButNotThreeAdjacentSame ds && digitsNonDecreasing ds
  where
    ds = toDigits x

type Digit = Char

toDigits :: Int -> [Digit]
toDigits = show

twoAdjacentSame :: Eq a => [a] -> Bool
twoAdjacentSame [] = False
twoAdjacentSame (_:[]) = False
twoAdjacentSame (x:y:xs) = x == y || twoAdjacentSame (y:xs)

twoButNotThreeAdjacentSame :: Eq a => [a] -> Bool
twoButNotThreeAdjacentSame = any ((== 2) . length) . group

digitsNonDecreasing :: Ord a => [a] -> Bool
digitsNonDecreasing xs = all (uncurry (>=)) (zip (drop 1 xs) xs)
