-- Part 1 - credit card number validation

toDigitsRev :: Integer -> [Integer]
toDigitsRev x | x < 0 = []
              | x < 10 = [x]
              | otherwise = (x `mod` 10) : (toDigitsRev (x `div` 10))

toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)
             
-- or
-- toDigits x = map digitToInt $ show x

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = fst (foldr step ([], False) x)
    where step val (acc, now) = ((if now then 2*val else val) : acc, not now)

-- or
-- doubleEveryOther = reverse . doubleEverySecond . reverse
--     where doubleEverySecond = zipWith ($) (cycle [id,(2*)])

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate x = num `mod` 10 == 0
    where num = sumDigits . doubleEveryOther . toDigits $ x

-- Part 2 - towers of hanoi

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi x _ _ _ | x < 1 = []
hanoi x from to temp = (hanoi (x-1) from temp to) ++ ((from,to) : (hanoi (x-1) temp to from))
