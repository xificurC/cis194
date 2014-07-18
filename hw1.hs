-- Part 1 - credit card number validation

toDigitsRev :: Integer -> [Integer]
toDigitsRev x | x <= 0 = []
              | otherwise = (x `mod` 10) : (toDigitsRev (x `div` 10))

toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = fst (foldr step ([], False) x)
    where step val (acc, now) = ((if now then 2*val else val) : acc, not now)

sumDigits :: [Integer] -> Integer
sumDigits x = sum $ concat $ map toDigits x

validate :: Integer -> Bool
validate x = num `mod` 10 == 0
    where num = sumDigits $ doubleEveryOther $ toDigits x


-- Part 2 - towers of hanoi

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

move 1 from to _ = [(from,to)]
move x from to temp = (move (x-1) from temp to) ++ [(from,to)] ++ (move (x-1) temp to from)
