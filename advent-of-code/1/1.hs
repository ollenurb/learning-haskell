test :: [Int]
test = [
    1721,
    979,
    366,
    299,
    675,
    1456]

solution2 :: [Int] -> Int
solution2 xs = let sums = [[x, y, z] | x <- xs, y <- xs, z <- xs, x+y+z == 2020]
               in (foldr (*) 1) (head sums)

solution1 :: [Int] -> Int
solution1 xs = let sums = [(x, y) | x <- xs, y <- xs, x+y == 2020]
               in (uncurry (*)) (head sums)

strToInt :: String ->  Int
strToInt x = read x :: Int

printableOutput :: [Int] -> String
printableOutput xs =
    "First solution is " ++
    show (solution1 xs) ++
    ", second is " ++
    show (solution2 xs)

main :: IO ()
main = interact (show . printableOutput . map strToInt . lines)

