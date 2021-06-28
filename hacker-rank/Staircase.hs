import Data.List

main :: IO ()
main = interact (unlines . staircaseFinal . read)

staircaseFinal :: Int -> [String]
staircaseFinal n = zipWith zipper (tail $ reverse (0:list)) list
    where
        zipper x y = replicate x ' ' ++ replicate y '#'
        list = [1..n]
