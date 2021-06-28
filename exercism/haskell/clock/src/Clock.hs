module Clock (addDelta, fromHourMin, toString) where

type Minutes = Int
type Hours = Int

data Clock = Clock Hours Minutes
  deriving Eq

fromHourMin :: Int -> Int -> Clock
fromHourMin h m
    | totMins >= 0 = Clock h1 m1
    | otherwise    = Clock h2 m2
    where
        totMins = (60 * h) + m
        (h1, m1) = quotRem (totMins `rem` 1440) 60
        (h2, m2) = quotRem (1440 + (totMins `rem` 1440)) 60

toString :: Clock -> String
toString (Clock h m) = fillWithZeros 2 (show h) ++ ":" ++ fillWithZeros 2 (show m)

addDelta :: Int -> Int -> Clock -> Clock
addDelta h m (Clock h' m') = fromHourMin (h + h') (m + m')

fillWithZeros :: Int -> String -> String
fillWithZeros n str = (take nZeros zeros) ++ str
    where
        zeros = ['0', '0'..]
        nZeros = n - length str
