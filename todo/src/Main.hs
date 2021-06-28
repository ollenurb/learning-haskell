module Main where

import Parser (Item, parseContent)
import Text.Trifecta
import Brick

type AppState = [Item]

render :: AppState -> [Widget String]
render items = map (str . show) items

app :: App AppState () String
app = App { appDraw = render
          , appChooseCursor = showFirstCursor
          , appHandleEvent = undefined
          , appStartEvent = return
          , appAttrMap const map
        
          }

loadFileContent :: String -> IO (Maybe [Item])
loadFileContent filePath = parseFromFile parseContent filePath

main :: IO ()
main = do
    putStr "Insert filename: "
    filePath <- getLine
    result <- parseFromFile parseContent filePath
    case result of
      Nothing -> return ()
      Just a -> sequence_ $ map (putStrLn . show) a

