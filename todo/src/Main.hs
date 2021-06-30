module Main where

import Parser (Status, Item, parseContent)
import qualified Text.Trifecta as T
import Graphics.Vty
import Control.Monad.Trans.Maybe
import Control.Monad (forever)

type AppState = (Item, [Item])

showItem :: Item -> String
showItem (state, body) = show state ++ " " ++ body

renderState :: AppState -> Image
renderState (selected, items)
  = (foldr (<->) emptyImage . map (string currentAttr . showItem)) items


-- testAppState :: AppState
-- testAppState = ((Done, "Questo e' finito"),
--                [(Todo, "Questo no"),(Done, "Questo e' finito"), (Done, "Questo e' finito")])

main :: IO ()
main = do
    config <- standardIOConfig
    vty <- mkVty config
    result <- T.parseFromFile parseContent "DB.todo"
    case result of
      Nothing -> return ()
      Just a -> do
          let pic = picForImage $ renderState (head a, a)
          mainLoop vty pic


mainLoop :: Vty -> Picture -> IO ()
mainLoop vty pic = do
    update vty pic
    e <- nextEvent vty
    case checkEvent e of
      Close -> shutdown vty
      _     -> mainLoop vty pic

data Action = Close
            | Down
            | Up
            | Set
            | None
            deriving (Show)

checkEvent :: Event -> Action
checkEvent (EvKey k [])
    = case k of
        KEsc      -> Close
        KChar 'j' -> Down
        KChar 'k' -> Up
        KUp       -> Up
        KDown     -> Down
        _ -> None
checkEvent _ = None
