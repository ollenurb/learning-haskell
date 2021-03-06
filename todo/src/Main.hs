{-# LANGUAGE TypeSynonymInstances #-}

module Main where


import Parser
import qualified Text.Trifecta as T
import Graphics.Vty
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)

type AppState = ListZipper Item

-- Wrap in a NewType to get a different Show instance
newtype RenderizableItem = RI Item

instance Show RenderizableItem where
    show (RI (Item Done body)) = "[X] " ++ body
    show (RI (Item Todo body)) = "[ ] " ++ body

-- TODO: Clean this up
renderState :: AppState -> Image
renderState (selected:below, above)
  = foldr (<->) emptyImage $ (reverse . toImages above) ++ (selectedImage:toImages below)
      where
          toImages = map (string defAttr . show . RI)
          selectedImage = string selectedAttr (show $ RI selected)
          selectedAttr = defAttr `withBackColor` white `withForeColor` black

changeSelectedState :: AppState -> AppState
changeSelectedState (selected:below, above) = (switchState selected:below, above)

saveChanges :: AppState -> String -> IO ()
saveChanges state path = do
    let items = uncurry (++) state
    writeFile path $ unlines . map show $ items

-- Main version without monad transformers.
-- In this case you can see that IO and Maybe are not composed, so we need to
-- get out the value wrapped indise the Maybe monad to continue the computation
-- within IO. MaybeT transformer just combine those two monads.
--
-- main :: IO ()
-- main = do
--     config <- standardIOConfig
--     vty <- mkVty config
--     result <- T.parseFromFile parseContent "DB.todo"
--     case result of
--       Nothing -> return ()
--       Just a -> do
--           let pic = picForImage $ renderState (head a, a)
--           mainLoop vty pic

main :: IO ()
main = do
    config <- standardIOConfig
    vty <- mkVty config
    runMaybeT $ do
        result <- MaybeT $ T.parseFromFile parseContent "DB.todo"
        let initialState = (result, [])
        lift $ mainLoop vty initialState
    return ()

mainLoop :: Vty -> AppState -> IO ()
mainLoop vty state = do
    update vty renderedState
    e <- nextEvent vty
    case checkEvent e of
      Close -> saveChanges state "DB.todo" >> shutdown vty
      Up    -> mainLoop vty $ backward state
      Down  -> mainLoop vty $ forward state
      Set   -> mainLoop vty $ changeSelectedState state
      _     -> mainLoop vty state
    where
        renderedState = picForImage $ renderState state

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
        KChar 's' -> Set
        KChar 'j' -> Down
        KChar 'k' -> Up
        KUp       -> Up
        KDown     -> Down
        _ -> None
checkEvent _ = None

-- List zipper definition
-- snd -> Elements ABOVE current element
-- fst -> Elements BELOW current element
-- Selected element: head . fst
-- Initial state ([1,2,3,4], [])
type ListZipper a = ([a], [a])

forward :: ListZipper a -> ListZipper a
forward (x:xs,ys) = (xs, x:ys)

backward :: ListZipper a -> ListZipper a
backward (xs, y:ys) = (y:xs, ys)
