{-# LANGUAGE NamedFieldPuns #-}
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (intercalate)
import Text.Printf (printf)
import System.Environment (getArgs)
import Prelude hiding (Left)

main = do
    args <- getArgs
    if length args /= 2
        then putStrLn "Two strings must be given as arguments"
        else do
            let [a,b] = args
            let result = lcs a b
            putStrLn $ showLCSResult result
            putStrLn $ showLCSSteps result

data LCSResult = LCSResult
    { from :: String
    , to :: String
    , values :: Map (Int,Int) Int
    , directions :: Map (Int,Int) Direction
    , size :: (Int,Int)
    }

data Direction = None | Up | Left | Diagonal

showDirection None = " "
showDirection Up = "↑"
showDirection Left = "←"
showDirection Diagonal = "↖"

showLCSResult :: LCSResult -> String
showLCSResult LCSResult { from, to, values, directions, size } =
    unlines $
        -- print top header
        "      " <> intercalate "  " (map (:[]) from)
        :
        [ -- print left header
          (if y >= 0 then [to !! y] <> " " else "  ") <>
          -- print value
          intercalate " " [showValue y x | x <- [-1..snd size]]
          | y <- [-1..fst size]
        ]
    where
        showValue y x = showDirection (directions M.! (y,x)) <> show (values M.! (y,x))

showLCSSteps :: LCSResult -> String
showLCSSteps LCSResult { to, directions, size } = unlines $ go size where
    go (-1,-1) = []
    go (y,x) = case directions M.! (y,x) of
       Diagonal -> go (y-1,x-1) 
       Left -> (sDelete x) : go (y,x-1)
       Up -> case M.lookup (y-1,x) directions of
               Just Left -> (sReplace y x) : go (y-1,x-1)
               _ -> (sInsert y x) : go (y-1,x)
    sDelete x = printf "delete(%d)" (x + 1)
    sInsert y x = printf "insertAfter(%d,%c)" (x + 1) (to !! y)
    sReplace y x = printf "replace(%d,%c)" (x + 1) (to !! y)

lcs :: String -> String -> LCSResult
lcs as bs = LCSResult as bs values directions size
    where
        size = (length bs - 1, length as - 1)
        coords = [(y,x) | y <- [-1..fst size], x <- [-1..snd size]]
        values = M.fromList (map (\c -> (c, mapValues c)) coords)
        mapValues (-1,_) = 0
        mapValues (_,-1) = 0
        mapValues (y,x)
            | as !! x == bs !! y = 1 + values M.! (y-1, x-1)
            | otherwise = max (values M.! (y-1,x)) (values M.! (y,x-1))
        directions = M.fromList (map (\c -> (c, mapDirections c)) coords)
        mapDirections (-1,-1) = None
        mapDirections (-1,_) = Left
        mapDirections (_,-1) = Up
        mapDirections (y,x)
            | as !! x == bs !! y = Diagonal
            | (values M.! (y,x-1)) > (values M.! (y-1,x)) = Left
            | otherwise = Up
