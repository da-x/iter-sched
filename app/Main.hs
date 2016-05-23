{-# LANGUAGE OverloadedStrings #-}

module Main where

------------------------------------------------------------------------------------------
import           Control.Monad
import           Data.IORef
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           System.Environment
import           Text.Regex.TDFA      ((=~))
import           Text.Regex.TDFA.Text ()
------------------------------------------------------------------------------------------

divisions :: [Int] -> Set ([(Int, Int)])
divisions x = root gs' 0 [] x
    where gs'           = sum x
          root _ _ _ [] = Set.empty
          root gs idx fs (n:xs)
              | n < 0     = error "neg"
              | n == 0    = root gs (idx + 1) (0:fs) xs
              | otherwise = let recurse = root (gs - 1) 0 [] ((reverse fs) ++ (n - 1):xs)
                             in if Set.null recurse
                                   then Set.singleton $ [(idx, 1)]
                                   else Set.fromList $ concat $ map (addMulti gs idx) $ Set.toList recurse
          addMulti gs idx s = map (addAt s) [0 .. gs - 1]
                where addAt [] pos = error $ show (pos, gs, idx, s)
                      addAt ((ridx, count):xs) pos
                          | pos > count     = (ridx, count):(addAt xs (pos - count))
                          | ridx == idx     = (ridx, count + 1)                         :xs
                          | pos == 0        = (idx, 1)     :(ridx, count)               :xs
                          | pos == count    =
                                case xs of
                                    []                    -> (ridx, count):(idx, 1):[]
                                    ((ridx', count'):xs') ->
                                        if ridx' == idx
                                            then (ridx, count):(ridx', count' + 1):xs'
                                            else (ridx, count):(idx, 1):xs
                          | otherwise       = (ridx, pos)  :(idx, 1):(ridx, count - pos):xs

perString :: Text -> IO ()
perString b = do
    lastIdxI <- newIORef Nothing
    fragmentsIdxI <- newIORef Map.empty

    forM_ (T.lines b) $ \line -> do
        let lineRegEx :: Text
            lineRegEx = "^:([0-9]): (.*)$"
        let otherRegEx :: Text
            otherRegEx = "^    (.*)$"
        let emptyRegEx :: Text
            emptyRegEx = "^[\t ]*$"

        let addLine idx str = do
                let f (Just xs)     = Just ([str]:xs)
                    f _             = Just [[str]]
                modifyIORef' fragmentsIdxI $ Map.alter f idx
        let appendLine idx str = do
                let f (Just (x:xs)) = Just ((x ++ [str]):xs)
                    f _             = Just [[str]]
                modifyIORef' fragmentsIdxI $ Map.alter f idx
        let appendLine' str = do
                midx <- readIORef lastIdxI
                case midx of
                    Just idx -> appendLine idx str
                    Nothing  -> return ()

        case (line =~ lineRegEx :: [[Text]]) of
            [[_, idxStr, restOfLine]] -> do
                let idx = read $ T.unpack idxStr :: Int
                writeIORef lastIdxI $ Just idx
                addLine idx restOfLine
            _ -> case (line =~ otherRegEx :: [[Text]], (line =~ emptyRegEx :: Bool)) of
                    ([[_, restOfLine]], False) -> appendLine' restOfLine
                    (_, True)                  -> appendLine' ""

    fragmentsIdx <- readIORef fragmentsIdxI
    let fragmentsZipped =
            zip [0..] (map (\(idx, linesg) ->
                            (idx, reverse linesg)) (Map.toList fragmentsIdx))
    let fragmentsMapped = Map.fromList fragmentsZipped
    let divisionsSet = divisions (map (\(zipidx, (idx, lines)) -> length lines) fragmentsZipped)
    forM_ (zip [1..] (Set.toList divisionsSet)) $ \(schedIdx, schedOrder) -> do
        fragmentsTrackI <- newIORef fragmentsMapped
        putStr $ "Schedling order #" ++ show schedIdx ++ ": "
        print schedOrder
        putStrLn "--------------------------------------------------"
        forM_ schedOrder $ \(item, count) -> do
            fragmentsTrack <- readIORef fragmentsTrackI
            (idx, linesg) <-
                case Map.lookup item fragmentsTrack of
                    Nothing -> error "unexpected: Map.lookup item fragmentsTrack"
                    Just r -> return r

            forM_ (take count linesg) $ \lineg ->
                forM_ (zip [0..] lineg) $ \(lineNr, line) -> do
                    case lineNr of
                        0 -> do
                            putStr ":"
                            putStr $ show idx
                            putStr ": "
                        _ -> do
                            putStr "    "
                    T.putStrLn line

            writeIORef fragmentsTrackI $ Map.insert item (idx, drop count linesg) fragmentsTrack

        putStrLn ""

perFile :: String -> IO ()
perFile filename = do
    b <- T.readFile filename
    perString b

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> perFile filename
        _ -> do T.putStrLn "Syntax: iter-sched <filename>"
                T.putStrLn ""
                T.putStrLn "Where filename contains lines that either start with "
                T.putStrLn "4 spaces or ':[0-9]: ', denoting a scheduling item"
                T.putStrLn "in a scheduling group bearing that number."
                T.putStrLn ""
                let inp = T.unlines [
                       ":0: action_a1"
                     , ":0: action_a2"
                     , ":1: action_b2"
                     ]

                T.putStrLn "Example input"
                T.putStrLn inp

                T.putStrLn "Example output"
                T.putStrLn ""
                perString inp
