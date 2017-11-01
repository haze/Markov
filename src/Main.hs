{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.List              (intercalate)
import qualified Data.MarkovChain       as M
import           Database.SQLite.Simple
import           System.Directory       (doesFileExist)
import           System.Environment     (getArgs)
import qualified System.IO              as F
import           System.Random          (mkStdGen)
import           Text.Printf            (printf)

createDatabase :: FilePath -> IO ()
createDatabase fileName = do
    putStrLn "Creating Markov Database"
    F.openFile fileName F.WriteMode
    conn <- open fileName
    execute_ conn "CREATE TABLE IF NOT EXISTS markov ( words TEXT NOT NULL )"
    close conn

write :: Connection -> String -> IO ()
write conn text = execute conn "INSERT INTO markov (words) VALUES (?)" (Only (text :: String))

getWords :: Connection -> IO [[String]]
getWords conn = query_ conn "SELECT * FROM markov" :: IO [[String]]

clearDatabase :: Connection -> IO ()
clearDatabase conn = execute_ conn "DELETE FROM markov"

-- Thank you, Al Pimenov.
flatten :: (Foldable t, Foldable t1) => t1 (t a) -> [a]
flatten xs = (\z n -> foldr (flip (foldr z)) n xs) (:) []

printMarkovChain :: Connection -> Int -> Int -> IO ()
printMarkovChain conn pred chars = do
  words <- getWords conn
  putStrLn $ (take chars $ M.run pred (unwords $ flatten words) 0 (mkStdGen 666))

-- co..  :: base -> search
contains :: (Eq x) => [x] -> [x] -> Bool
contains y x
  | null x || null y = False
  | x == y = True
  | take (length y) x == y  = True
  | otherwise = contains y (tail x)

exe :: [String] -> Connection -> IO ()
exe args conn
    | "--read" `elem` args = readWords conn
    | "--print" `elem` args && length args < 3 = putStrLn "Next time maybe add how many characters I should print?"
    | "--print" `elem` args = printMarkovChain conn (read $ args !! 1 :: Int) (read $ args !! 2 :: Int)
    | "--clear" `elem` args = clearDatabase conn

    | any (contains "--") args = putStrLn "Flag does not exist."
    | otherwise = do
        printf "Adding \"%s\" to the database...\n" final
        write conn final
    where readWords conn = do
                        words <- getWords conn
                        mapM_ print words
          final = unwords args

main :: IO ()
main = do
    let dbName = "markov.db"
    args <- getArgs
    fExist <- doesFileExist dbName
    if not fExist
        then createDatabase dbName
    else do
        conn <- open dbName
        exe args conn
