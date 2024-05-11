module Main where

import Lib

-- test _where with startsWith
startsWith :: Char -> String -> Bool
startsWith char string = char == (head string)

-- use a placholder main
main :: IO ()
main = do
    let nName = _where (Main.startsWith 'N' . firstName) (_select studentName students)
    let onlyN = head nName
    putStrLn ((firstName onlyN) ++ " " ++ (lastName onlyN))
