module Utils where

repeatTwiceAndLoop :: [a] -> [a]
repeatTwiceAndLoop [] = []
repeatTwiceAndLoop (x:xs) = (repeatTwiceAndLoop' (x:xs)) ++ [x] where
    repeatTwiceAndLoop [] = []
    repeatTwiceAndLoop' [y] = [y]
    repeatTwiceAndLoop' (x:y:ys) = x:y : repeatTwiceAndLoop' (y:ys)


repeatTwice :: [a] -> [a]
repeatTwice [] = []
repeatTwice [y] = [y]
repeatTwice (x:y:ys) = x:y : repeatTwice (y:ys)