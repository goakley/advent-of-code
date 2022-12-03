import Control.Arrow ((&&&))
import Data.List (sort)
import Data.Maybe (fromMaybe, mapMaybe)

-- |Get the median value from a (possibly unsorted) data set
median :: Integral a => [a] -> Maybe a
median [] = Nothing
median xs = let ys = sort xs
                len = length ys
                get f = ys !! f len
            in Just $ if even (length ys)
                      then (get (`div` 2) + get (subtract 1 . (`div` 2))) `div` 2
                      else get (`div` 2)

-- |Pairs of (open,close) characters that cannot be together
invalidPairs :: [(Char,Char)]
invalidPairs = [('(',']')
               ,('(','}')
               ,('(','>')
               ,('[',')')
               ,('[','}')
               ,('[','>')
               ,('{',')')
               ,('{',']')
               ,('{','>')
               ,('<',')')
               ,('<',']')
               ,('<','}')
               ]

-- |Is a character some kind of open delimiter?
isOpener :: Char -> Bool
isOpener = (`elem` "([{<")

-- |Is a character some kind of close delimiter?
isCloser :: Char -> Bool
isCloser = (`elem` ")]}>")

-- |Invert the delimiters in a string (non-delimiters will be unchanged)
complement :: String -> String
complement [] = []
complement ('(':xs) = ')' : complement xs
complement ('[':xs) = ']' : complement xs
complement ('{':xs) = '}' : complement xs
complement ('<':xs) = '>' : complement xs
complement (x:xs) = x : complement xs

-- |The checker score for a character
checkScore :: Char -> Int
checkScore ')' = 3
checkScore ']' = 57
checkScore '}' = 1197
checkScore '>' = 25137
checkScore _ = 0

-- |The autocomplete score for a character
completeScore :: Char -> Int
completeScore ')' = 1
completeScore ']' = 2
completeScore '}' = 3
completeScore '>' = 4
completeScore _ = 0

-- |Get the final score of an autocompletion
scoreAutocomplete :: String -> Int
scoreAutocomplete = foldl (\a -> ((a * 5) +) . completeScore) 0

-- |Parse a line, stopping when an invalid delimiter is reached (or at the end of the text)
--
-- Provides the stack of delimiters at stop time as well as the first invalid delimiter.
parseLine :: String -> String -> (String, Maybe Char)
parseLine os [] = (os, Nothing)
parseLine [] (x:xs) = if isCloser x
                      then ([], Just x)
                      else parseLine [x | isOpener x] xs
parseLine (o:os) (x:xs) = if (o,x) `elem` invalidPairs
                          then (o:os, Just x)
                          else parseLine (if isOpener x
                                          then x:o:os
                                          else if isCloser x
                                               then os
                                               else o:os
                                         ) xs

-- |Autocomplete a line of text (if it's a valid incomplete line)
autocompleteLine :: String -> Maybe String
autocompleteLine = (\(os,m) -> maybe (Just (complement os)) (const Nothing) m) . parseLine []

solve1 :: [String] -> Int
solve1 = sum . mapMaybe (fmap checkScore . snd . parseLine [])

solve2 :: [String] -> Int
solve2 = fromMaybe 0 . median . mapMaybe (fmap scoreAutocomplete . autocompleteLine)

main :: IO ()
main = interact (show . (solve1 &&& solve2) . lines)
