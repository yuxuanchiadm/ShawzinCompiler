module ShawzinCompiler where

import Data.Functor
import Data.Maybe
import Data.List
import Text.Read (readPrec, readMaybe)
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec (lift)
import System.Exit

data Scale = PentatonicMinor | PentatonicMajor | Chromatic | Hexatonic | Major | Minor | Hirajoshi | Phrygian deriving (Show, Read, Eq, Ord)
type Number = Int
data Note = N0 | N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | NA | NB | NC deriving (Eq, Ord)
type Track = [Note]
type Interval = Int
data Song = Song Scale Interval [Track] deriving (Show, Read, Eq, Ord)

instance Show Note where
    show N0 = "0"
    show N1 = "1"
    show N2 = "2"
    show N3 = "3"
    show N4 = "4"
    show N5 = "5"
    show N6 = "6"
    show N7 = "7"
    show N8 = "8"
    show N9 = "9"
    show NA = "A"
    show NB = "B"
    show NC = "C"

instance Read Note where
    readPrec = lift
        (   (char '0' $> N0)
        +++ (char '1' $> N1)
        +++ (char '2' $> N2)
        +++ (char '3' $> N3)
        +++ (char '4' $> N4)
        +++ (char '5' $> N5)
        +++ (char '6' $> N6)
        +++ (char '7' $> N7)
        +++ (char '8' $> N8)
        +++ (char '9' $> N9)
        +++ (char 'A' $> NA)
        +++ (char 'B' $> NB)
        +++ (char 'C' $> NC)
        )

scalaEncoded :: Scale -> String
scalaEncoded PentatonicMinor = "1"
scalaEncoded PentatonicMajor = "2"
scalaEncoded Chromatic = "3"
scalaEncoded Hexatonic = "4"
scalaEncoded Major = "5"
scalaEncoded Minor = "6"
scalaEncoded Hirajoshi = "7"
scalaEncoded Phrygian = "8"

numberMapping :: [Char]
numberMapping = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+', '/']

numberPaddedEncoded :: Number -> Int -> String
numberPaddedEncoded i padTo = replicate (max 0 (padTo - length result)) 'A' ++ result where
    result :: String
    result = numberEncoded i

numberEncoded :: Number -> String
numberEncoded i | i < 0 = ""
                | i >= 0 && i < 64 = [numberMapping !! i]
                | otherwise = numberEncoded (i `div` 64) ++ numberEncoded (i `mod` 64)

noteEncoded :: Note -> String
noteEncoded N0 = "A"
noteEncoded N1 = "B"
noteEncoded N2 = "C"
noteEncoded N3 = "E"
noteEncoded N4 = "J"
noteEncoded N5 = "K"
noteEncoded N6 = "M"
noteEncoded N7 = "R"
noteEncoded N8 = "S"
noteEncoded N9 = "U"
noteEncoded NA = "h"
noteEncoded NB = "i"
noteEncoded NC = "k"

linkSong :: Song -> String
linkSong (Song scale interval tracks) = linkShawzin scale interval tracks

linkShawzin :: Scale -> Interval -> [Track] -> String
linkShawzin scale interval tracks = linkHeader scale ++ linkBody 0 interval tracks

linkHeader :: Scale -> String
linkHeader = scalaEncoded

linkBody :: Number -> Interval -> [Track] -> String
linkBody _ _ [] = ""
linkBody position interval tracks = currentNotesEncoded ++ linkBody (position + interval) interval tracksTail where
    currentNotesEncoded :: String
    currentNotesEncoded = concat $ do
        note <- tracksHead
        return $ if note == N0 then "" else noteEncoded note ++ numberPaddedEncoded position 2
    tracksHead :: [Note]
    tracksHead = do
        track <- tracks
        if null track then [] else [head track]
    tracksTail :: [Track]
    tracksTail = do
        track <- tracks
        if null track then [] else [tail track]

compileTrack :: String -> Maybe Track
compileTrack input = sequenceA $ do
    word <- words input
    return $ readMaybe word

compileSong :: String -> Maybe Song
compileSong input = readLines $ lines input where
    readLines :: [String] -> Maybe Song
    readLines (scaleLine:intervalLine:trackLines) = do
        scale <- readMaybe scaleLine
        interval <- find (>= 0) $ readMaybe intervalLine
        tracks <- sequenceA $ compileTrack <$> trackLines
        return $ Song scale interval tracks
    readLines _ = Nothing

compileLinkSong :: String -> Maybe String
compileLinkSong input = linkSong <$> compileSong input

compileLinkSongIO :: IO ()
compileLinkSongIO = do
    input <- getContents
    maybe exitFailure putStrLn (compileLinkSong input)
