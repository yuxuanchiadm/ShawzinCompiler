module ShawzinCompiler where

import Data.Functor
import Data.Maybe
import Data.List
import Text.Read (readPrec, readMaybe)
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec (lift)
import System.Exit

data Scale = PentatonicMinor | PentatonicMajor | Chromatic | Hexatonic | Major | Minor | Hirajoshi | Phrygian deriving (Show, Read, Eq, Ord)
data Note = N0 | N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | NA | NB | NC deriving (Eq, Ord)
data Number = Number Int Int
type Track = [Note]
type Interval = Int
type Position = Int
data Song = Song Scale Interval [Track] deriving (Show, Read, Eq, Ord)
type Code = String

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

class Encodable a where
    encode :: a -> Code

instance Encodable Scale where
    encode PentatonicMinor = "1"
    encode PentatonicMajor = "2"
    encode Chromatic = "3"
    encode Hexatonic = "4"
    encode Major = "5"
    encode Minor = "6"
    encode Hirajoshi = "7"
    encode Phrygian = "8"

instance Encodable Int where
    encode i | i < 0 = ""
             | i >= 0 && i < 64 = [(['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+', '/']) !! i]
             | otherwise = encode (i `div` 64) ++ encode (i `mod` 64)

instance Encodable Number where
    encode (Number i padTo) = let code = encode i in replicate (max 0 (padTo - length code)) 'A' ++ code

instance Encodable Note where
    encode N0 = "A"
    encode N1 = "B"
    encode N2 = "C"
    encode N3 = "E"
    encode N4 = "J"
    encode N5 = "K"
    encode N6 = "M"
    encode N7 = "R"
    encode N8 = "S"
    encode N9 = "U"
    encode NA = "h"
    encode NB = "i"
    encode NC = "k"

linkSong :: Song -> Code
linkSong (Song scale interval tracks) = linkShawzin scale interval tracks

linkShawzin :: Scale -> Interval -> [Track] -> Code
linkShawzin scale interval tracks = linkHeader scale ++ linkBody 0 interval tracks

linkHeader :: Scale -> Code
linkHeader = encode

linkBody :: Position -> Interval -> [Track] -> Code
linkBody _ _ [] = ""
linkBody position interval tracks = currentNotesEncoded ++ linkBody (position + interval) interval tracksTail where
    currentNotesEncoded :: Code
    currentNotesEncoded = concat $ do
        note <- tracksHead
        return $ if note == N0 then "" else encode note ++ encode (Number position 2)
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

compileLinkSong :: String -> Maybe Code
compileLinkSong input = linkSong <$> compileSong input

compileLinkSongIO :: IO ()
compileLinkSongIO = do
    input <- getContents
    maybe exitFailure putStrLn (compileLinkSong input)
