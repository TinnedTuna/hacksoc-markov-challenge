module OptionParser (
    getOptions,
    optSeed,
    optInputDirectory,
    optOutputFile,
    optOutputLength, 
    Options
    ) where

import Control.Monad
import System.Environment
import System.Console.GetOpt
import System.Directory
import Data.Maybe (fromMaybe)
import Data.Text hiding (foldl, map, concat)

data Options = Options 
    { optSeed :: Maybe Int
    , optInputDirectory :: FilePath
    , optOutputFile :: Maybe FilePath
    , optOutputLength :: Int
    }
    deriving Show

defaultOptions :: Options
defaultOptions = Options
    { optSeed = Nothing
    , optInputDirectory = "./corpus/"
    , optOutputFile = Nothing
    , optOutputLength = 1500
    }

options :: [OptDescr (Options -> Options)]
options = 
    [ Option ['s'] ["seed"] (OptArg (\f opts -> maybe opts (\f -> opts { optSeed = read f}) f) "SEED") "Seed for the RNG" 
    , Option ['i'] ["inputDir"] (OptArg (\f opts -> maybe opts (\f -> opts { optInputDirectory = f}) f) "DIR") "Directory containing the corpus" 
    , Option ['o'] ["outputFile"] (OptArg (\f opts -> opts { optOutputFile = fmap read f}) "FILE") "File to write the generated document to"
    , Option ['l'] ["length"] (OptArg (\f opts -> maybe opts (\f -> opts { optOutputLength = read f}) f) "LENGTH") "The length of the generated document"
    ]

tokenise :: Text -> [Text]
tokenise = splitOn (singleton ' ')

getOptions :: [String] -> Either String Options
getOptions args = 
    case getOpt Permute options args  of
        (actions, _, []) -> Right $ foldl (flip id) defaultOptions actions
        (_,_,errors) -> Left $ "Could not parse comand line: \n" ++ concat errors
