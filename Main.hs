import Markov

import System.Environment
import System.Console.GetOpt
import Data.Maybe (fromMaybe)

data Options = Options 
    { optSeed :: Maybe Int
    , optInputDirectory :: Maybe FilePath
    , optOutputFile :: Maybe FilePath
    , optOutputLength :: Int
    }
    deriving Show

defaultOptions :: Options
defaultOptions = Options
    { optSeed = Nothing
    , optInputDirectory = Just "./corpus/"
    , optOutputFile = Nothing
    , optOutputLength = 1500
    }

options :: [OptDescr (Options -> Options)]
options = 
    [ Option ['s'] ["seed"] (OptArg ((\f opts -> opts { optSeed = Just f}) . read . fromMaybe "seed") "SEED") "Seed for the RNG" 
    , Option ['i'] ["inputDir"] (OptArg ((\f opts -> opts { optInputDirectory = Just f}) . fromMaybe "inputDir") "DIR") "Directory containing the corpus" 
    , Option ['o'] ["outputFile"] (OptArg ((\f opts -> opts { optOutputFile = Just f}) . fromMaybe "outputFile") "FILE") "File to write the generated document to"
    , Option ['l'] ["length"] (OptArg ((\f opts -> opts { optOutputLength = f}) . read .fromMaybe "length") "LENGTH") "The length of the generated document"
    ]

main = do
    args <- getArgs
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return defaultOptions) actions
    print opts

