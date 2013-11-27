import Markov

import System.Environment
import System.Console.GetOpt
import Data.Maybe (fromMaybe)

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

main = do
    args <- getArgs
    let (actions, nonOptions, errors) = getOpt Permute options args
    case errors of
        [] -> do  
            let opts = foldl id defaultOptions actions
            print opts 
        _ -> do { putStrLn "Errors: " ; print errors }
