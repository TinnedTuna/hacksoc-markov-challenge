import Markov
import OptionParser

import Control.Monad
import System.Environment
import System.Directory
import Data.Maybe (fromMaybe)
import Data.Text hiding (foldl, map, concat)

tokenise :: Text -> [Text]
tokenise = splitOn (singleton ' ')

main = do
    args <- getArgs
    let opts = case (getOptions args) of Right os -> os; Left err -> error err
    dirContents <- getDirectoryContents (optInputDirectory opts)  >>= filterM doesFileExist . (map (optInputDirectory opts ++))
    putStrLn $ show dirContents
