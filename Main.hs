import Markov
import OptionParser

import Control.Monad
import Control.Monad.Random (evalRand)
import System.Environment
import System.Directory
import System.Random
import Data.Maybe (fromMaybe)
import Data.Text hiding (foldl, map, concat, take)
import Data.Text.Encoding as E 
import Data.ByteString as B hiding (putStrLn, map, singleton, take, pack)

tokenise :: Text -> [Text]
tokenise = splitOn (singleton ' ') . replace (pack "\r\n") (singleton ' ') . replace  (singleton '"') (singleton ' ') . replace (pack "&&&") (singleton ' ')

deTokenise :: [Text] -> Text
deTokenise = Data.Text.intercalate $ singleton ' '

getGen :: Maybe Int -> StdGen -> StdGen
getGen seed gen = maybe gen (mkStdGen) seed

main = do
    args <- getArgs
    let opts = case (getOptions args) of Right os -> os; Left err -> error err
    dirContents <- getDirectoryContents (optInputDirectory opts)  >>= filterM doesFileExist . (map (optInputDirectory opts ++))
    putStrLn $ show dirContents
    corpusByteStringFiles <- mapM (B.readFile) dirContents
    let chain = buildChainMultipleInput $ map (tokenise . E.decodeUtf8) corpusByteStringFiles
    defaultGenerator <- getStdGen
    generated <- return $ evalRand (runChain chain) (getGen (optSeed opts) defaultGenerator)
    putStrLn . show $ deTokenise $ (take $ optOutputLength opts) generated
