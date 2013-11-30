import Markov
import OptionParser

import Control.Monad
import Control.Monad.Random (evalRand)
import Control.DeepSeq
import System.Environment
import System.Directory
import System.Random
import System.IO
import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import Data.Maybe (fromMaybe)
import Data.Text hiding (foldl, map, concat, take, unwords, length)
import Data.Text.Encoding as E 
import Data.Text.IO as TIO
import Data.ByteString as B hiding (putStrLn, map, singleton, take, pack, length)
import Data.Char
import Data.Map as M (keys)

space = fromIntegral . ord $ ' '

{-
tokenise :: ByteString -> [Text]
tokenise = 
    map (E.decodeUtf8 . copy)
    . B.split space 
-}
tokenise :: Text -> [Text]
tokenise = 
    splitOn (singleton ' ') 
    . replace (pack "  ") (singleton ' ')
    . replace (pack "\r\n") (singleton ' ') 
    . replace (singleton '"') (singleton ' ') 
    . replace (pack "&&&") (singleton ' ')
    . replace (pack "***") (singleton ' ')
    . replace (singleton '>') (singleton ' ')

deTokenise :: [Text] -> Text
deTokenise = Data.Text.intercalate $ singleton ' '

getGen :: Maybe Int -> StdGen -> StdGen
getGen seed gen = maybe gen (mkStdGen) seed

getOutput :: Maybe FilePath -> Handle -> (ByteString -> IO ()) 
getOutput path handle = maybe (B.hPut handle) (B.writeFile) path

writeHandle :: (ByteString -> IO ()) -> Text -> IO ()
writeHandle h str = do
    h $ E.encodeUtf8 str

getLoggerFromMaybe :: Maybe FilePath -> IO (GenericHandler Handle)
getLoggerFromMaybe path = case path of
    Just fp -> fileHandler fp INFO
    Nothing -> streamHandler stdin INFO

main = do
    args <- getArgs
    let opts = case (getOptions args) of Right os -> os; Left err -> error err
    dirContents <- getDirectoryContents (optInputDirectory opts)  >>= filterM doesFileExist . (map (optInputDirectory opts ++))
    corpusFiles <- mapM (TIO.readFile) dirContents
    let chain = buildChainPar . map (tokenise) $ corpusFiles
    logHandle <- getLoggerFromMaybe (optLogFile opts)
    return $ setFormatter logHandle (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    updateGlobalLogger "MarkovGenerator.Main" (addHandler logHandle)
    debugM "MarkovGenerator.Main" "Got the chain built."
    infoM "MarkovGenerator.Main" $ "We have " ++ show (length (M.keys chain)) ++ " keys"
    defaultGenerator <- getStdGen
    generated <- return $ evalRand (runChain chain) (getGen (optSeed opts) defaultGenerator)
    let outputHandle = getOutput (optOutputFile opts) stdout
    infoM "MarkovGenerator.Main" $ "Using corpus: " ++ (unwords (map show dirContents))
    infoM "MarkovGenerator.Main" $ "Seed: " ++ maybe "default" (show) (optSeed opts)
    writeHandle outputHandle $ deTokenise $ (take $ optOutputLength opts) generated
    
