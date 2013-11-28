import Markov
import OptionParser

import Control.Monad
import Control.Monad.Random (evalRand)
import Control.DeepSeq
import System.Environment
import System.Directory
import System.Random
import System.IO
import Data.Maybe (fromMaybe)
import Data.Text hiding (foldl, map, concat, take, unwords)
import Data.Text.Encoding as E 
import Data.Text.IO as TIO
import Data.ByteString as B hiding (putStrLn, map, singleton, take, pack, copy)

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

main = do
    args <- getArgs
    let opts = case (getOptions args) of Right os -> os; Left err -> error err
    dirContents <- getDirectoryContents (optInputDirectory opts)  >>= filterM doesFileExist . (map (optInputDirectory opts ++))
    corpusFiles <- mapM (TIO.readFile) dirContents
    let chain = force $ buildChainMultipleInput . map (tokenise) $ corpusFiles
    let logHandle = getOutput (optLogFile opts) stdout
    writeHandle logHandle (pack ("Got the chain built."))
    defaultGenerator <- getStdGen
    generated <- return $ evalRand (runChain chain) (getGen (optSeed opts) defaultGenerator)
    let outputHandle = getOutput (optOutputFile opts) stdout
    writeHandle logHandle (pack ("Using corpus: " ++ (unwords (map show dirContents)) ++ "\n"))
    writeHandle logHandle (pack ("Seed: " ++ maybe "default" (show) (optSeed opts) ++ "\n"))
    writeHandle outputHandle $ deTokenise $ (take $ optOutputLength opts) generated
