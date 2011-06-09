-- | Main module
--
module Main
    ( main
    ) where

import Control.Applicative ((<$>))
import System.Environment (getArgs, getProgName)
import System.FilePath (replaceExtension)

import Text.Blaze.Renderer.Utf8 (renderHtml)
import qualified Data.ByteString.Lazy as BL

import Criterion.ToHtml.Html
import Criterion.ToHtml.Result

toHtml' :: FilePath -> FilePath -> IO ()
toHtml' csv html = do
    putStrLn $ "Parsing " ++ csv
    csv' <- parseCriterionCsv <$> readFile csv
    BL.writeFile html $ renderHtml $ table csv'
    putStrLn $ "Wrote " ++ html

main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    case args of
        [csv, html] -> toHtml' csv html
        [csv]       -> toHtml' csv (replaceExtension csv "html")
        _           -> putStrLn $
            "Usage: " ++ progName ++ " <csv-file> [out-file]"
