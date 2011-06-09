-- | Output the results to HTML
--
{-# LANGUAGE OverloadedStrings #-}
module Criterion.ToHtml.Html
    ( report
    ) where

import Data.List (sortBy)
import Data.Monoid (mappend)
import Data.Ord (comparing)
import Text.Printf (printf)

import Text.Blaze (Html, toHtml, toValue, preEscapedString, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Criterion.ToHtml.Result

report :: [Result] -> Html
report results = H.docTypeHtml $ do
    H.head $ H.title "Criterion results"
    H.body $ do
        H.h1 "Results"
        table results
        H.h1 "Sorted"
        table $ sortBy (comparing resultMean) results

table :: [Result] -> Html
table results = H.table ! A.style "width: 100%;" $ do
    H.tr $ do
        H.th "Name"
        H.th "Mean"
    mapM_ row $ normalizeMeans results

row :: (Result, Double) -> Html
row (Result name _, n) = H.tr $ do
    H.td $ toHtml name
    H.td ! A.style "width: 100%" $
        H.div ! A.style (backgroundColor `mappend` "; " `mappend` width)
              $ preEscapedString "&nbsp;"
  where
    width = toValue $ "width: " ++ show (round (n * 100) :: Int) ++ "%"
    backgroundColor = toValue
        (printf "background-color: rgb(%d, %d, %d)" r g b :: String)
    r, g, b :: Int
    (r, g, b) = (round (150 * n), round (150 * (1 - n)), 0)
