-- | Output the results to HTML
--
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Criterion.ToHtml.Html
    ( report
    ) where

import Data.Monoid (mempty)

import Data.Aeson (encode)
import Data.ByteString (ByteString)
import Text.Blaze (Html, unsafeLazyByteString, unsafeByteString, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Criterion.ToHtml.Result

report :: ByteString -> [ResultGroup] -> Html
report js results = H.docTypeHtml $ do
    H.head $ do
        H.title "Criterion results"
        -- jQuery for DOM manipulation
        H.script ! A.type_ "text/javascript"
                 ! A.src "http://code.jquery.com/jquery-latest.js"
                 $ mempty
        -- Our results as JSON
        H.script ! A.type_ "text/javascript" $ do
            "var criterionResults = "
            unsafeLazyByteString $ encode results
            ";"
        H.script ! A.type_ "text/javascript" $ unsafeByteString js
    H.body mempty
