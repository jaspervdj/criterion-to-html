-- | Parse Criterion results
--
{-# LANGUAGE OverloadedStrings #-}
module Criterion.ToHtml.Result
    ( Result (..)
    , ResultGroup (..)
    , parseCriterionCsv
    , groupResults
    ) where

import System.FilePath (splitFileName)
import qualified Data.Map as M

import Data.Aeson (ToJSON (toJSON), object, (.=))

import Criterion.ToHtml.Csv

-- | A criterion result
--
data Result = Result
    { resultName :: String
    , resultMean :: Double
    }

instance ToJSON Result where
    toJSON (Result name mean) = object ["name" .= name, "mean" .= mean]

-- | A criterion result group
--
data ResultGroup = ResultGroup
    { resultGroupName    :: String
    , resultGroupResults :: [Result]
    }

instance ToJSON ResultGroup where
    toJSON (ResultGroup name results) =
        object ["name" .= name, "results" .= results]

-- | Parse a Criterion CSV file
--
parseCriterionCsv :: String -> [ResultGroup]
parseCriterionCsv =
    groupResults . map parseCriterionResult . drop 1 . parseCsv

-- | Parse a single result
--
parseCriterionResult :: Record -> Result
parseCriterionResult (name : mean : _) = Result name (read mean)
parseCriterionResult _                 = error
    "Criterion.ToHtml.Parse.parseCriterionResult: invalid CSV file"

-- | Group results
--
groupResults :: [Result] -> [ResultGroup]
groupResults =
    map toGroup . M.toList . M.fromListWith (flip (++)) . map splitResult
  where
    toGroup = uncurry ResultGroup
    splitResult (Result name mean) =
        let (group, name') = splitFileName name
        in (group, [Result name' mean])
