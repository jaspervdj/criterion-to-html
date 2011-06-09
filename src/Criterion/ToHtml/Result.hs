-- | Parse Criterion results
--
module Criterion.ToHtml.Result
    ( Result (..)
    , parseCriterionCsv
    , normalizeMeans
    ) where

import Criterion.ToHtml.Csv

-- | A criterion result
--
data Result = Result
    { resultName :: String
    , resultMean :: Double
    }

-- | Parse a Criterion CSV file
--
parseCriterionCsv :: String -> [Result]
parseCriterionCsv = map parseCriterionResult . drop 1 . parseCsv

-- | Parse a single result
--
parseCriterionResult :: Record -> Result
parseCriterionResult (name : mean : _) = Result name (read mean)
parseCriterionResult _                 = error
    "Criterion.ToHtml.Parse.parseCriterionResult: invalid CSV file"

normalizeMeans :: [Result] -> [(Result, Double)]
normalizeMeans []      = []
normalizeMeans results = map normalize results
  where
    normalize result = (result, resultMean result / max')
    max' = maximum $ map resultMean results
