-- | Simple CSV parser
--
module Criterion.ToHtml.Csv
    ( Field
    , Record
    , Csv
    , parseCsv
    ) where

type Field = String
type Record = [Field]
type Csv = [Record]

-- | Parse a CSV file
--
parseCsv :: String -> Csv
parseCsv = map parseRecord . lines

-- | Parse a CSV record
--
parseRecord :: String -> Record
parseRecord rec = case parseField rec of
    (x, ',' : y) -> x : parseRecord y
    (x, _)       -> [x]

-- | Parse a CSV field
--
parseField :: String -> (Field, String)
parseField xs = case xs of
    ('"' : xs') -> parse' "" xs'
    _           -> break (== ',') xs
  where
    parse' y ('"' : '"' : ys) = parse' ('"' : y) ys
    parse' y ('"' : ys)       = (reverse y, ys)
    parse' y (y'  : ys)       = parse' (y' : y) ys
    parse' y []               = (reverse y, "")
