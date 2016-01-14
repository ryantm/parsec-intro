module SSN where

import Text.ParserCombinators.Parsec

ssn = do
  first <- count 3 digit
  char '-'
  middle <- count 2 digit
  char '-'
  end <- count 3 digit
  eof
  return (read first, read middle, read end)

parseSSN :: String -> Either ParseError (Int, Int, Int)
parseSSN input = parse ssn "(unknown)" input
