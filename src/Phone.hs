module Phone where

import Text.ParserCombinators.Parsec

phoneNumber = do
  try (do
          cc <- countrycode
          ac <- areacode
          local <- localNumber
          eof
          return (read cc, read ac, read local)) <|>
   (do
       ac <- areacode
       local <- localNumber
       eof
       return (0, read ac, read local))

countrycode = do
  optional (char '+')
  code <- many1 digit
  space
  spaces
  return code


areacode = do
  optional (char '(')
  code <- count 3 digit
  optional (char ')')
  spaces
  return code

localNumber = do
  first <- count 3 digit
  char '-'
  second <- count 4 digit
  spaces
  return (first ++ second)

parsePhoneNumber :: String -> Either ParseError (Int, Int, Int)
parsePhoneNumber input = parse phoneNumber "(unknown)" input
