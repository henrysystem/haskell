module Bob (responseFor) where

import Data.Text as T
import Data.Text (Text)
import Data.Char (isUpper)

getLastCharacter :: Text -> Maybe Char
getLastCharacter xs = case T.unsnoc xs of
  Nothing -> Nothing
  Just (_, c) -> Just c

hasOnlyUppercase :: Text -> Bool
hasOnlyUppercase xs = T.all isUpper xs

hasSuffixText :: Text -> Bool
hasSuffixText xs = if hasOnlyUppercase (T.init xs )
                             then T.isSuffixOf  (T.pack "?") xs
                             else False
                             
responseFor :: Text -> Text
responseFor xs 
  | T.null xs = T.pack "Fine. Be that way!"
  | hasSuffixText xs = T.pack "Calm down, I know what I'm doing!"
  | getLastCharacter xs == Just '?' = T.pack "Sure" 
  | hasOnlyUppercase xs = T.pack "Whoa, chill out!" 
  | otherwise = T.pack "Whatever"  

main :: IO ()
main = do
  let xs = T.pack "Listo"
  putStrLn (T.unpack (responseFor xs))