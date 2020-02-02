{-# Language OverloadedStrings #-}
module Post
  ( context
  , Post.parse
  )
where

import           Data.Maybe
import           Data.Time                      ( Day
                                                , showGregorian
                                                , fromGregorian
                                                )
import           Data.Attoparsec.Text
import           Data.Attoparsec.Combinator
import qualified Template
import qualified Data.Map                      as Map
import qualified Data.Text                     as Text

data Post = Post {sourceDir :: String
                 ,title     :: String
                 ,slug      :: String
                 ,meta      :: String
                 ,date      :: Day
                 ,content   :: String
                 } deriving(Show)

context :: Post -> Template.Context
context p = Template.StringValue <$> Map.fromList
  [ ("title"  , title p)
  , ("slug"   , slug p)
  , ("meta"   , meta p)
  , ("date"   , shortDate p)
  , ("content", content p)
  ]

shortDate :: Post -> String
shortDate p = showGregorian (date p)

type Heading = Map.Map String String

extractHeader :: String -> (Heading, String)
extractHeader content = case parseOnly heading (Text.pack content) of
  Right (pairs, content) -> (pairs, content)
  Left  err              -> (Map.fromList [], err)

heading :: Parser (Heading, String)
heading = do
  string "---"
  pairs <- headerLines
  string "\n---"
  content <- body
  return (Map.fromList pairs, content)
 where
  body        = manyTill anyChar $ lookAhead Template.noMore
  headerLines = manyTill parseline $ lookAhead (endOfLine >> string "---")
  parseline   = do
    endOfLine
    line <- manyTill anyChar $ lookAhead endOfLine
    let (key, val) = break (== ':') line
    return (Template.trim key, tail val)

parse :: FilePath -> String -> String -> Post
parse dir slug content =
  let (header, body) = extractHeader content
      pTitle         = fromJust $ Map.lookup "title" header
      pMeta          = fromJust $ Map.lookup "meta" header
      pDate          = undefined
  in  Post { sourceDir = dir
           , title     = pTitle
           , slug      = slug
           , meta      = pMeta
           , date      = pDate
           , content   = body
           }


readPost :: String -> String -> Post
readPost = undefined
