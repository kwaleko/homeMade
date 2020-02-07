{-# Language OverloadedStrings #-}
module Post
  ( postCtx
  , archiveCtx
  , Post.parse
  --, markdownToHtml
  , Post
  , url
  )
where

import Types
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
import           Text.Pandoc                    ( writeHtml5String
                                                , readMarkdown
                                                , runPure
                                                , def
                                                , ReaderOptions
                                                , WriterOptions
                                                , readerExtensions
                                                , pandocExtensions
                                                )
import           System.FilePath                ( (</>) )
--import qualified Text.Pandoc                   as P




data Post = Post {sourceDir :: String
                 ,title     :: String
                 ,slug      :: String
                 ,meta      :: String
                 ,date      :: Day
                 ,content   :: String
                 } deriving(Show)

postCtx :: Post -> Template.Context
postCtx p = Template.StringValue <$> Map.fromList
  [ ("title"  , title p)
  , ("slug"   , slug p)
  , ("url"    , url p)
  , ("meta"   , meta p)
  , ("date"   , shortDate p)
  , ("content", content p)
  ]

url :: Post -> String
url p = sourceDir p
-- archive is a context of post grouped by year
archiveCtx :: [Post] -> Template.Context
archiveCtx posts = Template.listField "posts" $ map postCtx posts

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

parse :: FilePath -> Slug -> String -> Post
parse dir slug content =
  let (header, body) = extractHeader content
      pTitle         = fromJust $ Map.lookup "title" header
      pMeta          = fromJust $ Map.lookup "meta" header
      pDate          = readDate $ fromJust $ Map.lookup "date" header
  in  Post { sourceDir = slug
           , title     = pTitle
           , slug      = slug
           , meta      = pMeta
           , date      = pDate
           , content   = markdownToHtml body
           }

readDate :: String -> Day
readDate d = fromGregorian 2018 12 12

markdownToHtml :: String -> String
markdownToHtml content = case runPure txt of
  Right html -> Text.unpack html
  Left  err  -> show err
 where
  rOpt = def { readerExtensions = pandocExtensions } :: ReaderOptions
  wOpt = def :: WriterOptions
  txt  = do
    markdown <- readMarkdown rOpt (Text.pack content)
    writeHtml5String wOpt markdown
