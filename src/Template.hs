{-# Language OverloadedStrings #-}
module Template
  ( fragments
  , Fragment(Raw, Var, Include, Loop, End)
  , interpreter
  , ContextValue(StringValue,ListValue,Template)
  , Context
  , parse
  , raw
  , parseOnly
  , packed
  , stringField
  , listField
  , trim
  , anyBetween
  , noMore 
  )
where

import           Control.Monad                  ( join )
import qualified Data.Map                      as M
import           Data.Text                      ( pack
                                                , unpack
                                                , Text
                                                )
import           Data.Attoparsec.Text           ( choice
                                                , string
                                                , Parser
                                                , anyChar
                                                , skipSpace
                                                , parseOnly
                                                )
import           Data.Attoparsec.Combinator     ( manyTill
                                                , many1
                                                , lookAhead
                                                )
import           Data.Attoparsec.Internal       ( atEnd )

data Fragment
  = Raw     String
  | Var     String
  | Loop    String
  | Include String
  | End
  deriving (Eq)

instance (Show) Fragment where
  show (Raw     txt) = "\n - Raw    :" ++ txt
  show (Var     txt) = "\n - Var    :" ++ txt
  show (Loop    txt) = "\n - Loop   :" ++ txt
  show (Include txt) = "\n - Include:" ++ txt
  show End           = "\n - End" 

fragments :: Parser [Fragment]
fragments = manyTill (choice [raw, packed]) $ noMore

packed :: Parser Fragment
packed = choice [include, forEach, end, variable, noMore]
 where
  variable = between "{{" "}}" Var
  forEach  = between "{{foreach" "}}" Loop
  include  = between "{{include" "}}" Include
  end      = string "{{end}}" >> return End

raw :: Parser Fragment
raw = do
  str <- manyTill anyChar $ lookAhead packed
  case str of
    [] -> fail ""
    xs -> return $ Raw $ xs

between :: Text -> Text -> (String -> Fragment) -> Parser Fragment
between startWith endWith construct = do
  inner <- anyBetween startWith endWith 
  case inner of
    [] -> fail ""
    xs -> return $ construct $ trim inner

anyBetween :: Text -> Text -> Parser String
anyBetween startwith endwith = do
  string startwith
  inner <- anyCharTill endwith 
  string endwith
  return inner 

trim :: String -> String
trim list = case list of
  []       -> []
  ' ' : xs -> trim xs
  x   : xs -> x : trim xs

anyCharTill :: Text -> Parser String
anyCharTill stopAt = manyTill anyChar $ lookAhead $ string stopAt

noMore :: Parser Fragment
noMore = do
  endReached <- atEnd
  case endReached of
    True  -> return End
    False -> fail ""

type Context = M.Map String ContextValue

type Template = [Fragment]

data ContextValue =
    StringValue String
  | ListValue [Context]
  | Template  String
  deriving(Show)

stringField :: String -> String -> Context
stringField key val = M.singleton key (StringValue val)

listField :: String -> [Context] -> Context
listField key val = M.singleton key (ListValue val)

-- given a template and context
-- substitue the context in the template
-- and generate a string with the HTML output
parse :: String -> Context -> String
parse txt context = fst $ interpreter context template
 where
  template = case parseOnly fragments (pack txt) of
    Left  err -> []
    Right fs  -> fs

interpreter :: Context -> Template -> (String, [Fragment])
interpreter ctx template = next template ""
 where
  expand key = case M.lookup key ctx of
    Just (StringValue sub) -> sub
    Just _                 -> ""
    _                      -> ""
  substitute key = case M.lookup key ctx of
    Just (Template val) -> val
    Just _              -> ""
    _                   -> ""
  getList key = case M.lookup key ctx of
    Just (ListValue lst) -> lst
    Just _               -> []
    _                    -> []
  next :: [Fragment] -> String -> (String, [Fragment])
  next [] result = (result,[])
  next (f : more) result = case f of
    End              -> (result, more)
    Var     val      -> next more $ result ++ expand val
    Raw     val      -> next more $ result ++ val
    Include template -> next more $ result ++ substitute template
    (Loop list)      -> next rem $ result ++ inner
     where
      inner =
        join $ (\context -> fst (interpreter context more)) <$> getList list
      (_, rem) = interpreter ctx more

