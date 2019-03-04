module Parser
    ( run_parser
    ) where

import Data.String
import Data.List
import Text.Parsec
import Control.Monad
import Tokenizer.Types
import Debug.Trace


update_pos :: SourcePos -> Event -> [Event] -> SourcePos
update_pos pos _ (tok:_) = setSourceLine (setSourceColumn pos (1)) (1)
update_pos pos _ [] = pos

event :: Parsec [Event] () String
event = tokenPrim show update_pos testEvent
        where
            testEvent x = Just (show x)

eventType :: (Event -> Bool) -> Parsec [Event] () String
eventType eventTest = tokenPrim show update_pos testEvent
        where
            testEvent e = if (eventTest e) then Just (show e) else Nothing

isEventPageLoad :: Event -> Bool
isEventPageLoad (EventPageLoad _ _) = True
isEventPageLoad _ = False

isEventClick :: Event -> Bool
isEventClick (EventClick _ _) = True
isEventClick _ = False

isEventLogin :: Event -> Bool
isEventLogin (EventLogin _ _) = True
isEventLogin _ = False

isEventPlayVideo :: Event -> Bool
isEventPlayVideo (EventPlayVideo _ _) = True
isEventPlayVideo _ = False

events_parser :: Parsec [Event] () [String]
events_parser = count 1 event >> (lookAhead (eventType isEventPageLoad) <|> lookAhead (eventType isEventClick)) >> many event
  --( skipMany (lookAhead eventClick) )>> manyTill event eventClick

run_parser :: [Event] -> [String]
run_parser input = case parse events_parser "" input of
   Left err -> error $ "parse error at " ++ (show err)
   Right val  -> val

{-
data CustomLang = Foo | Bar | FooBar deriving (Show)

data Token = N {lin, col :: Int, val :: Integer}
           | Spc {lin, col :: Int}
           | Plus {lin, col :: Int}

instance Show Token where
   show N{val=x} = "number " ++ show x
   show Plus{} = "plus"

--example = [N 1 3 111, Plus 1 10, N 2 1 222]
example = [Spc 0 0, N 1 0 111, Plus 2 1, N 3 1 222]

play :: [Token] -> String
play s = case parse pmain "" s of
   Left err -> error $ "parse error at " ++ (show err)
   Right val  -> show val

pmain :: Parsec [Token] () [CustomLang]
pmain =  (skipMany pspc) >> many( pnum <|> pplus )

pnum = tokenPrim show update_pos get_num
       where
         get_num N{val=x} = Just Foo
         get_num _ = Nothing

pplus = tokenPrim show update_pos is_plus
       where
         is_plus Plus{} = Just Bar
         is_plus _ = Nothing

pspc = tokenPrim show update_pos is_space
      where
        is_space Spc{} = Just FooBar
        is_space _ = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = setSourceLine (setSourceColumn pos (col tok)) (lin tok)
update_pos pos _ [] = pos

run_parser = play example
-}
