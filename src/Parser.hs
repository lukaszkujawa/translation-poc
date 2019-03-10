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

notEventType :: (Event -> Bool) -> Parsec [Event] () String
notEventType eventTest = tokenPrim show update_pos testEvent
        where
            testEvent e = if (eventTest e) then Nothing else Just (show e)

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
events_parser = many expr
-- skipMany $ notEventType isEventClick
-- >> (many $ eventType isEventClick)
--events_parser = count 1 event >> (lookAhead (eventType isEventPageLoad) <|> lookAhead (eventType isEventClick)) >> many event
  --( skipMany (lookAhead eventClick) )>> manyTill event eventClick


expr = try (skip_nonclicks >> clicks)

clicks = eventType isEventClick
skip_nonclicks =  skipMany $ notEventType isEventClick

run_parser :: [Event] -> [String]
run_parser input = case parse events_parser "" input of
    Left err -> error $ "parse error at " ++ (show err)
    Right val  -> val
