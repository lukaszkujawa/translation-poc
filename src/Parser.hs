module Parser
    ( run_parser
    ) where

import Data.String
import Data.List
import Text.Parsec
import Control.Monad



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
