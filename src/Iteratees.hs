module Iteratees where

import Control.Exception

type ErrMsg = SomeException
data Stream
  = EOF (Maybe ErrMsg)
  | Chunk String deriving Show

data Iteratee a
  = IE_done a
  | IE_cont (Maybe ErrMsg) (Stream -> (Iteratee a, Stream))

empty_stream = Chunk ""

-- Ok, I give up
