module Delay where

data Delay a = Later (Delay a) | Now a
data Progress = NotYet Progress | Done
