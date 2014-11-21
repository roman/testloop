module System.TestLoop.Util where

--------------------

import Data.List (intersperse)

--------------------------------------------------------------------------------

join :: [a] -> [[a]] -> [a]
join delim l = concat (intersperse delim l)
