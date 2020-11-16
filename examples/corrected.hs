-- examples of using Hood, corrected to work with stack lts-10.7,
-- and standardized as annotated forms that should be passed to printO.
--
-- Based on code from the home page http://ku-fpg.github.io/software/hood
-- and from Andy Gill, "Debugging Haskell by Observing Intermediate Structures" ACM SIGPLAN Haskell Workshop 2000.
--     /usr/local/book/cs/lang/haskell/papers/debug/10.1.1.84.9011.pdf

import Debug.Hood.Observe

-- From hackage:

h1 = [observe "+1" (+1) x | x <- observe "xs" [1..3]]
h1a xs = [observe "+1" (+1) x | x <- observe "xs" xs]
-- printO h1 and  printO $ h1a [1..3] both give:
-- [2,3,4]
--  -- +1
--   {  1  -> 2
--   }
--  -- +1
--   {  2  -> 3
--   }
--  -- +1
--   {  3  -> 4
--   }
--  -- xs
--   1 : 2 : 3 : []

-- From the home page.
-- Hood can observe data structures:

h2 = reverse . (observe "intermediate") . reverse
-- printO $ h2 [0..9] gives:
-- [0,1,2,3,4,5,6,7,8,9]
-- -- intermediate
--    9 : 8 : 7 : 6 : 5 : 4 : 3 : 2 : 1 : 0 : []

-- Hood can observe functions:
h3 = observe "foldl (+) 0 [1..4]" foldl (+) 0 [1..4]
-- printO $ h3 gives:
-- 10
--
-- -- foldl (+) 0 [1..4]
--   { \ { \ 0 1  -> 1
--       , \ 1 2  -> 3
--       , \ 3 3  -> 6
--       , \ 6 4  -> 10
--       } 0 (1 : 2 : 3 : 4 : [])
--        -> 10
--   }

-- If an argument is not examined in the function, it remains unevaluated.
h4 = observe "sum xs" (\ xs ys -> sum xs) [0..2] [0..]
-- printO h4 gives:
-- Notice that ys is left unevaluated (denoted by the underscore):
-- 3
-- -- sum xs
--   { \ (0 : 1 : 2 : []) _  -> 3
--   }


-- From the workshop paper: simple examples of each feature:

-- (3.1) Observing a list
ex1  = observe "list" [0..9]
ex1a = (observe "list" :: Observing [Int]) [0..9]
ex1b = observe "list"
ex1c = (observe "list" :: Observing [Int])

-- print0 ex1 and print0 ex1a both print
-- list
--  0 : 1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : []
-- as do printO $ ex1b [0..9] and printO $ ex1c [0..9] 

-- (3.2) Observing an intermediate list
-- (observe can be used partially applied, which is the typical use scenario when
-- observing inside a point-free pipeline.)

ex2  = reverse . (observe "intermediate") . reverse
ex2a = reverse . (observe "intermediate" :: Observing [Int]) . reverse
-- printO $ ex2 [0..9] gives:
-- [0,1,2,3,4,5,6,7,8,9]
--
-- intermediate
-- 9 : 8 : 7 : 6 : 5 : 4 : 3 : 2 : 1 : 0 : []

-- (3.3) Observing an infinite list
ex3 = take 10 (observe "infinite list" [0..])
-- print0 ex3 shows
-- infinite list
-- 0 : 1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : _

-- (3.4) Observing lists with unevaluated elements
ex4 = length (observe "finite list" [1..10])
ex4a = length . (observe "finite list")
-- print0 ex4  and printO $ ex4a [1..10] both show"
-- finite list
-- _ : _ : _ : _ : _ : _ : _ : _ : _ : _ : []

ex5     = length $ (observe "finite list"::Observing[Int]) [error "oops!" | _ <- [0..9]]
ex5a xs = length $ (observe "finite list"::Observing[Int]) [error "oops!" | _ <- xs]
ex5b n  = length $ (observe "finite list"::Observing[Int]) (replicate n (error "oops"))
-- should all give:
-- 10
-- finite list
--   _ :  _ :  _ :  _ :  _ :  _ :  _ :  _ :  _ :  _ : []


ex6 = (xs !! 2 + xs !! 4) where xs = observe "list" [0..9]
-- printO ex6 shows
-- 6
--
-- -- list
--    _ :  _ :  _ :  _ :  4 : _
-- -- list
--    _ :  _ :  2 : _


-- (4.1) Observing functions
ex7  = (observe "length" length) [1..3]
ex7a = (observe "length" length)
-- printO $ ex7 and printO $ ex7a [1..3] both show;
-- -- length
-- { \ (_ : _ : _ : []) -> 3
-- }

-- We place observe at the caller site, and can see the effect that a specific
-- function has from this context, including higher order functions.

ex8  = (observe "foldl (+) 0 [1..4]") foldl (+) 0 [1..4]
ex8a = (observe "foldl (+) 0 [1..4]" :: Observing ((Int -> Int -> Int) -> Int -> [Int] -> Int)) foldl (+) 0 [1..4]
ex8b = (observe "foldl (+) 0") foldl (+) 0
-- printO ex8, printO ex8a, and printO $ ex8b [1..4] all give:
-- 10
-- 
-- -- foldl (+) 0 [1..4]
--   { \ { \ 0 1  -> 1
--      , \ 1 2  -> 3
--      , \ 3 3  -> 6
--      , \ 6 4  -> 10
--      } 0 ( 1 :  2 :  3 :  4 : [] )  -> 10
--   }

-- (4.2) Observing the State Monad [TBD]
-- We can use observe to look at the state inside the state monad. State monads
-- typically have a state transformer function that takes a complete state, and
-- returns a new state. Let’s call this function modify.
--  modify :: (State -> State) -> M ()
-- 
-- We can observe the state at a specific point using the function observeM
-- observeM :: String -> M ()
-- observeM label = modify (observe label :: Observing State)

-- (4.3) Observing the IO Monad

--  We render an IO action using the pseudo-constructor <IO>, followed by an
--  observation on the returned object.

ex9 :: IO Char
ex9 = (observe "getChar" :: Observing (IO Char)) getChar
-- printO ex9 shows:
-- getChar <IO> 'x'
-- We read this as "some side effect happened, resulting in the value 'x' being returned".
-- FAILS, with error
--     • No instance for (Show (IO Char)) arising from a use of ‘printO’
--     • In the expression: printO ex9
--       In an equation for ‘it’: it = printO ex9

ex10 :: Char -> IO ()
ex10 = (observe "putChar" :: Observing (Char -> IO ()) putChar)
-- printO ex10 'x' shows
-- putChar
-- let fn 'x' = <IO> ()
-- We read this as "we have a function that takes 'x', does some side- effect
-- stuff, and returns unit".
-- FAILS, with error
-- • No instance for (Show (IO ())) arising from a use of ‘printO’
-- • In the expression: printO $ ex10 'x'
--   In an equation for ‘it’: it = printO $ ex10 'x'


-- A running example (before annotation):
natural0 :: Int -> [Int]
natural0 =
  reverse
  . map (`mod` 10)
  . takeWhile (/= 0)
  . iterate (`div` 10)
-- natural 3408 gives ['3','4','0','8']

-- (3.5) Using more than one observer
natural1 :: Int -> [Int]
natural1 =
  (observe "after reverse") . reverse
  . (observe "after map") . map (`mod` 10)
  . (observe "after takeWhile") . takeWhile (/= 0)
  . (observe "after iterate") . iterate (`div` 10)

natural1a :: Int -> [Int]
natural1a =
  (observe "after reverse" :: Observing [Int])
  . reverse
  . (observe "after map" :: Observing [Int])
  . map (`mod` 10)
  . (observe "after takeWhile" :: Observing [Int])
  . takeWhile (/= 0)
  . (observe "after iterate" :: Observing [Int])
  . iterate (`div` 10)

-- printO $ natural1 3408, gives (note random order of steps):
-- [3,4,0,8]
-- 
-- -- after iterate
--    3408 :  340 :  34 :  3 :  0 : _
-- -- after map
--    8 :  0 :  4 :  3 : []
-- -- after reverse
--    3 :  4 :  0 :  8 : []
-- -- after takeWhile
--    3408 :  340 :  34 :  3 : []


-- Returning to our natural example, we can now observe the individual
-- transformers, rather than the structures between them.
natural2 :: Int -> [Int]
natural2 =
  observe "reverse"              reverse
  . observe "map (`mod` 10)"     map (`mod` 10)
  . observe "takeWhile (/= 0)"   takeWhile (/= 0)
  . observe "iterate (`div` 10)" iterate (`div` 10)

-- printO $ natural2 3408 gives (note random order of steps):
-- [3,4,0,8]
-- 
-- -- iterate (`div` 10)
--   { \ { \ 3  -> 0
--      , \ 34  -> 3
--      , \ 340  -> 34
--      , \ 3408  -> 340
--      } 3408  ->  3408 :  340 :  34 :  3 :  0 : _
--   }
-- -- map (`mod` 10)
--   { \ { \ 3  -> 3
--      , \ 34  -> 4
--      , \ 340  -> 0
--      , \ 3408  -> 8
--      } ( 3408 :  340 :  34 :  3 : [] )  ->  8 :  0 :  4 :  3 : []
--   }
-- -- reverse
--   { \ ( 8 :  0 :  4 :  3 : [] )  ->  3 :  4 :  0 :  8 : []
--   }
-- -- takeWhile (/= 0)
--   { \ { \ 0  -> False
--      , \ 3  -> True
--      , \ 34  -> True
--      , \ 340  -> True
--      , \ 3408  -> True
--      } ( 3408 :  340 :  34 :  3 :  0 : _ )  ->  3408 :  340 :  34 :  3 : []
--   }

-- (4.4) Multiple Observations.
-- (Based on www.haskell.org/hood/downloads/Main.hs from 2003).
natural3 :: Int -> [Int]
natural3 = observers "natural" $ \(O observe) ->
  (observe "after reverse"      :: Observing [Int])
  . reverse
  . (observe "after map"        :: Observing [Int])
  . map (`mod` 10)
  . (observe "after takeWhile"  :: Observing [Int])
  . takeWhile (/= 0) 
  . (observe "after iterate"    :: Observing [Int])
  . iterate (`div` 10)

-- Now printO $ (natural1 3408, natural1 123) will output intermingled observatons:
-- ([3,4,0,8],[1,2,3])
-- 
-- -- after iterate
--    3408 :  340 :  34 :  3 :  0 : _
-- -- after iterate
--    123 :  12 :  1 :  0 : _
-- -- after map
--    8 :  0 :  4 :  3 : []
-- -- after map
--    3 :  2 :  1 : []
-- -- after reverse
--    3 :  4 :  0 :  8 : []
-- -- after reverse
--    1 :  2 :  3 : []
-- -- after takeWhile
--    3408 :  340 :  34 :  3 : []
-- -- after takeWhile
--    123 :  12 :  1 : [
-- 
-- But printO $ (natural3 3408, natural3 123) should output:
-- ([3,4,0,8],[1,2,3])
-- 
-- -- natural
--   { \ 3408  ->  3 :  4 :  0 :  8 : []
--   }
--   -- after reverse
--      3 :  4 :  0 :  8 : []
--   -- after map
--      8 :  0 :  4 :  3 : []
--   -- after takeWhile
--      3408 :  340 :  34 :  3 : []
--   -- after iterate
--      3408 :  340 :  34 :  3 :  0 : _
--   
-- -- natural
--   { \ 123  ->  1 :  2 :  3 : []
--   }
--   -- after reverse
--      1 :  2 :  3 : []
--   -- after map
--      3 :  2 :  1 : []
--   -- after takeWhile
--      123 :  12 :  1 : []
--   -- after iterate
--      123 :  12 :  1 :  0 : _
