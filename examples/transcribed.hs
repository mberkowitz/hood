-- examples of using hood.
-- Transcribed by Marc Berkowitz
-- from the home page http://ku-fpg.github.io/software/hood
-- and from Andy Gill, "Debugging Haskell by Observing Intermediate Structures" ACM SIGPLAN Haskell Workshop 2000.
--     /usr/local/book/cs/lang/haskell/papers/debug/10.1.1.84.9011.pdf


import Debug.Hood.Observe

observe :: (Observable a) => String -> a -> a
type Observing a = a -> a

-- From hackage
runO (print [ observe "+1" (+1) x | x <- observe "xs" [1..3]])
-- gives:
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

-- From the home page:

-- hood can observe data structures:

x2 = print
      . reverse
      . (observe "intermediate")
      . reverse
      $ [0..9]

-- has output:
-- [0,1,2,3,4,5,6,7,8,9]
-- -- intermediate
--    9 : 8 : 7 : 6 : 5 : 4 : 3 : 2 : 1 : 0 : []

-- hood can observe functions:
x9 = print $ observe "foldl (+) 0 [1..4]" foldl (+) 0 [1..4]

-- gives:
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

runO $ print $ observe "sum xs" (\ xs ys -> sum xs) [0..2] [0..]

-- Notice that ys is left unevaluated (denoted by the underscore):
-- 3
-- -- sum xs
--   { \ (0 : 1 : 2 : []) _  -> 3
--   }


-- From the workshop paper:

-- a running example
-- before annotation:
natural :: Int -> [Int]
natural =
  reverse
  . map (`mod` 10)
  . takeWhile (/= 0)
  . iterate (`div` 10)


-- A simple example of each feature:

-- (3.1) Observing a list
ex1 :: IO ()
ex1  = print (observe "list" [0..9])
ex1a = print ((observe "list" :: Observing [Int]) [0..9])

-- prints:
-- list
--  0 : 1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : []

-- (3.2) Observing an intermediate list
-- (observe can be used partially applied, which is the typical use scenario when
-- observing inside a point-free pipeline.)

ex2 =
  print
  . reverse
  . (observe "intermediate" :: Observing [Int])
  . reverse
  $ [0..9]

-- prints
-- intermediate
-- 9 : 8 : 7 : 6 : 5 : 4 : 3 : 2 : 1 : 0 : []

-- (3.3) Observing an infinite list
ex3 = print (take 10 (observe "infinite list" [0..]))
-- prints:
-- infinite list
-- 0 : 1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : _

-- (3.4) Observing lists with unevaluated elements
ex4 = print (length (observe "finite list" [1..10]))
-- prints:
-- finite list
-- _ : _ : _ : _ : _ : _ : _ : _ : _ : _ : []

ex5 = print (length ((observe "finite list" :: Observing [()]) [ error "oops!" | _ <- [0..9]]))
-- prints the same

ex6 =
  let xs = observe "list" [0..9]
  in print (xs !! 2 + xs !! 4)
-- shows
-- list
-- _ : _ : 2 :
--  _ : 4 : _

-- (3.5) Using more than one observe
natural1 :: Int -> [Int]
natural1 =
  (observe "after reverse" :: Observing [Int])
  . reverse
  . (observe "after map ..." :: Observing [Int])
  . map (`mod` 10)
  . (observe "after takeWhi ..." :: Observing [Int])
  . takeWhile (/= 0)
  . (observe "after iterate ..." :: Observing [Int])
  . iterate (`div` 10)

-- natural1 3408, gives:
-- -- after iterate (`div` 10)
-- (3408 : 340 : 34 : 3 : 0 : _)
-- -- after takeWhile (/= 0)
-- ( 3408 : 340 : 34 : 3 : [] )
-- -- after map (`mod` 10)
-- ( 8 : 0 : 4 : 3 : [] )
-- -- after reverse
-- ( 3 : 4 : 0 : 8 : [])

-- (4.1) Observing functions
ex7 = print ((observe "length" :: Observing ([Int] -> Int)) length [1..3])
-- shows
-- -- length
-- { \ (_ : _ : _ : []) -> 3
-- }

-- We place observe at the caller site, and can see the effect that a specific
-- function has from this context, including higher order functions.

ex8 = print ((observe "foldl (+) 0 [1..4]" :: Observing ((Int -> Int -> Int) -> Int -> [Int] -> Int)) foldl (+) 0 [1..4])

-- gives:
-- -- foldl (+) 0 [1..4]
-- { \ { \ 6 4 -> 10
-- , \ 3 3 -> 6
-- , \ 1 2 -> 3
-- , \ 0 1 -> 1
-- }
-- 0
-- ( 1 : 2 : 3 : 4 : [])
-- -> 10
-- }

-- Returning to our natural example, we can now observe the individual
-- transformers, rather than the structures between them.
natural2 :: Int -> [Int]
natural2 =
  observe "reverse" reverse
  . observe "map (`mod` 10)" map (`mod` 10)
  . observe "takeWhile (/= 0)" takeWhile (/= 0)
  . observe "iterate (`div` ...)" iterate (`div` 10)

-- natural2 3408 gives in part
-- -- iterate (`div` 10)
-- { \ { \ 3 -> 0
-- , \ 34 -> 3
-- , \ 340 -> 34
-- , \ 3408 -> 340
-- } 3408
-- -> 3408 : 340 : 34 : 3 : 0 : _
-- }
-- -- takeWhile (/= 0)
-- { \ { \ 0 -> False
-- , \ 3 -> True
-- , \ 34 -> True
-- , \ 340 -> True
-- , \ 3408 -> True
-- } (3408 : 340 : 34 : 3 : 0 : _)
-- -> 3408 : 340 : 34 : 3 : []
-- }

-- (4.2) Observing the State Monad

-- We can use observe to look at the state inside the state monad. State monads
-- typically have a state transformer function that takes a complete state, and
-- returns a new state. Let’s call this function modify.

  modify :: (State -> State) -> M ()

-- We can observe the state at a specific point using the function observeM
observeM :: String -> M ()
observeM label = modify (observe label :: Observing State)
.
-- (4.3) Observing the IO Monad

--  We render an IO action using the pseudo-constructor <IO>, followed by an
--  observation on the returned object.

ex9 :: IO Int
ex9 = print ((observe "getChar" :: Observing (IO Char)) getChar)
-- shows
-- getChar <IO> 'x'

-- We read this as "some side effect happened, resulting in the value 'x' being returned". 

ex10 :: Char -> IO ()
ex10 ch = print (observe "putChar" :: Observing (Char -> IO ())) putChar ch)
-- shows
-- putChar
-- let fn 'x' = <IO> ()

-- We read this as "we have a function that takes 'x', does some side- effect
-- stuff, and returns unit".

-- (4.4) Multiple Observations
observations :: (Observable a) => String -> (Observer -> a) -> a
data Observer = Observer (forall a .(Observable a) => String -> a -> a)

natural :: Observer -> Int -> [Int]
natural = observations "natural" natural'
  $ \ (Observer observe) ->
  (observe "after reverse" :: Observing [Int])
  . reverse
  . (observe "after map ..." :: Observing [Int])
  . map (`mod` 10)
  . (observe "after takeWhi ...":: Observing [Int])
  . takeWhile (/= 0)
  . (observe "after iterate ...":: Observing [Int])
  . iterate (`div` 10)

-- This example outputs:
-- -- natural
--     { \ 3408 -> 3 : 4 : 0 : 8 : []
--     }
--     -- after reverse
--     3 : 4 : 0 : 8 : []
--     -- after map
--     8 : 0 : 4 : 3 : []
--     -- after takeWhile
--     3408 : 340 : 34 : 3 : []
--     -- after iterate
--     3408 : 340 : 34 : 3 : 0 : _
-- -- natural
--     { \ 123 -> 1 : 2 : 3 : []
--     }
--     -- after reverse
--     1 : 2 : 3 : []
--     -- after map
--     3 : 2 : 1 : []
--     -- after takeWhile
--     123 : 12 : 1 : []
--     -- after iterate
--     123 : 12 : 1 : 0 : _
