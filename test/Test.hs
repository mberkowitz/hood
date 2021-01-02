-- Simple regression test of HOOD; verifies that printO has expected results.
-- Tries some example expressions taken from
-- https://hackage.haskell.org/package/hood, from the home page
-- http://ku-fpg.github.io/software/hood, and from Andy Gill, "Debugging Haskell
-- by Observing Intermediate Structures" ACM SIGPLAN Haskell Workshop 2000.

import Debug.Hood.Observe
import Test.HUnit
import System.IO.Silently
import GHC.IO.Handle.FD (stdin, stdout, stderr)
import Data.Char (isSpace)
import Debug.Trace

-- test harness:

chomp::[String]->String
chomp [] = ""
chomp (s:rest) = s

blank::String->Bool
blank s = all isSpace s

-- HOOD writes "observations" to stderr as a formatted string. To be readable,
-- we break this output into lines.
type ActualObservations = [String]
type ExpectedObservations = [String]
type ActualValue = String
type ExpectedValue = String

-- (caughtO FORM) runs (printO FORM) and captures the results.
caughtO :: Show a => a -> IO (ActualValue, ActualObservations)
caughtO form = do
   (o, v) <- hCapture [stderr] $ capture_ $ printO $ form
   let obs = filter (not . blank) (lines o)
   let val = chomp (lines v)
   return (val, obs)

--- tests (printO FORM):
tprintO :: Show a => a -> ExpectedValue -> ExpectedObservations -> Test
tprintO form exVal exObs = TestCase trial
  where
    trial = do
      (acVal, acObs) <- caughtO form
      assertEqual "wrong value" exVal acVal
      assertEqual "wrong observation" exObs acObs
      return ()


-- Test Cases

-- From hackage.
h1::[Int]
h1 = [observe "+1" (+1) x | x <- observe "xs" [1..3]]
test_h1 = "test_h1 " ~: tprintO h1
          "[2,3,4]"
          ["-- +1","  { \\ 1  -> 2","  , \\ 2  -> 3","  , \\ 3  -> 4","  }",
           "-- xs","   1 :  2 :  3 : []"]

-- From the home page.
-- Hood can observe data structures:
h2 :: [Int] -> [Int]
h2 = reverse . (observe "intermediate") . reverse
test_h2  = "test_h2 " ~: tprintO (h2 [1..10])
           "[1,2,3,4,5,6,7,8,9,10]"
           ["-- intermediate",
            "   10 :  9 :  8 :  7 :  6 :  5 :  4 :  3 :  2 :  1 : []" ]

-- Hood can observe functions:
h3::Int
h3 = observe "foldl (+) 0 [1..4]" foldl (+) 0 [1..4]
test_h3  = "test_h3 " ~: tprintO h3
          "10"
          ["-- foldl (+) 0 [1..4]",
           "  { \\ { \\ 0 1  -> 1",
           "     , \\ 1 2  -> 3",
           "     , \\ 3 3  -> 6",
           "     , \\ 6 4  -> 10",
           "     } 0 ( 1 :  2 :  3 :  4 : [] )  -> 10",
           "  }" ]

-- If an argument is not examined in the function, it remains unevaluated.
h4 = observe "sum xs" (\ xs ys -> sum xs) [(0::Int)..2] [(0::Int)..]
test_h4  = "test_h4 " ~: tprintO h4
         "3"
         ["-- sum xs","  { \\ ( 0 :  1 :  2 : [] ) _  -> 3","  }"]


-- From the workshop paper: simple examples of each feature.
-- (3.1) Observing a list
ex1 = (observe "list" :: Observing [Int]) [0..9]
test_ex1 = "test_ex1" ~: tprintO ex1
         "[0,1,2,3,4,5,6,7,8,9]"
         ["-- list","   0 :  1 :  2 :  3 :  4 :  5 :  6 :  7 :  8 :  9 : []"]


-- (3.2) Observing an intermediate list
-- (observe can be used partially applied, which is the typical use scenario when
-- observing inside a point-free pipeline.)
ex2  = reverse . (observe "intermediate" :: Observing[Int]) . reverse
test_ex2 = "test_ex2" ~: tprintO (ex2 [0..10])
         "[0,1,2,3,4,5,6,7,8,9,10]"
          ["-- intermediate",
           "   10 :  9 :  8 :  7 :  6 :  5 :  4 :  3 :  2 :  1 :  0 : []"]

-- (3.3) Observing an infinite list
ex3 = take 10 (observe "infinite list" [(0::Int)..])
test_ex3 = "test_ex3" ~: tprintO ex3
         "[0,1,2,3,4,5,6,7,8,9]"
         ["-- infinite list",
          "   0 :  1 :  2 :  3 :  4 :  5 :  6 :  7 :  8 :  9 : _"]

-- (3.4) Observing lists with unevaluated elements
ex4 = length (observe "finite list" [(1::Int)..10])
test_ex4 = "test_ex4" ~: tprintO ex4
         "10"
         ["-- finite list",
          "   _ :  _ :  _ :  _ :  _ :  _ :  _ :  _ :  _ :  _ : []"]

ex5 = length $
  (observe "finite list"::Observing[Int]) [error "oops!" | _ <- [0..9]]
test_ex5 = "test_ex5" ~: tprintO ex5
         "10"
         ["-- finite list",
          "   _ :  _ :  _ :  _ :  _ :  _ :  _ :  _ :  _ :  _ : []"]

ex5a n = length $ (observe "finite list"::Observing[Int]) (replicate n (error "oops"))
test_ex5a = "test_ex5" ~: tprintO (ex5a 10)
         "10"
         ["-- finite list",
          "   _ :  _ :  _ :  _ :  _ :  _ :  _ :  _ :  _ :  _ : []"]

ex6 xs = (y !! 2 + y !! 4)
  where y = (observe "list" ::Observing[Int]) xs
test_ex6 = "test_ex6" ~: tprintO (ex6 [0..5])
         "6"
         ["-- list","   _ :  _ :  2 :  _ :  4 : _"]

-- (4.1) Observing functions
ex7 = (observe "length" length) [(1::Int)..3]
test_ex7 = "test_ex7" ~: tprintO ex7
         "3"
         ["-- length",
          "  { \\ ( _ :  _ :  _ : [] )  -> 3",
          "  }"]

-- We place observe at the caller site, and can see the effect that a specific
-- function has from this context, including higher order functions.
ex8  = (observe "foldl (+) 0 [1..4]") foldl (+) (0::Int) [1..4]
text_ex8 = "text_ex8" ~: tprintO ex8
         "10"
         ["-- foldl (+) 0 [1..4]",
          "  { \\ { \\ 0 1  -> 1",
          "     , \\ 1 2  -> 3",
          "     , \\ 3 3  -> 6",
          "     , \\ 6 4  -> 10",
          "     } 0 ( 1 :  2 :  3 :  4 : [] )  -> 10",
          "  }"]


-- A running example (before annotation):
-- natural 3408 gives ['3','4','0','8']
natural0 :: Int -> [Int]
natural0 =
  reverse
  . map (`mod` 10)
  . takeWhile (/= 0)
  . iterate (`div` 10)

test_natural0 = "test_natural0" ~: (natural0 3408) ~?= [3,4,0,8]

-- (3.5) Using more than one observer.
-- Note that observation parts are sorted by tag.
natural1 :: Int -> [Int]
natural1 =
    (observe "4 after reverse") . reverse
  . (observe "3 after map") . map (`mod` 10)
  . (observe "2 after takeWhile") . takeWhile (/= 0)
  . (observe "1 after iterate") . iterate (`div` 10)

test_natural1 = "test_natural1" ~: tprintO (natural1 3408)
              "[3,4,0,8]"
             ["-- 1 after iterate",
              "   3408 :  340 :  34 :  3 :  0 : _",
              "-- 2 after takeWhile",
              "   3408 :  340 :  34 :  3 : []",
              "-- 3 after map",
              "   8 :  0 :  4 :  3 : []",
              "-- 4 after reverse",
              "   3 :  4 :  0 :  8 : []"]


-- Returning to our natural example, we can now observe the individual
-- transformers, rather than the structures between them.
natural2 :: Int -> [Int]
natural2 =
    observe "4 reverse"            reverse
  . observe "3 map (`mod` 10)"     map (`mod` 10)
  . observe "2 takeWhile (/= 0)"   takeWhile (/= 0)
  . observe "1 iterate (`div` 10)" iterate (`div` 10)

test_natural2 = "test_natural2" ~: tprintO (natural2 3408)
              "[3,4,0,8]"
             ["-- 1 iterate (`div` 10)",
              "  { \\ { \\ 3  -> 0",
              "     , \\ 34  -> 3",
              "     , \\ 340  -> 34",
              "     , \\ 3408  -> 340",
              "     } 3408  ->  3408 :  340 :  34 :  3 :  0 : _",
              "  }",
              "-- 2 takeWhile (/= 0)",
              "  { \\ { \\ 0  -> False",
              "     , \\ 3  -> True",
              "     , \\ 34  -> True",
              "     , \\ 340  -> True",
              "     , \\ 3408  -> True",
              "     } ( 3408 :  340 :  34 :  3 :  0 : _ )  ->  3408 :  340 :  34 :  3 : []",
              "  }",
              "-- 3 map (`mod` 10)",
              "  { \\ { \\ 3  -> 3",
              "     , \\ 34  -> 4",
              "     , \\ 340  -> 0",
              "     , \\ 3408  -> 8",
              "     } ( 3408 :  340 :  34 :  3 : [] )  ->  8 :  0 :  4 :  3 : []",
              "  }",
              "-- 4 reverse",
              "  { \\ ( 8 :  0 :  4 :  3 : [] )  ->  3 :  4 :  0 :  8 : []",
              "  }" ]


-- (4.4) Multiple Observations.
-- (Based on www.haskell.org/hood/downloads/Main.hs from 2003).
-- Now printO $ (natural1 3408, natural1 123) will output intermingled
-- observations, but printO $ (natural3 3408, natural3 123) will not.

natural3 :: Int -> [Int]
natural3 = observers "natural" $ \(O observe) ->
    (observe "4 after reverse"    :: Observing [Int])
  . reverse
  . (observe "3 after map"        :: Observing [Int])
  . map (`mod` 10)
  . (observe "2 after takeWhile"  :: Observing [Int])
  . takeWhile (/= 0)
  . (observe "1 after iterate"    :: Observing [Int])
  . iterate (`div` 10)

test_natural3 = "test_natural3" ~: tprintO (natural3 3408, natural3 123)
              "([3,4,0,8],[1,2,3])"
             ["-- natural",
              "  { \\ 3408  ->  3 :  4 :  0 :  8 : []",
              "  }",
              "  -- 4 after reverse",
              "     3 :  4 :  0 :  8 : []",
              "  -- 3 after map",
              "     8 :  0 :  4 :  3 : []",
              "  -- 2 after takeWhile",
              "     3408 :  340 :  34 :  3 : []",
              "  -- 1 after iterate",
              "     3408 :  340 :  34 :  3 :  0 : _",
              "-- natural",
              "  { \\ 123  ->  1 :  2 :  3 : []",
              "  }",
              "  -- 4 after reverse",
              "     1 :  2 :  3 : []",
              "  -- 3 after map",
              "     3 :  2 :  1 : []",
              "  -- 2 after takeWhile",
              "     123 :  12 :  1 : []",
              "  -- 1 after iterate",
              "     123 :  12 :  1 :  0 : _"
             ]


tests = test [test_h1, test_h2, test_h3, test_h4,
              test_ex1, test_ex2, test_ex3, test_ex4, test_ex5, test_ex6,
              test_natural0, test_natural1, test_natural2, test_natural3]

main = do
  runTestTT tests
  return ()
