-- original examples from 2003, via web archive: www.haskell.org/hood/downloads/Main.hs

import Observe

{-
 - These are various examples from the documentation
 - an other places.
 -
 -} 

main = runO ex1

ex1 :: IO ()
ex1 = print 
       ((observe "list" :: Observing [Int]) [0..9])

ex2 :: IO ()
ex2 = print 
    . reverse
    . (observe "intermediate" :: Observing [Int])
    . reverse
    $ [0..9]

ex3 :: IO ()
ex3 = print 
       (take 10
          (observe "infinite list" ([0..] :: [Int]))
       )

ex4 :: IO ()
ex4 = print 
       (length
         (observe "finite list" ([1..10] :: [Int]))
       )

ex5 :: IO ()
ex5 = print 
       (length
         ((observe "finite list" :: Observing [()])
            [ error "oops!" | _ <- ([0..9] :: [Int])]
         )
       )

ex6 :: IO ()
ex6 = let xs = observe "list" ([0..9] :: [Int])
      in print (xs !! 2 + xs !! 4)

run_ex7 :: IO ()
run_ex7 = print (ex7 3408)

ex7 :: Int -> [Int]
ex7
 = (observe "after reverse" :: Observing [Int])
 . reverse 
 . (observe "after map (`mod` 10)" :: Observing [Int])
 . map (`mod` 10)
 . (observe "after takeWhile (/= 0)" :: Observing [Int])
 . takeWhile (/= 0) 
 . (observe "after iterate (`div` 10)" :: Observing [Int])
 . iterate (`div` 10)

ex8 :: IO ()
ex8 = print
    ((observe "length" :: Observing ([Int] -> Int))
        length [1..3]
    )

ex9 :: IO ()
ex9 = print
      ((observe "foldl (+) 0 [1..4]" :: Observing ((Int -> Int -> Int) -> Int -> [Int] -> Int)
       ) foldl (+) 0 [1..4]
      )

run_ex10 = print (ex10 3408)

ex10 :: Int -> [Int]
ex10
 = observe "reverse"             reverse 
 . observe "map (`mod` 10)"      map (`mod` 10)
 . observe "takeWhile (/= 0)"    takeWhile (/= 0)
 . observe "iterate (`div` 10)"  iterate (`div` 10)

ex11 :: IO ()
ex11 = do { ch <- (observe "getChar" :: Observing (IO Char))
                   getChar
          ; print ch
          }

run_ex12 = ex12 'z'

ex12 :: Char -> IO ()
ex12 ch 
 = (observe "putChar" :: Observing (Char -> IO ()))
     putChar ch

{-
 - does not work with GHC
ex13 :: Int -> [Int]
ex13 = observers "natural"   $ \ (O observe) ->
    (observe "after reverse"	:: Observing [Int])
  . reverse
  . (observe "after map"	:: Observing [Int])
  . map (`mod` 10)
  . (observe "after takeWhile"  :: Observing [Int])
  . takeWhile (/= 0) 
  . (observe "after iterate"    :: Observing [Int])
  . iterate (`div` 10)
-}

test :: [Int]
test = observe "test" [1..10]

data Mine = Cons1 Int Bool
          | Cons2
	deriving Show

instance Observable Mine where
  observer (Cons1 a b) = send "Cons1" (return Cons1 << a << b)
  observer (Cons2)     = send "Cons2" (return Cons2)

data Shaft a = Cable a Bool

instance (Observable a) => Observable (Shaft a) where
  observer (Cable a b) = send "Cable" (return Cable << a << b)

-- This works fine in GHC.
errors = print $ observe "take" take (-1) [(1::Int)..]
