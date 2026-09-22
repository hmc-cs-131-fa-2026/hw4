{-|
Module       : RandomArt
Description  : A program to generate random pictures
Maintainer   : CS 131, Programming Languages (Melissa O'Neill, Chris Stone, Ben Wiedermann)

Original code and concept: Chris Stone, with changes back and forth by Melissa
  O'Neill, Chris Stone, and Ben Wiedermann 

Note: When this code is finished, you can create pictures by calling functions
        such as doGray or doColor in ghci with a size, a random seed and a
        maximum nesting depth, e.g.,
                doGray 300 20304 10
        or
                doColor 300 9999 11

      If your code is working well, and you want to repeatedly generate 
        pictures, you can compile the code into an optimized binary via
        the command
            ghc -O RandomArt.hs

      As a command-line program, you can specify either no arguments, a seed, 
        or a seed and a nesting depth, e.g.,
            ./RandomArt 20400 11
      If the seed and/or maximum nesting depth is omitted, it defaults to 10.

      Also, if you want to generate faster code inside ghci, you can either
        compile your code *outside* of ghci (as shown above), or you can run
        ghci as
            ghci -fobject-code
-}

-----------------------------------------------------------------
-- YOU DON'T NEED TO READ FURTHER IN THE FILE THAN THIS ...
--
-- This code uses Haskell features we've not seen in class yet, including
-- "do notation".  Thus, it may not look like Haskell code you're familiar
-- with.
-----------------------------------------------------------------


module Main (module RandomArtAST, module RandomArtEvaluation, sampleExp, main) where

import RandomArtAST
import RandomArtEvaluation

import qualified System.Environment as SysEnv

-- A sample expression
sampleExp :: Exp

display :: Exp -> [Char]
display X = "x"                 
display Y = "y"           
display (Times left right) = display left ++ " * " ++ display right
display (Avg left right) = "average(" ++ display left ++ ", " ++ display right ++ ")"
display (SinPi e) = "sin(π * " ++ display e ++ ")"
display (CosPi e) = "cos(π * " ++ display e ++ ")"

sampleExp2 = Avg (CosPi X) (SinPi Y)

sampleExp = SinPi (CosPi (Avg (SinPi (Times (CosPi (SinPi (Avg (Avg
  (SinPi Y) (SinPi Y)) (Times (SinPi Y) (SinPi Y))))) (Avg (SinPi (Avg
  (Times (SinPi Y) (Avg X X)) (CosPi (Times Y Y)))) (SinPi (CosPi
  (SinPi (Avg X Y))))))) (Avg (Avg (SinPi (CosPi (SinPi (Avg (Times X
  Y) (Times X X))))) (CosPi (Times (CosPi (CosPi (Avg Y Y))) (SinPi
  (Times (SinPi Y) (Times Y X)))))) (CosPi (CosPi (CosPi (CosPi (SinPi
  (CosPi X)))))))))

main :: IO ()
main =
    do  args <- SysEnv.getArgs
        let (pictureSize, args') =
                case args of
                    "--size" : n : rest -> (read n, rest)
                    _                   -> (defaultPictureSize, args)
            (seed, maxDepth1, maxDepth2opt) =
                case map read args' of
                    -- If there are no command-line arguments
                    --   generate a default picture.
                    []                 -> (10, 10, Nothing)
                    -- if there's one, it's the seed
                    [arg]              -> (arg, 10, Nothing)
                    -- if there's two, they're the seed & depth (greyscale)
                    [arg1, arg2]       -> (arg1, arg2, Nothing)
                    -- if there's two, they're the seed & two depths (color)
                    [arg1, arg2, arg3] -> (arg1, arg2, Just arg3)
                    _ -> error "Too many arguments!"
        progname <- SysEnv.getProgName
        case maxDepth2opt of 
            Just maxDepth2 -> doColorAlt  pictureSize seed maxDepth1 maxDepth2
            _              -> if (maxDepth1 < 0) then
                                  doGray pictureSize seed (-maxDepth1)
                              else
                                  doColor pictureSize seed maxDepth1
    where
        -- 300 pixels on a side is a good compromise between speed
        -- and visability.
        defaultPictureSize :: Int
        defaultPictureSize = 300
