{-|
Module       : RegexArtEvaluation
Description  : Evaluator for expressions represented as an "Exp"
Maintainer   : CS 131, Programming Languages (Melissa O'Neill, Chris Stone, Ben Wiedermann)
-}

module RandomArtEvaluation where

module RandomArtEvaluation
    ( module RandomArtEvaluation
    , module RandomArtAST) where

import qualified Data.Word                   as Word
import qualified Data.ByteString.Lazy        as Bytes
import qualified Data.ByteString.Lazy.Char8  as Char8
import qualified Data.Map.Strict             as Map
import qualified System.Random               as Random
import qualified System.Directory            as Directory
import qualified System.Process              as System
import qualified System.Exit                 as Exit
import qualified Codec.Picture               as Picture

--------------------------------------------------------------------------------
-- Expression evaluation 
--------------------------------------------------------------------------------


-- | Representation of a point with x and y coordinates in [-1, 1]
type Point = (Float, Float)


-- | Evaluate an expression at a particular point (x,y).
--   This implementation is incomplete. Complete the definition for the remaining
--   forms of "Exp", both the provided ones (e.g., "Y") and the ones you add.
--  
--   Note: Thanks to currying, the type of eval can be read as:
--     "Given an expression and a point, return the value of the expression at that point" 
--   *or* the type of eval can be read as:
--     "Given an expression, return a function that maps points to their values".
eval :: Exp -> Point -> Float
eval X             (x,_) = x
eval (SinPi e)     point = sin(pi * eval e point)
eval _             _     = undefined -- FIXME


--------------------------------------------------------------------------------
-- Expression generation
--
-- To make random art, we randomly generate many different expressions.
--
-- A note about randomness in Haskell...
-- 
--   Haskell functions have no side-effects (i.e., for a given input, a function
--     always returns exactly the same result). This property rules out calls to
--     a random-number-generating routine.
--
--   There are a couple ways to get around this. Here's how we decided to do it:
--     Rather than have the function generate its own random numbers, we pass
--     in, as an extra *input*,  an infinite list of "random" numbers (here,
--     floating-point values in the interval [0,1]). The function can use as
--     many of these numbers as it needs.  It can even split this stream into
--     several infinite lists, if need be.
--  
--   It is still true that the function returns the same answer every
--     time we call it with the same argument (i.e., every time we pass
--     in the same infinite list of random numbers)!
--------------------------------------------------------------------------------


-- | An infinite list of pseudorandom floating-point numbers, each between 0.0 and 1.0
type RandomFloats = [Float]


-- | Build a random expression
--   Given a maximum expression-nesting depth and an inexhaustible
--      source (infinite list) of random numbers between 0.0 and 1.0,
--      return a randomly-constructed symbolic expression
--   The depth should keep the expression from growing too large.
--   Warning: if you are generating two subexpressions, be sure not
--      to use the same random numbers for both!
build :: Int -> RandomFloats -> Exp
build d (r:rs)
    | r < 0.5   = X
    | otherwise = Y


-- | Turn one infinite stream of random floats into two
splitRandomFloats :: RandomFloats -> (RandomFloats, RandomFloats)
splitRandomFloats = evenodds


-- | evenodds [a,b,c,d,...] --> ([a,c,...],[b,d,...])
evenodds :: [a] -> ([a], [a])
evenodds []     = ([],[])
evenodds (x:xs) = (x:os,es)
    where (es,os) = evenodds xs


-----------------------------------------------------------------
-- YOU DON'T NEED TO READ FURTHER IN THE FILE THAN THIS ...
--
-- This code uses Haskell features we've not seen in class yet, including
-- "do notation".  Thus, it may not look like Haskell code you're familiar
-- with.
-----------------------------------------------------------------

-- Create a visualization of a one-argument function, usage is
--      plotOneArg SinPi
-- to produce a PNG image of the function
plotOneArg :: (Exp -> Exp) -> IO ()
plotOneArg expFn =
    do  
        toPNGgray name 307 func
    where
        fullExp = expFn X
        name = takeWhile (/= ' ') (show fullExp)
        func = eval fullExp

-- Create a visualization of a two-argument function, usage is
--      plotTwoArg Times
-- to produce a PNG image of the function
plotTwoArg :: (Exp -> Exp -> Exp) -> IO ()
plotTwoArg expFn =
    do  
        toPNGgray name 307 func
    where
        fullExp = expFn X Y
        name = takeWhile (/= ' ') (show fullExp)
        func = eval fullExp

-- Create a visualization of a three-argument function, usage is
--      plotThreeArg MyThreeArgFun
-- to produce a PNG image of the function.  Because the image has only
-- two dimensions, visualizing a three-argument function is a challenge.
-- This approach uses color to achieve some level of visualization, but
-- it may not be obvious what the colors mean.
plotThreeArg :: (Exp -> Exp -> Exp -> Exp) -> IO ()
plotThreeArg expFn =
    do  
        toPNG name 307 func
    where
        fullExp1 = expFn X Y (Times X Y)
        fullExp2 = expFn (Times X Y) Y X 
        fullExp3 = expFn X (Times X Y) Y 
        name = takeWhile (/= ' ') (show fullExp1)
        func xy = (eval fullExp1 xy, eval fullExp2 xy, eval fullExp3 xy)


-- Generate a histogram of the occurrences of anything

histogram :: Ord a => [a] -> Map.Map a Int
histogram = foldl addOneMore Map.empty
    where addOneMore m k = Map.insertWith (+) k 1 m

-- Generate a histogram for an array of values in the range -1.0..1.0.
-- Counts out-of-range values too.

makeHistogram :: String -> [Float] -> IO ()
makeHistogram name list = 
    do putStrLn ("Histogram for " ++ name ++ ":") 
       mapM_ putStrLn histlines
    where coordToHisto :: Float -> Int
          coordToHisto x = if x < 0 then -(tenScale (-x)) else tenScale x
              where tenScale x = if x > 1.0 then 11 else ceiling (x * 10.0)
          histo  = histogram (map coordToHisto list)
          valFor n = case Map.lookup n histo of
                         Just v  -> v
                         Nothing -> 0
          leftBound  | valFor(-11)> 0 = -11
                     | otherwise      = -10
          rightBound | valFor  11 > 0 =  11
                     | otherwise      =  10
          results = [(x,valFor x) | x <- [leftBound..rightBound]]
          most    = maximum (1 : map snd results)
          stars n = replicate
                        (ceiling (72.0 * fromIntegral n / fromIntegral most))
                        '*'
          leftPadTo i c str = replicate (i - length str) c ++ str
          label i | i >  10   = ">>>" 
                  | i < -10   = "<<<"
                  | otherwise = leftPadTo 3 ' ' (show i)
          doline (i,v) = label i ++ " : " ++ stars v
          histlines = map doline results

-- The call
--      testDomain n
-- produces 2n+1 values in the range -1..1 (n negative, zero, n positive)

testDomain :: Integral a => a -> [Float]
testDomain size = [fromIntegral x / fromIntegral size | x <- [-size..size]]

-- Create a histogram of a one-argument function, usage is
--      histoOneArg SinPi
-- to print out a simple ASCII hisogram of the output range of the function

histoOneArg :: (Exp -> Exp) -> IO ()
histoOneArg vCons1 =
    makeHistogram name [eval fullExpr (x,x) | x <- testDomain 10496]
    where fullExpr = vCons1 X
          name     = takeWhile (/= ' ') (show fullExpr)

-- Create a histogram of a two-argument function, usage is
--      histoTwoArg Times
-- to print out a simple ASCII hisogram of the output range of the function

histoTwoArg :: (Exp -> Exp -> Exp) -> IO ()
histoTwoArg vCons2 =
    makeHistogram name [eval fullExpr (x,y) | x <- testDomain 832,
                                              y <- testDomain 832]
    where fullExpr = vCons2 X Y
          name     = takeWhile (/= ' ') (show fullExpr)

-- Create a histogram of a three-argument function, usage is
--      histoThreeArg MyThreeArgFun
-- to print out a simple ASCII hisogram of the output range of the function.
-- Due to some limitations in providing input values, the results aren't
-- quite as great as we might like, but should be good enough.

histoThreeArg :: (Exp -> Exp -> Exp -> Exp) -> IO ()
histoThreeArg vCons3 =
    makeHistogram name [r | x <- testDomain 587,
                            y <- testDomain 587,
                            r <- [f1 (x,y), f2 (x,y), f3 (x,y)]]
    where
        name = takeWhile (/= ' ') (show $ vCons3 X X X)
        f1 = eval (vCons3 X Y (Times X Y))
        f2 = eval (vCons3 (Times X Y) Y X)
        f3 = eval (vCons3 X (Times X Y) Y)


-- Emit a random grayscale png, given a maximum depth and a seed.
doGray :: Int -> Int -> Int -> IO ()
doGray pictureSize seed maxDepth =
    do putStrLn "───────────────────────────────"
       putStrLn ("seed = "   ++ show seed)
       putStrLn ("maxDepth = "   ++ show maxDepth ++ "\n")
       putStrLn exprText
       toPNGgray baseName pictureSize f
       toTXT baseName exprText
         where expr     = build1 seed maxDepth
               exprText = "g(X,Y) = " ++ show expr ++ "\n"
               f        = eval expr
               baseName = "gray_" ++ show seed ++ "_" ++ show maxDepth

-- Emit a random color png, given a maximum depth and a seed.
doColor :: Int -> Int -> Int -> IO ()
doColor pictureSize seed maxDepth =
    do putStrLn "───────────────────────────────"
       putStrLn ("seed = "   ++ show seed)
       putStrLn ("maxDepth = "   ++ show maxDepth ++ "\n")
       putStrLn exps
       toPNG baseName pictureSize colorFn
       toTXT baseName exps 
          where (rExp, gExp, bExp) = build3 seed maxDepth
                colorFn xy = (eval rExp xy, eval gExp xy, eval bExp xy)
                exps = "red(X,Y) = "   ++ show rExp ++ "\n\
                    \green(X,Y) = " ++ show gExp ++ "\n\ 
                    \blue(X,Y) = "  ++ show bExp ++ "\n "
                baseName = "color3_" ++ show seed ++ "_" ++ show maxDepth

-- Emit a random color png, given a two maximum depths and a seed.
doColorAlt :: Int -> Int -> Int -> Int -> IO ()
doColorAlt pictureSize seed maxDepth1 maxDepth2 =
    do putStrLn "───────────────────────────────"
       putStrLn ("seed = "   ++ show seed)
       putStrLn ("maxDepth1 = "   ++ show maxDepth1)
       putStrLn ("maxDepth2 = "   ++ show maxDepth2 ++ "\n")
       putStrLn exps
       toPNG baseName pictureSize colorFn
       toTXT baseName exps
          where (pExp, qExp, rExp, gExp, bExp) = build5 seed maxDepth1 maxDepth2
                pFn        = eval pExp
                qFn        = eval qExp
                redShift   = eval rExp
                greenShift = eval gExp
                blueShift  = eval bExp
                colorFn xy = (redShift pq, greenShift pq, blueShift pq)
                   where pq = (pFn xy, qFn xy)
                exps = "p(X,Y) = " ++ show pExp ++ "\n\
                    \q(X,Y) = " ++ show qExp ++ "\n\n\ 
                    \red(Xp,Yq) = "   ++ show rExp ++ "\n\ 
                    \green(Xp,Yq) = " ++ show gExp ++ "\n\ 
                    \blue(Xp,Yq) = "  ++ show bExp ++ "\n"
                baseName = "color3alt_" ++ show seed ++ "_" ++ show maxDepth1
                           ++ "_" ++ show maxDepth2

-- Create one expression, given a maximum depth and an integer "seed"
--   used to create a list of random numbers.
build1 :: Int -> Int -> Exp
build1 seed maxDepth = build maxDepth randomStream
    where
        -- randomStream is an infinite pseudorandom list of
        --   floating-point numbers in the interval [0,1]
        --   initialized using the seed.  Different seeds produce
        --   different infinite lists, but the same seed repeatedly
        --   produces the same sequence.
        randomStream :: RandomFloats
        randomStream = Random.randoms (Random.mkStdGen seed)

-- Create three expressions, given a maximum depth and an integer "seed"
--    used to create lists of random numbers.
build3 :: Int -> Int -> (Exp, Exp, Exp)
build3 seed maxDepth = (rExp, gExp, bExp)
    where
        rExp  = build maxDepth rs1
        gExp  = build maxDepth rs2
        bExp  = build maxDepth rs3
        rs = Random.randoms (Random.mkStdGen seed)
        (rs1, rsA) = splitRandomFloats rs
        (rs2, rs3) = splitRandomFloats rsA

-- Create five expressions, given a maximum depth and an integer "seed"
--    used to create a list of random numbers.  The first two will be of
--    maxDepth1 and used to create the p and q expressions, while
--    the last three are of maxDepth2 and are used to create the red, green
--    and blue expressions (the expectation is that maxDepth1 > maxDepth2)
build5 :: Int -> Int -> Int -> (Exp, Exp, Exp, Exp, Exp)
build5 seed maxDepth1 maxDepth2 = (pExp, qExp, rExp, gExp, bExp)
    where
        pExp  = build maxDepth1 rs1
        qExp  = build maxDepth1 rs2
        rExp  = build maxDepth2 rs3
        gExp  = build maxDepth2 rs4
        bExp  = build maxDepth2 rs5
        rs = Random.randoms (Random.mkStdGen seed)
        (rsA, rsB) = splitRandomFloats rs
        (rs1, rs2) = splitRandomFloats rsA
        (rs3, rsC) = splitRandomFloats rsB
        (rs4, rs5) = splitRandomFloats rsC


-- Creates a color PNG file of the given name and dimensions
--    from 3 functions (red, green, and blue values).
toPNG :: String -> Int -> (Point -> (Float, Float, Float)) -> IO ()
toPNG baseName size f =
    do  putStrLn ("Writing " ++ pngName ++ "...")
        Picture.writePng pngName $ Picture.generateImage getPixel size size
    where
        getPixel ix iy =
          let (r,g,b) = f (pixelToCoord size ix, - (pixelToCoord size iy))
          in Picture.PixelRGB8 (toIntensity r) (toIntensity g) (toIntensity b)
        pngName = baseName ++ ".png"
        txtName = baseName ++ ".txt"

        -- Convert pixel coordinate in the picture to the [-1,1] range.
        pixelToCoord :: Int -> Int -> Float
        pixelToCoord size i = 2*(fromIntegral i / fromIntegral (size-1))-1

        -- Convert output of [-1.0,1.0] to an 8-bit word, [0,255]
        toIntensity :: Float -> Word.Word8
        toIntensity z
           | abs z <= 1.0  = round (127.5 + (127.5 * z))
           | otherwise     = error "Funciton value outside range [-1,1]"  -- Should never happen


-- Creates a grayscale PNG file of the given size from a single function
toPNGgray :: String -> Int -> (Point -> Float) -> IO ()
toPNGgray baseName size f =
    toPNG baseName size (\p -> let x = f p in (x,x,x))

-- Creates a text file of the expression corresponding to an image
toTXT :: String -> String -> IO ()
toTXT baseName expr =
    do  putStrLn ("Writing " ++ txtName ++ "...")
        writeFile txtName expr
    where
        txtName = baseName ++ ".txt"
