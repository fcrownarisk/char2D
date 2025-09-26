
module Cuttlefish where

import Data.List (intercalate)
import System.Random (Random(randomR), newStdGen)

data Color = Red | Orange | Yellow | Green | Blue | Purple | White | Black
    deriving (Show, Eq, Enum, Bounded)

data Pattern = 
    Solid Color 
    | Stripes [Color] 
    | Spots Color Color 
    | Waves Color Color
    | Dynamic [Pattern]
    deriving (Show, Eq)

data Chromophore = Chromophore {
    color :: Color,
    intensity :: Float  -- 0.0 to 1.0
} deriving (Show)

data Cuttlefish = Cuttlefish {
    name :: String,
    age :: Int,
    precision :: Float,  -- 0.0 to 1.0
    chromophores :: [Chromophore],
    currentPattern :: Pattern
} deriving (Show)

createCuttlefish :: String -> Int -> Float -> Cuttlefish
createCuttlefish n a p = Cuttlefish {
    name = n,
    age = a,
    precision = p,
    chromophores = [Chromophore c 1.0 | c <- [Red .. Black]],
    currentPattern = Solid Blue
}

calculateColorMix :: [Color] -> Pattern
calculateColorMix colors
    | length colors == 1 = Solid (head colors)
    | length colors == 2 = Spots (colors !! 0) (colors !! 1)
    | otherwise = Stripes colors

changeCamouflage :: Cuttlefish -> Pattern -> IO Cuttlefish
changeCamouflage cuttlefish newPattern = do
    putStrLn $ "🎨 " ++ name cuttlefish ++ " ..."
    
    let adjustmentSteps = ["Red", "Green", "Blue"]
    mapM_ (\step -> do
        putStrLn $ "  ➤ " ++ step
        return () ) adjustmentSteps
    
    putStrLn " !!！"
    return cuttlefish { currentPattern = newPattern }

-- Haskell
displayPattern :: Cuttlefish -> String
displayPattern cf = 
    "" ++ name cf ++ "" ++ patternDescription (currentPattern cf)
    where
        patternDescription (Solid c) = ++ show c
        patternDescription (Stripes cs) =  ++ intercalate "-" (map show cs)
        patternDescription (Spots c1 c2) =  ++ show c1 ++ "/" ++ show c2
        patternDescription (Waves c1 c2) =  ++ show c1 ++ "→" ++ show c2
        patternDescription (Dynamic ps) =  ++ show (length ps) ++ "<-"

calculateCamouflageEffectiveness :: Cuttlefish -> Pattern -> Float
calculateCamouflageEffectiveness cf pattern =
    let basePrecision = precision cf
        patternComplexity = case pattern of
            Solid _ -> 0.1
            Stripes cs -> 0.3 * fromIntegral (length cs)
            Spots _ _ -> 0.5
            Waves _ _ -> 0.6
            Dynamic ps -> 0.8 * fromIntegral (length ps)
    in basePrecision * (1.0 + patternComplexity)

generateDynamicPattern :: Int -> IO Pattern
generateDynamicPattern complexity = do
    gen <- newStdGen
    let colors = [Red .. Black]
        patterns = take complexity $ map Solid colors ++ 
                   [Stripes [Red, Green, Blue], Spots Yellow Black, Waves Purple Orange]
    return $ Dynamic patterns

main :: IO ()
main = do
    
    let cuttlefish = createCuttlefish 1 0.92
    
    putStrLn $ ": " ++ show cuttlefish
    putStrLn $ ": " ++ displayPattern cuttlefish

    let newPattern = Stripes [Red, Orange, Yellow]
    updatedCuttlefish <- changeCamouflage cuttlefish newPattern
    
    putStrLn $ ": " ++ displayPattern updatedCuttlefish
    
    let effectiveness = calculateCamouflageEffectiveness updatedCuttlefish newPattern
    putStrLn $ ": " ++ show effectiveness
    complexPattern <- generateDynamicPattern 3
    putStrLn $ ": " ++ patternDescription complexPattern
    where
        patternDescription (Dynamic ps) = "\\\" " ++ show (length ps) ++ "/// "
        
        patternDescription p = show p

