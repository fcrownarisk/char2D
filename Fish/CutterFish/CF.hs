{-
Cuttlefish Simulator - 模拟墨鱼的精确伪装和数学般完美的模式
Haskell的纯函数式和类型安全完美匹配墨鱼的精确性
-}

module Cuttlefish where

import Data.List (intercalate)
import System.Random (Random(randomR), newStdGen)

-- 类型定义 - Haskell的强类型系统体现墨鱼的精确性
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
    precision :: Float,  -- 伪装精度 0.0 to 1.0
    chromophores :: [Chromophore],
    currentPattern :: Pattern
} deriving (Show)

-- 纯函数 - 体现Haskell的函数式特性
-- 创建默认墨鱼
createCuttlefish :: String -> Int -> Float -> Cuttlefish
createCuttlefish n a p = Cuttlefish {
    name = n,
    age = a,
    precision = p,
    chromophores = [Chromophore c 1.0 | c <- [Red .. Black]],
    currentPattern = Solid Blue
}

-- 数学函数计算颜色混合 - 体现精确性
calculateColorMix :: [Color] -> Pattern
calculateColorMix colors
    | length colors == 1 = Solid (head colors)
    | length colors == 2 = Spots (colors !! 0) (colors !! 1)
    | otherwise = Stripes colors

-- 模拟伪装过程 - 使用递归和模式匹配
changeCamouflage :: Cuttlefish -> Pattern -> IO Cuttlefish
changeCamouflage cuttlefish newPattern = do
    putStrLn $ "🎨 " ++ name cuttlefish ++ " 正在改变伪装模式..."
    
    -- 模拟精确的颜色调整过程
    let adjustmentSteps = ["分析环境", "匹配颜色", "调整色素", "同步模式"]
    mapM_ (\step -> do
        putStrLn $ "  ➤ " ++ step
        -- 模拟处理时间
        return () ) adjustmentSteps
    
    putStrLn "  伪装完成！"
    return cuttlefish { currentPattern = newPattern }

-- 显示当前模式 - 体现Haskell的不可变性
displayPattern :: Cuttlefish -> String
displayPattern cf = 
    "墨鱼 " ++ name cf ++ " 当前模式: " ++ patternDescription (currentPattern cf)
    where
        patternDescription (Solid c) = "纯色 " ++ show c
        patternDescription (Stripes cs) = "条纹 " ++ intercalate "-" (map show cs)
        patternDescription (Spots c1 c2) = "斑点 " ++ show c1 ++ "/" ++ show c2
        patternDescription (Waves c1 c2) = "波浪 " ++ show c1 ++ "→" ++ show c2
        patternDescription (Dynamic ps) = "动态模式 (" ++ show (length ps) ++ "种)"

-- 数学计算伪装效果 - 纯函数
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

-- 生成复杂的动态模式 - 使用高阶函数
generateDynamicPattern :: Int -> IO Pattern
generateDynamicPattern complexity = do
    gen <- newStdGen
    let colors = [Red .. Black]
        patterns = take complexity $ map Solid colors ++ 
                   [Stripes [Red, Green, Blue], Spots Yellow Black, Waves Purple Orange]
    return $ Dynamic patterns

-- 主演示函数
main :: IO ()
main = do
    putStrLn "=== Haskell 墨鱼模拟器 ===\n"
    
    -- 创建精确的墨鱼 - 类型安全
    let cuttlefish = createCuttlefish "卡米洛" 1 0.92
    
    putStrLn $ "创建墨鱼: " ++ show cuttlefish
    putStrLn $ "初始状态: " ++ displayPattern cuttlefish
    putStrLn ""
    
    -- 演示伪装变化
    let newPattern = Stripes [Red, Orange, Yellow]
    updatedCuttlefish <- changeCamouflage cuttlefish newPattern
    
    putStrLn $ "更新后: " ++ displayPattern updatedCuttlefish
    
    -- 计算伪装效果
    let effectiveness = calculateCamouflageEffectiveness updatedCuttlefish newPattern
    putStrLn $ "伪装效果: " ++ show effectiveness
    putStrLn ""
    
    -- 生成复杂模式
    complexPattern <- generateDynamicPattern 3
    putStrLn $ "生成复杂模式: " ++ patternDescription complexPattern
    where
        patternDescription (Dynamic ps) = "动态模式包含 " ++ show (length ps) ++ " 种子模式"
        patternDescription p = show p