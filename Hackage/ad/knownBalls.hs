{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, StandaloneDeriving #-}
import Numeric.AD
import Numeric.AD.Newton (gradientDescent)
-- import ConjugateGradient (gradientDescent)
import Data.Tensor.TypeLevel
import Data.List (intercalate)
import Control.Monad
import qualified Data.Foldable as F
import Data.Traversable (Traversable(..))
import Text.Printf

type Pt = Vec3

newtype Molecule a = Molecule {runMolecule :: [Pt a]} deriving (Eq,Show)

deriving instance Functor Molecule
deriving instance F.Foldable Molecule
deriving instance Traversable Molecule

spring :: (Fractional a) => a -> Pt a -> Pt a -> a
spring length x y = (^2) $ (length^2-) $ F.sum $ fmap (^2) $ x-y

well :: (Fractional a) => Pt a -> a
well = (\r2 -> (r2-1)^2) . F.sum . fmap (^2)

bindingEnergy :: (Floating a) => Molecule a -> a
bindingEnergy (Molecule xs) = global + interaction
  where
    global = sum $ map well xs
    interaction =
      F.sum $
      [spring (2 * sin angle1) (xs!!i) (xs!!j)
      |i <- [0..n-1], let j =(i+1)`mod`n] ++
      [spring (2 * sin angleHalf) (xs!!i) (xs!!j)
      |i <- [0..n-1], let j =(i+halfN)`mod`n]
    n = length xs
    halfN = length xs `div` 2
    angle1 = pi / fromIntegral n
    angleHalf = pi * fromIntegral halfN / fromIntegral n


main :: IO ()
main = do
  forM_ [0..100000000::Int] $ \i -> do
    putStrLn $ unwords [show i, show $ bindingEnergy (ret!!i)]
    when (mod i 100 == 0 && False) $ do
      print i
      let fn :: String
          fn = printf "mol-%08d.txt" i
          ptShow = intercalate " " . map show .  F.toList
          title = "# " ++ (show $ bindingEnergy (ret!!i)) ++ "\n"
          content = unlines $ map ptShow $ runMolecule (ret!!i)
      writeFile fn $ title ++ content

  where
    initMolecule :: Molecule Double
    initMolecule = Molecule [vec3 (2*x) (sqrt $ x+1) (sqrt $ x+2) | x <- [0..26]]
    ret = gradientDescent bindingEnergy initMolecule
