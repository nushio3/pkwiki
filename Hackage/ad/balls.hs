{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, StandaloneDeriving #-}
import Numeric.AD
import ConjugateGradient (gradientDescent)
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

vanDerWaals :: (Fractional a) => a -> a
vanDerWaals r2 = -1/r2^(3)+1/r2^(6)

potential :: (Fractional a) => Pt a -> a
potential = vanDerWaals . F.sum . fmap (^2)

well :: (Fractional a) => Pt a -> a
well = (1e-4 *) . F.sum . fmap (^2)

bindingEnergy :: (Fractional a) => Molecule a -> a
bindingEnergy (Molecule xs) = global + interaction
  where
    global = sum $ map well xs
    interaction =
      F.sum $
      [potential (xs!!i - xs!!j)
      |i <- [0..n-1], j <- [(i+1)..n-1]]
    n = length xs

main :: IO ()
main = do
  forM_ [0..100000000::Int] $ \i -> do
    when (mod i 1000 == 0) $ do
      print i
      let fn :: String
          fn = printf "mol-%08d.txt" i
          ptShow = intercalate " " . map show .  F.toList
          title = "# " ++ (show $ bindingEnergy (ret!!i)) ++ "\n"
          content = unlines $ map ptShow $ runMolecule (ret!!i)
      writeFile fn $ title ++ content

  where
    initMolecule :: Molecule Double
    initMolecule = Molecule [vec3 (2*x) (sqrt $ x+1) (sqrt $ x+2) | x <- [0..12]]
    ret = gradientDescent bindingEnergy initMolecule
