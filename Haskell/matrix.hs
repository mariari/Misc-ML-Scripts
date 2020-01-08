import           Data.Matrix  as M
import           Control.Lens as L
import           Data.Monoid
import qualified Data.Vector as V

x3x3 :: (Num a, Enum a) => Matrix a
x3x3 = fromLists [[1..3],[4..6],[7..9]]

x3x1 = fromLists [[1],[2],[3]]


fromRGB :: Matrix Double -> Matrix Double
fromRGB = (fmap (* (1/0.17697)) mat *)
  where mat = fromLists [[0.49,   0.31,    0.20]
                        ,[0.1769, 0.81240, 0.01063]
                        ,[0.00,   0.01,    0.99]]

makeRGB :: Num a => a -> a -> a -> Matrix a
makeRGB red green blue = fromList 3 1 [red,green,blue]

-- element wise looks something like this
-- matrix 3 3 $ \k -> (fromBook ! k) + (fromBookOffset ! k)

fromBook :: (Num a, Enum a) => Matrix a
fromBook = extractWindows 3 3 matrix310 ! (2,2)

matrix310 :: (Num a, Enum a) => Matrix a
matrix310 = fromLists [[45,60,98,127,132,133,137,133]
                      ,[46,65,98,123,126,128,131,133]
                      ,[47,65,96,115,119,123,135,137]
                      ,[47,63,91,107,113,122,138,134]]

fromBookOffset = fromList 3 3 $ replicate 4 0.1 <> (0.2 : repeat 0.1)

extractWindows :: Int -> Int -> Matrix a -> Matrix (Matrix a)
extractWindows row col mat = matrix (nrows mat - row + 1) (ncols mat - col + 1) f
  where
    f (i,j) = submatrix i (i + row - 1) j (j + col - 1) mat

splitFrmaes = extractWindows

-- size of the matrix shall be l-m by k-n
linearFilter :: (RealFrac a, Integral b) => Matrix a -> Matrix a -> Matrix b
linearFilter filt = fmap (round . foldr (+) 0 . elementwise (*) filt) . extractWindows row col
  where row = nrows filt
        col = ncols filt

(⊕) :: (RealFrac a, Integral b) => Matrix a -> Matrix a -> Matrix b
(⊕) = flip linearFilter

-- matrix310 ⊕ fromBookOffset
