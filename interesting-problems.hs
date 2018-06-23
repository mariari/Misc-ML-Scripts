maxGen :: Ord t1 => (t -> t1) -> [t] -> [(t, t1)]
maxGen _   []     = []
maxGen key (x:xs) = foldl func [(x, key x)] xs
  where
    func ys@((val,greatest):_) x = (x, max greatest (key val)) : ys

maxLeft :: Ord t1 => (t -> t1) -> [t] -> [(t, t1)]
maxLeft = (reverse .) . maxGen

maxRight :: Ord t1 => (t -> t1) -> [t] -> [(t, t1)]
maxRight key = maxGen key . reverse

waterSustained :: (Ord a, Num a) => [a] -> a
waterSustained = foldl func 0 . maxR . maxL
  where
    maxL = maxLeft id
    maxR = maxRight fst
    func acc ((val,lMax), rMax) = acc + max (smaller - val) 0
      where smaller = min rMax lMax
