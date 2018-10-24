-- from http://jelv.is/blog/Generating-Mazes-with-Inductive-Graphs/

import Data.Graph.Inductive
import Data.Graph.Inductive.PatriciaTree
import Data.Char

-- taken from example!
cyc3 :: Gr Char String
cyc3 = buildGr -- cycle of three nodes
       [([("ca",3)],1, 'a', [("ab",2)])
       ,([],        2, 'b', [("bc",3)])
       ,([],        3, 'c', [])
       ]

ghead :: Graph gr => gr a b -> Maybe a
ghead g
  | isEmpty g = Nothing
  | otherwise = Just l
  where
    ((_,_,l,_),_) = matchAny g

-- oddly enough this doesn't get inferred
mapNodes :: DynGraph gr => (a -> b) -> gr a e -> gr b e
mapNodes f g
  | isEmpty g = empty
  | otherwise = (in', node, f label, out) & mapNodes f g'
  where
    ((in', node, label, out), g') = matchAny g
