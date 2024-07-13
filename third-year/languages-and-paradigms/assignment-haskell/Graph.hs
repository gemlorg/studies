module Graph where
import qualified Set as Set
import           Set (Set)
class Graph g where
  empty   :: g a
  vertex  :: a -> g a
  union   :: g a -> g a -> g a
  connect :: g a -> g a -> g a

data Relation a = Relation { domain :: Set a, relation :: Set (a, a) }
    deriving (Eq, Show)

data Basic a = Empty
             | Vertex a
             | Union (Basic a) (Basic a)
             | Connect (Basic a) (Basic a)

instance Graph Relation where
  empty = Relation { domain = Set.empty, relation = Set.empty }
  vertex x = Relation { domain = Set.singleton x, relation = Set.empty }
  union x y = Relation { domain = Set.union (domain x) (domain y)
                       , relation = Set.union (relation x) (relation y) }
  connect x y = Relation { domain = Set.union (domain x) (domain y)
                         , relation = Set.union  (Set.union (relation x) (relation y)) added }
    where added = Set.fromList [ (a, b) | a <- Set.toList (domain x), b <- Set.toList (domain y)]

instance (Ord a, Num a) => Num (Relation a) where
  fromInteger = vertex . fromInteger
  (+) x y = Relation { domain = Set.fromList $ Set.toAscList $ Set.union (domain x) (domain y)
                       , relation = Set.fromList $ Set.toAscList $ Set.union (relation x) (relation y) }
  (*) x y = Relation { domain = Set.fromList $ Set.toAscList $ Set.union (domain x) (domain y)
                         , relation = Set.fromList $ Set.toAscList $ Set.union  (Set.union (relation x) (relation y)) added }
      where added = Set.fromList [ (a, b) | a <- Set.toAscList (domain x), b <- Set.toAscList (domain y)]
  signum      = const empty
  abs         = id
  negate      = id

instance Graph Basic where
  empty = Empty
  vertex x = Vertex x
  union a b = Union a b
  connect a b = Connect a b

instance Ord a => Eq (Basic a) where
  a == b = getEdgesVertices a == getEdgesVertices b

instance (Ord a, Num a) => Num (Basic a) where
    fromInteger = vertex . fromInteger
    (+)         = union
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance Semigroup (Basic a) where
  (<>) = union

instance Monoid (Basic a) where
  mempty = Empty
  mappend = (<>)

fromBasic :: Graph g => Basic a -> g a
fromBasic Empty         = empty
fromBasic (Vertex  x)   = vertex x
fromBasic (Union  x y)  = union (fromBasic x) (fromBasic y)
fromBasic (Connect x y) = connect (fromBasic x) (fromBasic y)

-- | Example graph
-- >>> example34
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]
instance (Ord a, Show a) => Show (Basic a) where
  show g = "edges " ++ show edges ++ " + vertices " ++ show isoVertices
    where
      (edges, isoVertices) = getEdgesIsoVertices g

example34 :: Basic Int
example34 = 1*2 + 2*(3+4) + (3+4)*5 + 17

todot :: (Ord a, Show a) => Basic a -> String
todot g  = "digraph {\n" ++ showEdges ++ showVertices ++ "}"
  where
    (edges, isoVertices) = getEdgesIsoVertices g
    showVertices =concatMap (\x -> show x ++ ";\n") isoVertices
    showEdges = concatMap (\(x,y) -> show x ++ " -> " ++ show y ++ ";\n") edges

instance Functor Basic where
  fmap _ Empty         = Empty
  fmap f (Vertex a)    = Vertex (f a)
  fmap f (Union a b)   = Union (fmap f a) (fmap f b)
  fmap f (Connect a b) = Connect (fmap f a) (fmap f b)

-- | Merge vertices
-- >>> mergeV 3 4 34 example34
-- edges [(1,2),(2,34),(34,5)] + vertices [17]

mergeV :: Eq a => a -> a -> a -> Basic a -> Basic a
mergeV a b c g = fmap f g where
  f x
    | x == a || x == b = c
    | otherwise =  x

instance Applicative Basic where
  pure = Vertex
  Empty <*> _       = Empty
  _ <*> Empty       = Empty
  Vertex f <*> x    = fmap f x
  Union f g <*> x   = Union (f <*> x) (g <*> x)
  Connect f g <*> x = Connect (f <*> x) (g <*> x)

instance Monad Basic where
  return = pure
  Empty >>= _       = Empty
  Vertex x >>= f    = f x
  Union x y >>= f   = Union (x >>= f) (y >>= f)
  Connect x y >>= f = Connect (x >>= f) (y >>= f)

-- | Split Vertex
-- >>> splitV 34 3 4 (mergeV 3 4 34 example34)
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

splitV :: Eq a => a -> a -> a -> Basic a -> Basic a
splitV a b c g =  g >>= f where
  f x
   | x == a =  Union (Vertex b) (Vertex c)
   | otherwise = Vertex x

getEdgesVertices :: Ord a => Basic a -> ([(a, a)], [a])
getEdgesVertices Empty = ([],[])
getEdgesVertices (Vertex a) = ([], [a])
getEdgesVertices (Union a b) = (merge e1 e2, merge v1 v2)
  where
    (e1, v1) = getEdgesVertices a
    (e2, v2) = getEdgesVertices b
getEdgesVertices (Connect a b) = ( merge (merge e1 e2) prod, merge v1  v2)
  where
    (e1, v1) = getEdgesVertices a
    (e2, v2) = getEdgesVertices b
    prod = [(x, y) | x <- v1, y <- v2]

-- same as in the Set implementation
merge :: Ord a => [a] -> [a] -> [a]
merge [] b = b
merge a [] = a
merge (x:xs) (y:ys)
    | x < y = x : merge xs (y:ys)
    | x == y = x : merge xs ys
    | otherwise = y : merge (x:xs) ys

sortedListSubtract :: Ord a => [a] -> [a] -> [a]
sortedListSubtract x [] = x
sortedListSubtract [] _ = []
sortedListSubtract (x:xs) (y:ys)
  | x < y = x : sortedListSubtract xs (y:ys)
  | x == y = sortedListSubtract xs ys
  | otherwise = sortedListSubtract (x:xs) ys

getEdgesIsoVertices :: Ord a => Basic a -> ([(a, a)], [a])
getEdgesIsoVertices g = (edges, isoVertices) where
  (edges, vertices) = getEdgesVertices g
  seenVertices = Set.toAscList $ Set.fromList $ [x| (x,_) <- edges] ++ [y| (_,y) <- edges]
  isoVertices = sortedListSubtract vertices seenVertices

