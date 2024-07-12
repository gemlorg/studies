module Set(Set(..), empty, null, singleton, union, fromList
              , member, toList, toAscList, elems
              ) where
import           Prelude hiding (null)

data Set a = Empty
           | Singleton a
           | Union (Set a) (Set a)

empty :: Set a
empty = Empty

null :: Set a -> Bool
null Empty         = True
null (Singleton _) = False
null (Union a b)   = null a && null b

member :: Eq a => a -> Set a -> Bool
member _ Empty         = False
member a (Singleton b) = a == b
member x (Union a b)   = member x a || member x b

singleton :: a -> Set a
singleton  = Singleton

fromList :: [a] -> Set a
fromList []     = Empty
fromList (x:xs) = Union (Singleton x) (fromList xs)

toList :: Set a -> [a]
toList Empty         = []
toList (Singleton x) = [x]
toList (Union a b)   = toList a ++ toList b

toAscList :: Ord a => Set a -> [a]
toAscList =  mergeSort . toList
    where
        -- partially provided by the copilot
        mergeSort :: Ord a => [a] -> [a]
        mergeSort [] = []
        mergeSort [x] = [x]
        mergeSort xs = merge (mergeSort left) (mergeSort right)
            where
                (left, right) = splitAt (length xs `div` 2) xs
        merge :: Ord a => [a] -> [a] -> [a]
        merge [] b = b
        merge a [] = a
        merge (x:xs) (y:ys)
            | x < y = x : merge xs (y:ys)
            | x > y = y : merge (x:xs) ys
            | otherwise = x : merge xs ys

elems :: Set a -> [a]
elems = toList

union :: Set a -> Set a -> Set a
union a Empty = a
union Empty b = b
union a b     = Union a b

insert :: a -> Set a -> Set a
insert a s = Union (Singleton a) s

instance Ord a => Eq (Set a) where
    a == b = toAscList a == toAscList b

instance Semigroup (Set a) where
    (<>) = union

instance Monoid (Set a) where
    mempty = empty
    mappend = (<>)

-- it is worth noting that the show method
-- does not require Ord a, so it can't be
-- used in the corresponding methods in Graph.hs
instance Show a => Show (Set a) where
    show s = show (toList s)

instance Functor Set where
    fmap _ Empty         = Empty
    fmap f (Singleton x) = Singleton (f x)
    fmap f (Union a b)   = Union (fmap f a) (fmap f b)
