import Data.List

-- TODO: make representations for S_1
data Sphere = Sphere Int [Int]

instance Show Sphere where
    show (Sphere i s) = unwords (map (("s_" ++) . show) s) ++ " i_" ++ show i

newtype Sum a = Sum [a]

instance Functor Sum where
    fmap f (Sum a) = Sum (map f a)

instance Show a => Show (Sum a) where
    show (Sum a) = intercalate " + " (map show a)

data Commutator a = Commutator a a

instance Functor Commutator where
    fmap f (Commutator a b) = Commutator (f a) (f b)

instance Show a => Show (Commutator a) where
    show (Commutator a b) = "[" ++ show a ++ ", " ++ show b ++ "]"

-- TODO: make lambda_i function
-- TODO: fix the order of suspensions (insert penultimate, I think)
lambda_1 (Sphere i s) = Commutator (Sphere (i `div` 2) (0 : s)) (Sphere (i `div` 2) (1 : s))

lambda_2 (Sphere i s) = Sum [
    Commutator (Sphere (i `div` 2) (3 : 2 : s)) (Sphere (i `div` 2) (1 : 0 : s)),
    Commutator (Sphere (i `div` 2) (2 : 1 : s)) (Sphere (i `div` 2) (3 : 0 : s)),
    Commutator (Sphere (i `div` 2) (3 : 1 : s)) (Sphere (i `div` 2) (2 : 0 : s)) ]

lambda_1_lambda_1 = lambda_1 <$> lambda_1 (Sphere 4 [])
lambda_2_lambda_1 = fmap lambda_1 <$> lambda_2 (Sphere 4 [])

main = do
    print lambda_2_lambda_1