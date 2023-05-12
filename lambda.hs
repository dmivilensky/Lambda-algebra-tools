import Data.List
import Inversions

toSubscript x = if x == 0 then "₀" else helper x where
    helper 0 = ""
    helper x = helper (x `div` 10) ++ subscript where
        subscript = case x `rem` 10 of
            0 -> "₀"
            1 -> "₁"
            2 -> "₂"
            3 -> "₃"
            4 -> "₄"
            5 -> "₅"
            6 -> "₆"
            7 -> "₇"
            8 -> "₈"
            9 -> "₉"

data SphereSpectrum = SphereSpectrum Int [Int]

instance Show SphereSpectrum where
    show (SphereSpectrum i s) = concatMap (("s" ++) . toSubscript) s ++ " i" ++ toSubscript i

data Lie a = Unit a | Commutator (Lie a) (Lie a) | Sum [Lie a]

instance Show a => Show (Lie a) where
    show (Unit a) = show a
    show (Commutator a b) = "[" ++ show a ++ ", " ++ show b ++ "]"
    show (Sum a) = intercalate " + " (map show a)

instance Functor Lie where
    fmap f (Unit a) = Unit (f a)
    fmap f (Commutator a b) = Commutator (fmap f a) (fmap f b)
    fmap f (Sum a) = Sum (map (fmap f) a)

s js (SphereSpectrum i s) = Unit $ SphereSpectrum (i `div` 2) (s ++ js)

subsequencesOfSize n xs = let l = length xs in if n>l then [] else subsequencesBySize xs !! (l-n) where
    subsequencesBySize [] = [[[]]]
    subsequencesBySize (x:xs) = let next = subsequencesBySize xs in zipWith (++) ([]:next) (map (map (x:)) next ++ [[]])

complement x [] = []
complement [] y = y
complement a@(x:xs) (y:ys) = if x /= y then y : complement a ys else complement xs ys

shuffle n m = [(x, complement x [0,1..(n+m-1)]) | x <- subsequencesOfSize n [0,1..(n+m-1)]]

notEqual [] [] = False
notEqual x [] = True
notEqual [] y = True
notEqual (x:xs) (y:ys) = (x /= y) || notEqual xs ys

unique [] = []
unique (x:xs) = x : unique (filter (notEqual (fst x) . snd) xs)

uniqueShuffles m = unique $ shuffle m m
lambdaUnit i x = Sum [Commutator (s (reverse s1) x) (s (reverse s2) x) | (s1, s2) <- uniqueShuffles i]

equivalence (Unit a) = a
equivalence (Commutator a b) = Commutator (equivalence a) (equivalence b)
equivalence (Sum as) = Sum (map equivalence as)

lambda [i] x = lambdaUnit i x
lambda (i:is) x = foldr (\j result -> equivalence (lambdaUnit j <$> result)) (lambdaUnit i x) is

-- lambda_1_lambda_1 = equivalence (lambda 1 <$> lambda 1 (SphereSpectrum 4 []))
-- lambda_2_lambda_1 = equivalence (lambda 1 <$> lambda 2 (SphereSpectrum 4 []))
-- lambda_1_lambda_1_lambda_1 = equivalence (lambda 1 <$> equivalence (lambda 1 <$> lambda 1 (SphereSpectrum 8 [])))

data FreeLie = FreeUnit Int | FreeCommutator FreeLie FreeLie | FreeSum [FreeLie]

instance Show FreeLie where
    show (FreeUnit a) = case a of
        0 -> "x"
        1 -> "y"
        2 -> "z"
        3 -> "p"
        4 -> "q"
        5 -> "u"
        6 -> "v"
        7 -> "w"
    show (FreeCommutator a b) = "[" ++ show a ++ ", " ++ show b ++ "]"
    show (FreeSum a) = intercalate " + " (map show a)

computeSuspension i [j] = if j <= i then i + 1 else i
computeSuspension i s   = computeSuspension (computeSuspension i (tail s)) [head s]

computeSuspensionLie (Unit (SphereSpectrum i s)) = FreeUnit (computeSuspension (i-1) s)
computeSuspensionLie (Commutator a b) = FreeCommutator (computeSuspensionLie a) (computeSuspensionLie b)
computeSuspensionLie (Sum as) = FreeSum (map computeSuspensionLie as)

main = do
    putStr "λ₁     = "
    print $ lambda [1] (SphereSpectrum 2 [])
    putStr "λ₁λ₁   = "
    print $ lambda [1, 1] (SphereSpectrum 4 [])
    putStr "λ₂λ₁   = "
    print $ lambda [2, 1] (SphereSpectrum 4 [])
    putStr "λ₁λ₁λ₁ = "
    print $ lambda [1, 1, 1] (SphereSpectrum 8 [])
    putStrLn ""

    putStr "λ₁     = "
    print $ computeSuspensionLie $ lambda [1] (SphereSpectrum 2 [])
    putStr "λ₁λ₁   = "
    print $ computeSuspensionLie $ lambda [1, 1] (SphereSpectrum 4 [])
    putStr "λ₂λ₁   = "
    print $ computeSuspensionLie $ lambda [2, 1] (SphereSpectrum 4 [])
    putStr "λ₁λ₁λ₁ = "
    print $ computeSuspensionLie $ lambda [1, 1, 1] (SphereSpectrum 8 [])