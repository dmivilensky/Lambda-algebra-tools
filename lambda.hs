import Data.List

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

-- TODO: make lambda_i function
lambda_1 x = Commutator (s [0] x) (s [1] x)

lambda_2 x = Sum [
    Commutator (s [3, 2] x) (s [1, 0] x),
    Commutator (s [2, 1] x) (s [3, 0] x),
    Commutator (s [3, 1] x) (s [2, 0] x) ]

equivalence (Unit a) = a
equivalence (Commutator a b) = Commutator (equivalence a) (equivalence b)
equivalence (Sum as) = Sum (map equivalence as)

lambda_1_lambda_1 = equivalence (lambda_1 <$> lambda_1 (SphereSpectrum 4 []))
lambda_2_lambda_1 = equivalence (lambda_1 <$> lambda_2 (SphereSpectrum 4 []))
lambda_1_lambda_1_lambda_1 = equivalence (lambda_1 <$> equivalence (lambda_1 <$> lambda_1 (SphereSpectrum 8 [])))

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
    print $ lambda_1 (SphereSpectrum 2 [])
    putStr "λ₁λ₁   = "
    print lambda_1_lambda_1
    putStr "λ₂λ₁   = "
    print lambda_2_lambda_1
    putStr "λ₁λ₁λ₁ = "
    print lambda_1_lambda_1_lambda_1
    putStrLn ""

    putStr "λ₁     = "
    print $ computeSuspensionLie $ lambda_1 (SphereSpectrum 2 [])
    putStr "λ₁λ₁   = "
    print $ computeSuspensionLie lambda_1_lambda_1
    putStr "λ₂λ₁   = "
    print $ computeSuspensionLie lambda_2_lambda_1
    putStr "λ₁λ₁λ₁ = "
    print $ computeSuspensionLie lambda_1_lambda_1_lambda_1