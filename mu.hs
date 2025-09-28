{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

-- main.hs
-- Implements:
--   * "Geometric" λ-words (ASCII indices), only λ (μ is not meaningful there)
--   * Lambda algebra over F_p (p odd):
--       generators λ, μ; differential d^1; extended Adem reduction
--       strict admissible (Curtis-style) normal form
--   * Basis generation up to degree cap with optional printing of d^1
-- Design notes:
--   * Words are ordered lexicographically by a Curtis order on generators:
--       Lam < Mu, and within each family indices decrease left-to-right.
--   * Polynomials are Maps WordL -> coeff (mod p), normalized aggressively:
--       coefficients reduced mod p and zero terms removed immediately.
--   * Basis-by-degree is Map Int (Set WordL), so there are NO duplicates.

module Main where

import Data.List (intercalate, sortBy)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.Set as S
import Data.Set (Set)
import Data.Bits ( (.&.), shiftR )

--------------------------------------------------------------------------------
-- Geometric part: λ only (ASCII indices s_3). No μ here.

toSubscript :: Int -> String
toSubscript n = '_' : show n  -- always ASCII

data SphereSpectrum = SphereSpectrum Int [Int] deriving (Eq)

instance Show SphereSpectrum where
  show (SphereSpectrum i s) =
    concatMap (("s" ++) . toSubscript) s ++ " i" ++ toSubscript i

data Lie a
  = Unit a
  | Comm (Lie a) (Lie a)
  | Plus [Lie a]
  deriving (Eq, Functor, Foldable, Traversable)

instance Show a => Show (Lie a) where
  show (Unit a)   = show a
  show (Comm a b) = "[" ++ show a ++ ", " ++ show b ++ "]"
  show (Plus xs)  = intercalate " + " (map show xs)

-- subsequences of given size
subsequencesOfSize :: Int -> [Int] -> [[Int]]
subsequencesOfSize n xs =
  let l = length xs
  in if n>l then [] else table xs !! (l-n)
  where
    table []     = [[[]]]
    table (y:ys) = let t = table ys
                   in zipWith (++) ([]:t) (map (map (y:)) t ++ [[]])

-- complement of a sorted subset in the interval [i..j]
-- (separate name to avoid clash with Data.Bits.complement)
compRange :: [Int] -> Int -> Int -> [Int]
compRange [] i j = [i..j]
compRange a@(x:xs) i j
  | i>j       = []
  | x /= i    = i : compRange a (i+1) j
  | otherwise = compRange xs (i+1) j

shuffle2 :: Int -> Int -> [([Int],[Int])]
shuffle2 n m = [(xs, compRange xs 0 (n+m-1))
               | xs <- subsequencesOfSize n [0..(n+m-1)]]

-- filter out trivial equal pairs
notEqual :: [Int] -> [Int] -> Bool
notEqual [] []         = False
notEqual [] _          = True
notEqual _  []         = True
notEqual (a:as) (b:bs) = a/=b || notEqual as bs

unique2 :: [([Int],[Int])] -> [([Int],[Int])]
unique2 []     = []
unique2 (z:zs) = z : unique2 (filter (\w -> notEqual (fst z) (snd w)) zs)

uniqueShuffles2 :: Int -> [([Int],[Int])]
uniqueShuffles2 m = unique2 (shuffle2 m m)

-- suspension marker
s :: [Int] -> SphereSpectrum -> Lie SphereSpectrum
s js (SphereSpectrum i ss) = Unit (SphereSpectrum (i `div` 2) (ss ++ js))

-- one geometric λ
lambdaUnitGeom :: Int -> SphereSpectrum -> Lie SphereSpectrum
lambdaUnitGeom i x =
  Plus [ Comm (s (reverse s1) x) (s (reverse s2) x)
       | (s1,s2) <- uniqueShuffles2 i ]

-- flatten Lie (Lie a) -> Lie a
equivalence :: Lie (Lie a) -> Lie a
equivalence (Unit a)     = a
equivalence (Comm a b)   = Comm (equivalence a) (equivalence b)
equivalence (Plus terms) = Plus (map equivalence terms)

lambdaGeom :: [Int] -> SphereSpectrum -> Lie SphereSpectrum
lambdaGeom is x = foldl (\acc j -> equivalence $ lambdaUnitGeom j <$> acc) (Unit x) is

--------------------------------------------------------------------------------
-- Lucas binomial coefficients mod p, with a small per-digit cache

digitsBase :: Int -> Int -> [Int]
digitsBase _ 0 = [0]
digitsBase p n = go n where
  go 0 = []
  go t = (t `mod` p) : go (t `div` p)

factRow :: Int -> [Int]
factRow p = scanl (\a b -> (a*b) `mod` p) 1 [1..p-1]

invModPrime :: Int -> Int -> Int
invModPrime p a = pow a (p-2)
  where
    pow _ 0 = 1
    pow b e
      | e .&. 1 == 0 = let t = pow b (e `shiftR` 1) in (t*t) `mod` p
      | otherwise    = (b * pow b (e-1)) `mod` p

smallChoose :: Int -> Int -> Int -> Int
smallChoose p a b
  | b<0 || b>a   = 0
  | b==0 || b==a = 1
  | otherwise    =
      let fr = factRow p
          fa = fr !! a
          fb = fr !! b
          fc = fr !! (a-b)
      in (fa * invModPrime p ((fb*fc) `mod` p)) `mod` p

binomModP :: Int -> Int -> Int -> Int
binomModP p n k
  | k<0 || k>n = 0
  | otherwise  = foldl step 1 (zip (digitsBase p n) (digitsBase p k))
  where
    step acc (ni,ki)
      | ki > ni   = 0
      | otherwise = (acc * smallChoose p ni ki) `mod` p

--------------------------------------------------------------------------------
-- Lambda algebra core

data Prime = OddP Int deriving (Eq, Show)

data Gen = Lam Int | Mu Int deriving (Eq)

-- Curtis-style order: Lam < Mu, indices decrease left-to-right
instance Ord Gen where
  compare (Lam i) (Lam j) = compare j i
  compare (Mu  i) (Mu  j) = compare j i
  compare (Lam _) (Mu  _) = LT
  compare (Mu  _) (Lam _) = GT

instance Show Gen where
  show (Lam k) = "lambda" ++ show (k+1)
  show (Mu  k) = "mu"     ++ show (k+1)

newtype WordL = W [Gen] deriving (Eq)

-- Words ordered lexicographically by the above generator order
instance Ord WordL where
  compare (W a) (W b) = compare a b

instance Show WordL where
  show (W [])   = "1"
  show (W gens) = intercalate " " (map show gens)

(⋅) :: WordL -> WordL -> WordL
W a ⋅ W b = W (a ++ b)

lam :: Int -> WordL
lam i | i>=1      = W [Lam (i-1)]
      | otherwise = error "lam: i must be >=1"

muW :: Int -> WordL
muW i | i>=1      = W [Mu (i-1)]
      | otherwise = error "mu: i must be >=1"

degGen :: Int -> Gen -> Int
degGen p (Lam (i1)) = 2*(i1+1)*(p-1) - 1
degGen p (Mu  (i1)) = 2*(i1+1)*(p-1)

degW :: Int -> WordL -> Int
degW p (W gs) = sum (map (degGen p) gs)

--------------------------------------------------------------------------------
-- Poly over F_p as Map WordL -> coeff, normalized everywhere

type Coeff = Int
type Poly  = Map WordL Coeff

pNorm :: Int -> Coeff -> Coeff
pNorm p c = let r = c `mod` p in if r<0 then r+p else r

pAdd :: Int -> Coeff -> Coeff -> Coeff
pAdd p a b = pNorm p (a+b)

pScale :: Int -> Coeff -> Coeff -> Coeff
pScale p a b = (a*b) `mod` p

normPoly :: Int -> Poly -> Poly
normPoly p = M.filter (/=0) . M.map (pNorm p)

zeroP :: Poly
zeroP = M.empty

singletonP :: Int -> Coeff -> WordL -> Poly
singletonP p c w = normPoly p (M.singleton w c)

addP :: Int -> Poly -> Poly -> Poly
addP p a b = normPoly p (M.unionWith (pAdd p) a b)

scaleP :: Int -> Coeff -> Poly -> Poly
scaleP p c = normPoly p . M.map (pScale p c)

mulRight :: Int -> Poly -> WordL -> Poly
mulRight p poly w = normPoly p $
  M.fromListWith (pAdd p) [ (w1 ⋅ w, c) | (w1,c) <- M.toList poly ]

mulLeft :: Int -> WordL -> Poly -> Poly
mulLeft p w poly = normPoly p $
  M.fromListWith (pAdd p) [ (w ⋅ w1, c) | (w1,c) <- M.toList poly ]

showPoly :: Int -> Poly -> String
showPoly p mp0 =
  let mp = normPoly p mp0
  in if M.null mp then "0"
     else
       let items = M.toList mp
           showTerm (w,c) = let c' = pNorm p c
                            in if c' == 1 then show w else show c' ++ "·" ++ show w
       in intercalate " + " (map showTerm items)

--------------------------------------------------------------------------------
-- Differential d^1 (BCKQRS 2.4′(iv)) + Leibniz

d1Gen :: Prime -> WordL -> Poly
d1Gen (OddP p) (W [Lam (n1)]) =
  let n = n1 + 1
  in if n >= 2
     then normPoly p $ M.fromListWith (pAdd p)
            [ (lam i ⋅ lam j, binomModP p (i+j) i)
            | i <- [1..n-1], let j = n - i ]
     else zeroP
d1Gen (OddP p) (W [Mu (n1)]) =
  let n = n1 + 1
      part1 = [ (lam i ⋅ muW j,            binomModP p (i+j) i)
              | i <- [1..n-1], let j = n-i, j>=1 ]
      part2 = [ (muW i ⋅ lam j,  (p - binomModP p (i+j) i) `mod` p)
              | i <- [1..n-1], let j = n-i, j>=1 ]
  in normPoly p $ M.fromListWith (pAdd p) (part1 ++ part2)
d1Gen _ _ = zeroP

d1 :: Prime -> WordL -> Poly
d1 pr@(OddP p) (W [])     = zeroP
d1 pr           (W [g])   = d1Gen pr (W [g])
d1 pr@(OddP p)  (W (g:gs)) =
  let left  = W [g]
      right = W gs
      dl    = d1 pr left
      dr    = d1 pr right
      sgn   = if odd (degW p left) then (p-1) else 1
  in addP p (mulRight p dl right) (scaleP p sgn (mulLeft p left dr))

--------------------------------------------------------------------------------
-- Adem reduction (Curtis admissible normal form)

-- Local Adem expansion for an adjacent pair; Nothing means already admissible.
-- NOTE: This encodes the mixed Adem relations 2.4′(iii) for odd primes.
ademPair :: Prime -> Gen -> Gen -> Maybe Poly
ademPair pr@(OddP p) a b =
  case (a,b) of
    -- lambda_{i-1+m} lambda_{j-1+pm}
    (Lam i1, Lam j1) ->
      let i = i1+1; j = j1+1
          ms = [ m | m <- [1..i+j+3], j1 == (j-1 + p*m) - 1 ]
      in case ms of
           (m:_) ->
             let n = i + j - 1 - m
             in Just $ normPoly p $ M.fromListWith (pAdd p)
                 [ (lam (u+m) ⋅ lam (v + p*m), binomModP p (u+v) u)
                 | u <- [0..n], let v = n-u ]
           _ -> Nothing
    -- lambda_{i-1+m} mu_{j-1+pm}
    (Lam i1, Mu j1) ->
      let i = i1+1; j = j1+1
          ms = [ m | m <- [1..i+j+3], j1 == (j-1 + p*m) - 1 ]
      in case ms of
           (m:_) ->
             let n = i + j - 1 - m
             in Just $ normPoly p $ M.fromListWith (pAdd p)
                 [ (lam (u+m) ⋅ muW (v + p*m), binomModP p (u+v) u)
                 | u <- [0..n], let v = n-u ]
           _ -> Nothing
    -- mu_{i-1+m} lambda_{j-1+pm}
    (Mu i1, Lam j1) ->
      let i = i1+1; j = j1+1
          ms = [ m | m <- [1..i+j+3], j1 == (j-1 + p*m) - 1 ]
      in case ms of
           (m:_) ->
             let n = i + j - 1 - m
             in Just $ normPoly p $ M.fromListWith (pAdd p)
                 [ (muW (u+m) ⋅ lam (v + p*m), (p - binomModP p (u+v) u) `mod` p)
                 | u <- [0..n], let v = n-u ]
           _ -> Nothing
    -- mu_{i-1+m} mu_{j + pm}
    (Mu i1, Mu j1) ->
      let i = i1+1; j = j1+1
          ms = [ m | m <- [0..i+j+3], j1 == (j + p*m) - 1 ]
      in case ms of
           (m:_) ->
             let n = i + j - 1 - m
             in Just $ normPoly p $ M.fromListWith (pAdd p)
                 [ (muW (u+m) ⋅ muW (v + p*m), binomModP p (u+v) u)
                 | u <- [0..n], let v = n-u ]
           _ -> Nothing

-- Reduce one word step (leftmost reducible pair); leaves admissible words intact.
reduceWordOnce :: Prime -> WordL -> Poly
reduceWordOnce pr@(OddP p) (W gs) = step gs
  where
    step (x:y:rest) =
      case ademPair pr x y of
        Just polyPair ->
          normPoly p $ M.fromListWith (pAdd p)
            [ (w ⋅ W rest, c) | (w,c) <- M.toList polyPair ]
        Nothing ->
          let tailRed = reduceWordOnce pr (W (y:rest))
          in if M.null tailRed
             then singletonP p 1 (W (x:y:rest))
             else normPoly p $ M.fromListWith (pAdd p)
                    [ (W (x:ws), c) | (W ws, c) <- M.toList tailRed ]
    step _ = singletonP p 1 (W gs)

-- Iterate reductions until a fixed point (admissible normal form).
reduceWordFull :: Prime -> WordL -> Poly
reduceWordFull pr w0 = loop (singletonP p 1 w0)
  where
    OddP p = pr
    loop poly =
      let step = normPoly p $ M.fromListWith (pAdd p) . concat
               $ [ M.toList (reduceWordOnce pr w) >>= \(w',c') -> [(w', pScale p c c')]
                 | (w,c) <- M.toList poly ]
      in if step == poly then poly else loop step

-- Reduce a polynomial by reducing each word and summing up.
reducePolyFull :: Prime -> Poly -> Poly
reducePolyFull pr@(OddP p) poly =
  normPoly p $ M.fromListWith (pAdd p) . concat
    $ [ M.toList (scaleP p c (reduceWordFull pr w))
      | (w,c) <- M.toList poly ]

-- Shortcut
toAdmissible :: Prime -> WordL -> Poly
toAdmissible = reduceWordFull

--------------------------------------------------------------------------------
-- Basis generation up to a degree cap (Curtis admissible words only)

-- All generators allowed by degree ≤ cap
gensUpTo :: Int -> Int -> [Gen]
gensUpTo p cap =
  let ls = takeWhile (\(i,_) -> 2*i*(p-1)-1 <= cap) [ (i, Lam (i-1)) | i <- [1..] ]
      ms = takeWhile (\(i,_) -> 2*i*(p-1)   <= cap) [ (i, Mu  (i-1)) | i <- [1..] ]
  in map snd (ls ++ ms)

-- Extend words on the right, keeping local admissibility and degree bound
extendWords :: Prime -> Int -> Set WordL -> Set WordL
extendWords pr@(OddP p) cap ws =
  S.fromList
    [ W (gs ++ [g])
    | W gs <- S.toList ws
    , let w = W gs
    , g <- gensUpTo p (cap - degW p w)
    , case gs of
        []     -> True
        _      -> case ademPair pr (last gs) g of
                    Nothing -> True
                    Just _  -> False
    ]

-- BFS-style generation of all admissible words with degree ≤ cap
basisByDegree :: Prime -> Int -> Map Int (Set WordL)
basisByDegree pr@(OddP p) cap = go (M.singleton 0 (S.singleton (W [])))
  where
    go acc =
      let current = S.unions (M.elems acc)
          next    = extendWords pr cap current
          acc'    = S.foldl'
                      (\m w -> if w == W []
                                 then m
                                 else M.insertWith S.union (degW p w) (S.singleton w) m)
                      M.empty next
          merged  = M.unionWith S.union acc acc'
      in if M.size merged == M.size acc
           then M.delete 0 merged
           else go merged

-- Pretty print the basis table; withD1=True also print d^1 in admissible form
printBasisTable :: Prime -> Int -> Bool -> IO ()
printBasisTable pr@(OddP p) cap withD1 = do
  putStrLn $ "=== Basis of Λ over F_" ++ show p ++ " up to degree " ++ show cap ++ " ==="
  let table = basisByDegree pr cap
  mapM_ (\(deg, ws) -> do
           putStrLn $ "-- degree " ++ show deg ++ ":"
           let ws' = S.toList ws
               wsSorted = sortBy (\w1 w2 -> compare w1 w2) ws'
           mapM_ (\w -> do
                    putStrLn $ "  " ++ show w
                    if withD1
                      then do
                        let rhs = reducePolyFull pr (d1 pr w)
                        putStrLn $ "    d^1 = " ++ showPoly p rhs
                      else pure ()
                 ) wsSorted
        ) (M.toList table)
  putStrLn ""

--------------------------------------------------------------------------------
-- Demo

demoGeom :: IO ()
demoGeom = do
  putStrLn "=== Geometric λ part (ASCII indices) ==="
  putStr "lambda1          = " ; print $ lambdaGeom [1]     (SphereSpectrum 2 [])
  putStr "lambda1^2        = " ; print $ lambdaGeom [1,1]   (SphereSpectrum 4 [])
  putStr "lambda2·lambda1  = " ; print $ lambdaGeom [2,1]   (SphereSpectrum 4 [])
  putStr "lambda1^3        = " ; print $ lambdaGeom [1,1,1] (SphereSpectrum 8 [])
  putStrLn ""

demoLambda :: Prime -> IO ()
demoLambda pr@(OddP p) = do
  putStrLn $ "=== Lambda algebra over F_" ++ show p ++ " (odd p) ==="
  let w1 = lam 2                       -- lambda1
      w2 = lam 2 ⋅ lam 2               -- lambda1 lambda1
      w3 = lam 3 ⋅ lam 2               -- lambda2 lambda1
      m1 = muW 1                       -- mu0
      m2 = muW 2 ⋅ muW 1               -- mu1 mu0
      x1 = lam 3 ⋅ lam (1+p)           -- sample for reduction
      x2 = lam (1+p) ⋅ muW 1           -- mixed pair
      x3 = muW 1 ⋅ lam (1+p)

  putStrLn "d^1(lambda1):"
  putStrLn (showPoly p (d1 pr w1))
  putStrLn "d^1(lambda1·lambda1) (Leibniz):"
  putStrLn (showPoly p (d1 pr w2))
  putStrLn "d^1(lambda2·lambda1) (Leibniz):"
  putStrLn (showPoly p (d1 pr w3))
  putStrLn "d^1(mu0):"
  putStrLn (showPoly p (d1 pr m1))
  putStrLn "d^1(mu1·mu0) (Leibniz):"
  putStrLn (showPoly p (d1 pr m2))
  putStrLn ""

  putStrLn "Adem reduction (admissible form):"
  putStrLn $ "reduce( lambda2 · lambda_{1+p} ) = " ++ showPoly p (toAdmissible pr x1)
  putStrLn $ "reduce( lambda_{1+p} · mu0 ) = "     ++ showPoly p (toAdmissible pr x2)
  putStrLn $ "reduce( mu0 · lambda_{1+p} ) = "     ++ showPoly p (toAdmissible pr x3)
  putStrLn ""

main :: IO ()
main = do
  -- All ASCII output
  demoGeom
  demoLambda (OddP 3)
  demoLambda (OddP 5)
  -- Basis tables (including d^1)
  printBasisTable (OddP 3)  30 True
  printBasisTable (OddP 5)  30 True
