# Lambda Algebra Tools

This repository provides an experimental Haskell implementation of the **mod-p Lambda algebra** and related constructions in algebraic topology.  
It builds on the classic work of:

- A. K. Bousfield, E. B. Curtis, D. M. Kan, D. G. Quillen, D. L. Rector, and J. W. Schlesinger,  
  *The mod-p lower central series and the Adams spectral sequence*, Topology 12 (1973), 97–118.  
- E. B. Curtis, *Simplicial Homotopy Theory*, Lecture Notes in Mathematics 11, Springer, 1967.

---

## Features

- **Geometric λ-words**  
  A toy model of λ-operations via shuffle products on suspensions (`s_0, s_1, …`) in the style of the original Haskell prototype.  
  Only λ is meaningful here; μ does not admit such a geometric expression.

- **Lambda algebra over an odd prime p**  
  - Generators `lambdaN`, `muN` corresponding to `\lambda(i-1)` and `\mu(i-1)` in [BCKQRS, Appendix].  
  - Differential `d^1` defined on generators by the binomial coefficient formulas (2.4′(iv)), extended via the Leibniz rule.  
  - Adem relations (2.4′(iii)) implemented as rewriting rules.  
  - Reduction to **Curtis admissible form**: leftmost–innermost strategy, canonical lex order (λ < μ, indices decrease left to right).  
  - Polynomials normalized over `F_p` (coefficients reduced mod p, zero terms removed).

- **Basis generation**  
  - Enumerates **all admissible words** up to a given degree cap.  
  - Groups them by degree and optionally prints \( d^1 \) in admissible form.  
  - Implemented with `Data.Set`, so each admissible word appears exactly once.

- **Examples included**  
  - Geometric λ-words for small suspensions.  
  - Computations of \( d^1 \) on λ and μ generators.  
  - Adem reduction samples.  
  - Basis tables for \( p=3,5 \) up to degree 30.

---

## Theory

### Free Lie algebras and lower central series
For a free group `F`, the graded quotients of the lower central series
```math
\gamma_n F / \gamma_{n+1} F
```
assemble into the free Lie algebra `L`. Over `F_p` this structure is enriched with a *restricted Lie algebra* operation `(.)^{[p]}`.

### The Lambda algebra
The **Lambda algebra** `Lambda` is a combinatorial model that encodes this structure together with the action of Steenrod operations. It provides the `E^1`–page of the Adams spectral sequence for spheres at odd primes.

- **Generators:**
  - `lambda(i-1)` in degree `2*i*(p-1) - 1`, corresponding to Steenrod operations P^i.
  - `mu(i-1)` in degree `2*i*(p-1)`, corresponding to the Bockstein applied to P^i, i.e. βP^i.

- **Relations (Adem-type, [BCKQRS 2.4′(iii)]):**  
  Products of λ–λ, λ–μ, μ–λ, μ–μ with “bad” index patterns are rewritten as sums of admissible words with binomial coefficients mod p.

- **Differential ( [BCKQRS 2.4′(iv)] ):**
  ```math
  d(\lambda_{n-1}) \;=\; \sum_{i+j=n} \binom{i+j}{i}\, \lambda_{i-1}\lambda_{j-1}, \quad n\ge 2
  ```
  ```math
  d(\mu_{n-1}) \;=\; \sum_{i+j=n} \binom{i+j}{i}\,(\lambda_{i-1}\mu_{j-1} - \mu_{i-1}\lambda_{j-1}), \quad n\ge 1
  ```
  and extended to all words by the Leibniz rule.

- **Admissible form (Curtis):**  
  Words are reduced by Adem relations to a canonical basis where indices decrease left to right and no forbidden pairs occur.

Thus, λ corresponds to the primary Steenrod operations, μ to their Bocksteins, and the whole algebra governs the structure of the Adams spectral sequence.

---

## How to Run

The main file is `mu.hs`. Compile or run with GHC:

```bash
runhaskell mu.hs
````

or

```bash
ghc mu.hs && ./mu
```

This will print:

* Geometric λ examples,
* Differential computations in the Lambda algebra,
* Adem reductions,
* Basis tables up to degree 30 (with differentials).

All output is **ASCII only** (`s_3`, `lambdaN`, `muN`) for portability.

---

## Example Output

```
=== Geometric λ part (ASCII indices) ===
lambda1          = [s_1 i_1, s_0 i_1]
lambda1^2        = [[s_1s_1 i_1, s_1s_0 i_1], [s_0s_1 i_1, s_0s_0 i_1]]
lambda2·lambda1  = [[s_3s_2s_1 i_1, s_3s_2s_0 i_1], [s_1s_0s_1 i_1, s_1s_0s_0 i_1]] + [[s_3s_1s_1 i_1, s_3s_1s_0 i_1], [s_2s_0s_1 i_1, s_2s_0s_0 i_1]] + [[s_2s_1s_1 i_1, s_2s_1s_0 i_1], [s_3s_0s_1 i_1, s_3s_0s_0 i_1]]
lambda1^3        = [[[s_1s_1s_1 i_1, s_1s_1s_0 i_1], [s_1s_0s_1 i_1, s_1s_0s_0 i_1]], [[s_0s_1s_1 i_1, s_0s_1s_0 i_1], [s_0s_0s_1 i_1, s_0s_0s_0 i_1]]]

=== Lambda algebra over F_3 (odd p) ===
d^1(lambda1):
2·lambda1 lambda1
d^1(lambda1·lambda1) (Leibniz):
lambda2 lambda1 lambda1 + 2·lambda1 lambda1 lambda2
d^1(lambda2·lambda1) (Leibniz):
lambda3 lambda1 lambda1
d^1(mu0):
0
d^1(mu1·mu0) (Leibniz):
2·lambda1 mu1 mu1 + mu1 lambda1 mu1

Adem reduction (admissible form):
reduce( lambda2 · lambda_{1+p} ) = lambda3 lambda4
reduce( lambda_{1+p} · mu0 ) = lambda4 mu1
reduce( mu0 · lambda_{1+p} ) = mu1 lambda4

=== Lambda algebra over F_5 (odd p) ===
d^1(lambda1):
2·lambda1 lambda1
d^1(lambda1·lambda1) (Leibniz):
3·lambda2 lambda1 lambda1 + 2·lambda1 lambda1 lambda2
d^1(lambda2·lambda1) (Leibniz):
3·lambda3 lambda1 lambda1 + 3·lambda2 lambda1 lambda2 + 3·lambda1 lambda2 lambda2
d^1(mu0):
0
d^1(mu1·mu0) (Leibniz):
2·lambda1 mu1 mu1 + 3·mu1 lambda1 mu1

Adem reduction (admissible form):
reduce( lambda2 · lambda_{1+p} ) = lambda3 lambda6
reduce( lambda_{1+p} · mu0 ) = lambda6 mu1
reduce( mu0 · lambda_{1+p} ) = mu1 lambda6
```
