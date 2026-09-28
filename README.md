# Haskell Combinator & Pattern Matching Reducer

This project is a custom evaluator for a simplified, untyped subset of Haskell. Building upon basic combinator reduction, this iteration introduces value constructors and lazy pattern matching, complete with step-by-step visualization of the evaluation process.

## Key Features

* **Constructors & Pattern Matching:** Supports capitalized value constructors (e.g., `S`, `Z`) and pattern matching. The evaluator is strictly lazy, forcing argument reduction *only* when necessary to resolve a match.
* **Precise Reduction History:** Employs the State monad and context-tracking (e.g., zippers or left-right paths) to accurately record and display every single reduction step, including nested forced evaluations.
* **Custom `SnocList`:** Implements a custom sequence data type optimized for appending elements at the end in $O(1)$ time, complete with instances for `Eq`, `Show`, `Semigroup`, `Monoid`, `Functor`, `Applicative`, and `Alternative`.
* **AST Parsing:** Utilizes the `haskell-src` library to parse raw Haskell syntax and map it into a simplified internal AST.

## Usage

Run the evaluator using Cabal:

```bash
cabal run -- zadanie3 [file]

```

---

**Side effects of the architectural choices:**

* **Direct:** Implementing a custom `SnocList` instead of a standard `[a]` list optimizes the specific operation of appending history steps, but forces manual implementation of basic typeclasses and pattern matching logic for the data structure.
* **Hidden:** Using `haskell-src` in an untyped subset implies the evaluator blindly trusts the input's correctness. Applying a value to wrong arguments won't fail at the parsing stage but will result in runtime errors during reduction. Furthermore, maintaining a detailed reduction history via the State monad and zipper contexts heavily increases memory consumption and garbage collection overhead during deep recursive calls compared to a standard, side-effect-free recursive evaluator.
