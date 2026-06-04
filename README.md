# Black Box Game Simulation

![Haskell](https://img.shields.io/badge/Haskell-Functional_Programming-purple?style=flat&logo=haskell)
![Status](https://img.shields.io/badge/Status-Year_2_Coursework-lightgrey)

> A functional Haskell implementation of the Black Box board game, featuring automated ray interaction calculation and atom position deduction using purely functional programming techniques.

---

## Overview

The Black Box game consists of an N×N grid containing hidden atoms. Players fire rays into the grid from edge positions and observe whether they are absorbed, reflected, or deflected to deduce atom locations. This project implements the game logic entirely in Haskell, demonstrating functional programming concepts including recursion, higher-order functions, polymorphism, and list processing.

The implementation solves two core challenges:
- **Challenge 1** — Given a grid and atom positions, compute all ray interaction outcomes across every possible edge entry point
- **Challenge 2** — Given a set of observed interactions, deduce all valid atom configurations that could produce them, including handling incomplete interaction lists via brute-force search

---

## Game Rules

A ray fired into the grid behaves as follows:

| Outcome | Condition |
|---------|-----------|
| **Absorbed** | Ray strikes an atom directly — does not exit the grid |
| **Deflected** | Ray strikes the corner of an atom — redirected 90 degrees |
| **Reflected** | Ray exits at the same edge it entered — via edge reflection or double deflection |
| **Path** | Ray exits at a different edge position than it entered |

---

## Implementation

### Challenge 1 — `calcBBInteractions`

```haskell
calcBBInteractions :: Int -> Atoms -> Interactions
```

Computes all ray interactions for an N×N grid given a list of atom positions. Key functions:

| Function | Description |
|----------|-------------|
| `toCoords` | Converts edge position to grid coordinate |
| `initialDirection` | Determines ray's starting direction from edge |
| `edgeReflection` | Detects immediate edge reflection at entry |
| `deflectRay` | Computes deflection or reflection marking |
| `tracePath` | Simulates full ray traversal through the grid |
| `absorbRay` | Checks if ray is absorbed by a direct atom hit |
| `touchTwo` | Handles double deflection (reverse direction) |
| `toEdgePos` | Maps out-of-bounds position back to edge |
| `genEdgePos` | Generates all edge positions for grid size N |

### Challenge 2 — `solveBB` and `solveBB'`

```haskell
solveBB  :: Int -> Interactions -> Atoms
solveBB' :: Int -> Int -> Interactions -> [[Atoms]]
```

`solveBB` deduces atom positions from a complete interactions list by extracting candidate coordinates from absorbed rays and validating combinations against Challenge 1 output.

`solveBB'` extends this with brute-force search for incomplete interaction lists — generates all possible atom combinations across the full grid and returns any configuration whose interactions are a superset of the provided input.

---

## Example Output

```
ghci> calcBBInteractions 8 [(2,3), (4,6), (7,3), (7,8)]
[((North,1),Path (West,2)),((North,2),Absorb), ...]

ghci> solveBB 4 testInteractions
[(2,3),(4,6),(7,3),(7,8)]

ghci> solveBB' 2 3 [((North, 2), Absorb)]
[[(1,2),(2,2)],[[(1,3),(2,3)]], ...]
```

---

## Getting Started

**Prerequisites:** GHCup installed with GHC and GHCi

**If GHCup is installed in the C drive:**
```
cd \
cd ghcup
cd bin
ghci
```

**Load and run:**
```haskell
:load Black_Box.hs
calcBBInteractions 8 [(2,3), (4,6), (7,3), (7,8)]
solveBB 4 testInteractions
```

A `testInteractions` value is pre-defined in the source file for quick testing. Modify it directly to test different configurations.

---

## Output Screenshots

<img width="1088" height="176" alt="image" src="https://github.com/user-attachments/assets/ba1e95cd-89d4-4c47-9c64-49e88a1afa44" />

<img width="1639" height="110" alt="image" src="https://github.com/user-attachments/assets/7c4bb3fc-4c13-46b6-b0d0-0d1539cd2f61" />

---

## Notes

Individual coursework for COMP2209 Programming III at the University of Southampton Malaysia. Demonstrates functional programming concepts including recursion, higher-order functions, list comprehensions, and the `Maybe` monad for safe atom edge detection.
