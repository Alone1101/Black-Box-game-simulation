import Data.List (find, subsequences)
import Data.Maybe (isJust)

--2D Coordinates
type Pos = (Int, Int)

--Entry or exit points
data Side = North | East | South | West deriving (Show, Eq, Ord)
type EdgePos = (Side, Int)

--Location of atoms
type Atoms = [Pos]

--Outcomes of firing rays from stated entry position
type Interactions = [(EdgePos, Marking)]
data Marking = Absorb | Reflect | Path EdgePos deriving (Show, Eq)

--Challenge 1: Calculate All Interactions

--Convert edge postion to grid coordinate
toCoords :: EdgePos -> Int -> Pos
toCoords (side, i) n = case side of
 North -> (i, 1)
 South -> (i, n)
 East -> (n, i)
 West -> (1, i)

--Move to next grid position
move :: Pos -> (Int, Int) -> Pos
move (x, y) (dx, dy) = (x + dx, y + dy)

--Check if position is within bounds
inBounds :: Pos -> Int -> Bool
inBounds (x, y) n = x >= 1 && x <= n && y >= 1 && y <= n

--Check for edge reflection
edgeReflection :: EdgePos -> Int -> Atoms -> Bool
edgeReflection (side, i) n atoms = 
  let (x, y) = toCoords (side, i) n
  in case side of
       North -> any (`elem` atoms) [(x-1, y), (x+1, y)]
       South -> any (`elem` atoms) [(x-1, y), (x+1, y)]
       East  -> any (`elem` atoms) [(x, y-1), (x, y+1)]
       West  -> any (`elem` atoms) [(x, y-1), (x, y+1)]

--Check for ray deflection and reflection
deflectRay :: EdgePos -> Int -> Atoms -> Marking
deflectRay edgePos n atoms = do
    let start = toCoords edgePos n
    if edgeReflection edgePos n atoms
        then Reflect
        else tracePath start (initialDirection edgePos) n atoms edgePos

--Determine direction based on edge position
initialDirection :: EdgePos -> (Int, Int)
initialDirection (side, _) = case side of
  North -> (0, 1)
  South -> (0, -1)
  East  -> (-1, 0)
  West  -> (1, 0)

--Map exit coordinates to edge position
toEdgePos :: Pos -> Int -> EdgePos
toEdgePos (x, y) n
  | x == 0 = (West, y)
  | x == n+1 = (East, y)
  | y == 0 = (North, x)
  | y == n+1 = (South, x)
  | otherwise = error "Invalid edge position"

--Check if touching edges of two atoms
touchTwo :: Pos -> (Int, Int) -> Atoms -> Bool
touchTwo (x, y) (dx, dy) atoms =
  let edges = if dx == 0
                then [(x - 1, y + dy), (x + 1, y + dy)]
                else [(x + dx, y - 1), (x + dx, y + 1)]
    in length (filter (`elem` atoms) edges) == 2

--Handle ray deflection
deflect :: Pos -> (Int, Int) -> Atoms -> (Int, Int)
deflect (x, y) d@(dx, dy) atoms
  | touchTwo (x, y) d atoms = (-dx, -dy) --Reverse direction
  | otherwise = case findAtomEdge (x, y) atoms of
        Just (ax, ay) -> case d of

          (0, 1)  -> if x == ax - 1 && y == ay - 1 then (-1, 0) -- North to West
                     else if x == ax + 1 && y == ay - 1 then (1, 0) -- North to East
                     else d

          (0, -1) -> if x == ax - 1 && y == ay + 1 then (-1, 0) -- South to West
                     else if x == ax + 1 && y == ay + 1 then (1, 0) -- South to East
                     else d

          (1, 0)  -> if x == ax - 1 && y == ay + 1 then (0, 1) -- East to South
                     else if x == ax - 1 && y == ay - 1 then (0, -1) -- East to North
                     else d

          (-1, 0) -> if x == ax + 1 && y == ay + 1 then (0, 1) -- West to South
                     else if x == ax + 1 && y == ay - 1 then (0, -1) -- West to North
                     else d

        Nothing -> d

--Find atom edges
findAtomEdge :: Pos -> Atoms -> Maybe Pos
findAtomEdge (x, y) atoms = find (\(ax, ay) -> abs (ax - x) == 1 && abs (ay - y) == 1) atoms

--Trace ray path
tracePath :: Pos -> (Int, Int) -> Int -> Atoms -> EdgePos -> Marking
tracePath pos d n atoms startEdge
  | not (inBounds pos n) =
      let exitEdge = toEdgePos pos n
      in if exitEdge == startEdge then Reflect else Path exitEdge
  | isJust (findAtomEdge pos atoms) = tracePath (move pos d') d' n atoms startEdge
  | otherwise = tracePath (move pos d) d n atoms startEdge
 where d' = deflect pos d atoms

--Handle ray absorption
absorbRay :: EdgePos -> Atoms -> Int -> Marking
absorbRay edge atoms n =
 let (x, y) = toCoords edge n
 in case edge of
    (North, _) -> if any (\(ax, ay) -> ax == x && ay > y) atoms then Absorb else Path (North, x)
    (South, _) -> if any (\(ax, ay) -> ax == x && ay < y) atoms then Absorb else Path (South, x)
    (East, _) -> if any (\(ax, ay) -> ay == y && ax < x) atoms then Absorb else Path (East, y)
    (West, _) -> if any (\(ax, ay) -> ay == y && ax > x) atoms then Absorb else Path (West, y)

--Calculate all interactions
calcBBInteractions :: Int -> Atoms -> Interactions
calcBBInteractions n atoms = 
 let edgePos = genEdgePos n
 in map (\e -> (e, mark e atoms n)) edgePos
 
--Get markings for different edge positions
mark :: EdgePos -> Atoms -> Int -> Marking
mark edgePos atoms n = 
    case deflectRay edgePos n atoms of
        Reflect -> Reflect
        _       -> case absorbRay edgePos atoms n of  -- Check absorption
                      Absorb -> Absorb
                      _     -> deflectRay edgePos n atoms  -- Check deflection again

--Generate edge positions
genEdgePos :: Int -> [EdgePos]
genEdgePos n = concatMap (\side -> [(side, i) | i <- [1..n]]) [North, East, South, West]

--Challenge 2: Solve a Black Box

solveBB :: Int -> Interactions -> Atoms
solveBB n interactions =
  let
    --Get all possible xs and ys
    (xValues, yValues) = getXsandYs interactions

    --Get all combinations with defined length
    allCombinations = filter (\combo -> length combo == n) $ combinations n (generateCombinations xValues yValues)

    --Check each combination with functions in Challenge 1
    validateCombination atoms = calcBBInteractions (gridSize testInteractions) atoms == interactions

    --Filter out all valid combinations
    validCombinations = filter validateCombination allCombinations

  in
    --Return all valid combinations, or [] if no working combination exist
    if null validCombinations then [] else concat validCombinations

--Helper function to get grid size from interactions
gridSize :: Interactions -> Int
gridSize = maximum . map snd . concatMap (\((_, n), _) -> [(n, n)])

--Get xs and ys from absorption in interactions
getXsandYs :: Interactions -> ([Int], [Int])
getXsandYs interactions =
    let
        xValues = [n | ((North, n), Absorb) <- interactions]  
        yValues = [n | ((East, n), Absorb) <- interactions]
    in
        (xValues, yValues)

--Get all possible (x, y) combinations
generateCombinations :: [Int] -> [Int] -> [(Int, Int)]
generateCombinations xValues yValues = [(x, y) | x <- xValues, y <- yValues]

--Generate combinations of given size
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations _ [] = []
combinations k (x:xs) = [x:rest | rest <- combinations (k-1) xs] ++ combinations k xs

--SolveBB' using brute force to handle incomplete interactions list as input

--Get all possible atom positions from given grid size
allPositions :: Int -> [Pos]
allPositions n = [(x, y) | x <- [1..n], y <- [1..n]]

--Generate all combinations
atomCombinations :: Int -> Int -> [[Pos]]
atomCombinations n gridSize = subsequences (allPositions gridSize) >>= \positions ->
  if length positions == n then return positions else []

--Check if the given interactions are a subset of the calculated interactions
isSubsetOfInteractions :: Interactions -> Interactions -> Bool
isSubsetOfInteractions subset superset = all (\x -> elem x superset) subset

solveBB' :: Int -> Int -> Interactions -> [[Atoms]]
solveBB' n gridSize interactions = do
  --Generate all atom combinations
  atomPositions <- atomCombinations n gridSize
  
  --Calculate all the interactions from all atom combinations reutilizing code in Challenge 1
  let calculatedInteractions = calcBBInteractions gridSize atomPositions
  
  --If Interactions given is a subset of result of an atom combination then return it, if none, then return []
  if isSubsetOfInteractions interactions calculatedInteractions
    then return [atomPositions]
    else []

--Example interactions for testing, atoms = [(2,3), (4,6), (7,3), (7,8)] (Change for testing)
testInteractions :: Interactions
testInteractions =
  [ ((North, 1), Path (West, 2)), ((North, 2), Absorb), ((North, 3), Path (North, 6)), ((North, 4), Absorb),
    ((North, 5), Path (East, 5)), ((North, 6), Path (North, 3)), ((North, 7), Absorb), ((North, 8), Path (East, 2)),
    ((East, 1), Path (West, 1)), ((East, 2), Path (North, 8)), ((East, 3), Absorb), ((East, 4), Path (East, 7)),
    ((East, 5), Path (North, 5)), ((East, 6), Absorb), ((East, 7), Path (East, 4)), ((East, 8), Absorb),
    ((South, 1), Path (West, 4)), ((South, 2), Absorb), ((South, 3), Path (West, 7)), ((South, 4), Absorb),
    ((South, 5), Path (West, 5)), ((South, 6), Reflect), ((South, 7), Absorb), ((South, 8), Reflect),
    ((West, 1), Path (East, 1)), ((West, 2), Path (North, 1)), ((West, 3), Absorb), ((West, 4), Path (South, 1)),
    ((West, 5), Path (South, 5)), ((West, 6), Absorb), ((West, 7), Path (South, 3)), ((West, 8), Absorb) ]