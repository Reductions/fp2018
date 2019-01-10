-- Задача 1
genGlm :: Int -> Int -> [Int] -> [Int]
genGlm currentNumber 0 (head : tail) =
  genGlm (currentNumber + 1) head tail
genGlm currentNumber left glm =
  currentNumber : genGlm currentNumber (left - 1) glm

golomb :: [Int]
golomb =
  1 : 2 : genGlm 2 1 (drop 2 golomb)


-- Задача 2

data NonEmptyTree = Node Int [NonEmptyTree] deriving(Show)
data Tree = Empty | NonEmpty NonEmptyTree deriving(Show)
type Family = [Int]

-- a

similarFamilies :: Tree -> Tree -> Bool
similarFamilies Empty Empty = True
similarFamilies Empty _ = False
similarFamilies _ Empty = False
similarFamilies (NonEmpty tree1) (NonEmpty tree2) =
  isSimilar family1 family2
  where
    family1 = collectFamily tree1
    family2 = collectFamily tree2

collectFamily :: NonEmptyTree -> Family
collectFamily (Node root subtrees) =
  root : map (\(Node subroot _) -> subroot) subtrees

isSimilar :: Family -> Family -> Bool
isSimilar f1 f2 =
  isSubset f1 f2 && isSubset f2 f1

isSubset [] _ = True
isSubset (head : tail) set =
  elem head set && isSubset tail set

-- b

findAllFamiliesInTree :: NonEmptyTree -> [Family]
findAllFamiliesInTree tree@(Node _ subtrees) =
  collectFamily tree : concat (map findAllFamiliesInTree subtrees)

chechEachDistantFamily :: [Family] -> NonEmptyTree -> Bool
chechEachDistantFamily [] _ = False
chechEachDistantFamily _ _ = True


similarButDistant :: Tree -> Bool
similarButDistant Empty = False
similarButDistant (NonEmpty tree) = similarButDistantH tree

similarButDistantH :: NonEmptyTree -> Bool
similarButDistantH tree =
  chechEachDistantFamily allFamilies tree
  where allFamilies = findAllFamiliesInTree tree


-- Задача 3

--              тактове размер темпо
type Segment = (Int,    Int,   Int)
type Song = [Segment]

segDuration :: Segment -> Float
segDuration (tacts, size, bpm) =
  (fromIntegral (tacts * size)) / (fromIntegral bpm)

-- a
duration :: Song -> Float
duration =
  sum . map segDuration

-- b
slowestSegs :: Song -> Song
slowestSegs song =
  filter (\(_, _, bpm) -> bpm == slowestBpm) song
  where
    slowestBpm = minimum $ map (\(_,_,bpm) -> bpm) song

longestSeg :: Song -> Maybe Segment
longestSeg [] = Nothing
longestSeg song =
  Just $ head $ filter (\seg -> segDuration seg == longestDuration) song
  where
    longestDuration = maximum $ map segDuration song

longestSlowest :: Song -> Maybe Segment
longestSlowest =
  longestSeg . slowestSegs

-- v

findMidleHelper :: Float -> Song -> Segment
findMidleHelper timeLeftToMid (head : tail)
  | newLeft <= 0 = head
  | True         = findMidleHelper newLeft tail
  where
    newLeft = timeLeftToMid - segDuration head

findMiddle :: Song -> Maybe Segment
findMiddle [] = Nothing
findMiddle song =
  Just $ findMidleHelper ((duration song) / 2) song
