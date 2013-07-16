module Example.Road (
  Road(..)
, Branch(..)
, Route
, searchRoute
) where

data Road
  = Road Int Int Int
  deriving (Show)

data Branch
  = BranchA
  | BranchB
  | BranchC
  deriving (Show)

type Route = [(Branch, Int)]

-- $setup
-- >>> let rMap = [(Road 50 10 30), (Road 5 90 20), (Road 40 2 25), (Road 10 8 0)]

-- | Stack the shortest route.
--
-- >>> stackRoute ([], []) (Road 50 10 30)
-- ([(BranchC,30),(BranchB,10)],[(BranchB,10)])
-- >>> stackRoute ([], []) (Road 5 90 20)
-- ([(BranchA,5)],[(BranchC,20),(BranchA,5)])
stackRoute :: (Route, Route) -> Road -> (Route, Route)
stackRoute (r1, r2) (Road a b c) =
  let
    t1 = sum (map snd r1)
    t2 = sum (map snd r2)
    r1ToA = t1 + a
    r2ToA = t2 + b + c
    r1ToB = t1 + a + c
    r2ToB = t2 + b
    n1 = if r1ToA <= r2ToA
      then (BranchA, a) : r1
      else (BranchC, c) : (BranchB, b) : r2
    n2 = if r2ToB <= r1ToB
      then (BranchB, b) : r2
      else (BranchC, c) : (BranchA, a) : r1
  in (n1, n2)

-- | Search the shortest route.
--
-- >>> rMap
-- [Road 50 10 30,Road 5 90 20,Road 40 2 25,Road 10 8 0]
-- >>> searchRoute rMap
-- [(BranchB,10),(BranchC,30),(BranchA,5),(BranchC,20),(BranchB,2),(BranchB,8),(BranchC,0)]
searchRoute :: [Road] -> Route
searchRoute rs =
  let
    (r1, r2) = foldl stackRoute ([], []) rs
  in
    reverse $ if sum (map snd r1) <= sum (map snd r2) then r1 else r2
