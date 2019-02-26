import Data.Maybe (catMaybes)
import Data.List (find)

data Face = Top | Bot | Lef | Ri | Front | Back deriving Show
data Dir = North | South | East | West deriving Show

data GearState = GearState{ face :: Face, dir :: Dir, tooth :: Int } deriving Show

turn :: GearState -> Maybe GearState
turn (GearState face North tooth) = Just (GearState face East tooth)
turn (GearState face East tooth) = Just (GearState face North tooth)

turn (GearState face South tooth) = Just (GearState face West tooth)
turn (GearState face West tooth) = Just (GearState face South tooth)

forward :: GearState -> Maybe GearState
forward (GearState Top North tooth) = Nothing
forward (GearState Top South tooth) = Nothing
forward (GearState Top East tooth) = Just (GearState Ri South (incr tooth))
forward (GearState Top West tooth) = Just (GearState Lef South (incr tooth))

forward (GearState Bot North tooth) = Just (GearState Front West (incr tooth))
forward (GearState Bot South tooth) = Just (GearState Back East (incr tooth))
forward (GearState Bot East tooth) = Nothing
forward (GearState Bot West tooth) = Just (GearState Ri North (incr tooth))

forward (GearState Front North tooth) = Nothing
forward (GearState Front South tooth) = Just (GearState Bot North (incr tooth))
forward (GearState Front East tooth) = Nothing
forward (GearState Front West tooth) = Just (GearState Back West (incr tooth))

forward (GearState Back North tooth) = Nothing
forward (GearState Back South tooth) = Just (GearState Bot North (incr tooth))
forward (GearState Back East tooth) = Just (GearState Ri East (incr tooth))
forward (GearState Back West tooth) = Just (GearState Lef West (incr tooth))

forward (GearState Lef North tooth) = Just (GearState Top East (incr tooth))
forward (GearState Lef South tooth) = Nothing
forward (GearState Lef East tooth) = Just (GearState Front East (incr tooth))
forward (GearState Lef West tooth) = Just (GearState Back West (incr tooth))

forward (GearState Ri North tooth) = Just (GearState Top West (incr tooth))
forward (GearState Ri South tooth) = Just (GearState Bot East (incr tooth))
forward (GearState Ri East tooth) = Just (GearState Back East (incr tooth))
forward (GearState Ri West tooth) = Nothing

forward _ = Nothing

backward :: GearState -> Maybe GearState

backward (GearState Top South tooth) = Nothing
backward (GearState Top North tooth) = Nothing
backward (GearState Top West tooth) = Just (GearState Ri South (decr tooth))
backward (GearState Top East tooth) = Just (GearState Lef South (decr tooth))

backward (GearState Bot South tooth) = Just (GearState Back North (decr tooth))
backward (GearState Bot North tooth) = Just (GearState Front North (decr tooth))
backward (GearState Bot West tooth) = Nothing
backward (GearState Bot East tooth) = Just (GearState Ri North (decr tooth))

backward (GearState Front South tooth) = Nothing
backward (GearState Front North tooth) = Just (GearState Bot South (decr tooth))
backward (GearState Front West tooth) = Nothing
backward (GearState Front East tooth) = Just (GearState Back East (decr tooth))

backward (GearState Back South tooth) = Nothing
backward (GearState Back North tooth) = Just (GearState Bot North (decr tooth))
backward (GearState Back West tooth) = Just (GearState Ri West (decr tooth))
backward (GearState Back East tooth) = Just (GearState Lef East (decr tooth))

backward (GearState Lef South tooth) = Just (GearState Top West (decr tooth))
backward (GearState Lef North tooth) = Nothing
backward (GearState Lef West tooth) = Just (GearState Front West (decr tooth))
backward (GearState Lef East tooth) = Just (GearState Back East (decr tooth))

backward (GearState Ri South tooth) = Just (GearState Top East (decr tooth))
backward (GearState Ri North tooth) = Just (GearState Bot West (decr tooth))
backward (GearState Ri West tooth) = Just (GearState Back West (decr tooth))
backward (GearState Ri East tooth) = Nothing

backward _ = Nothing


mod5 :: Int -> Int
mod5 = (flip mod) 5

incr :: Int -> Int
incr x = mod5 (x+1)

decr :: Int -> Int
decr x = mod5 (x-1)

isWinningState :: GearState -> Bool
isWinningState (GearState Top North 0) = True
isWinningState _ = False

getNextStates :: GearState -> [GearState]
getNextStates state = catMaybes [(turn state), (forward state), (backward state)]

getNewFrontier :: [GearState] -> [GearState]
getNewFrontier frontier = (concatMap getNextStates frontier)

checkFrontier :: [GearState] -> Maybe GearState
checkFrontier frontier = find isWinningState frontier

search :: Int -> [GearState] -> Maybe GearState
search 0 _ = Nothing
search remaining frontier = 
    case (checkFrontier frontier) of 
        Nothing -> (search (remaining - 1) (getNewFrontier frontier)) 
        (Just state) -> Just state
