import Data.Maybe (catMaybes)
import Data.List (find)

data Face = Top | Bot | Lef | Ri | Front | Back deriving (Show, Eq)
data Dir = North | South | East | West deriving (Show, Eq)

data GearState = GearState{ face :: Face, dir :: Dir, tooth :: Int, history :: [(Face, Dir)] } deriving Show

turn :: GearState -> Maybe GearState
turn (GearState face North tooth history) = Just (GearState face East tooth ((face,North):history))
turn (GearState face East tooth history) = Just (GearState face North tooth ((face,East):history))

turn (GearState face South tooth history) = Just (GearState face West tooth ((face,South):history))
turn (GearState face West tooth history) = Just (GearState face South tooth ((face,West):history))

connections = 
    [ ((Top, East),(Ri, South))
    , ((Top, West),(Lef, South))

    , ((Bot, North),(Back, North))
    , ((Bot, South),(Front, North))
    , ((Bot, West),(Ri, North)) 

    , ((Front, South),(Bot, North)) 
    , ((Front, West),(Lef, West)) 

    , ((Back, South),(Bot, South)) 
    , ((Back, East),(Lef, East)) 
    , ((Back, West),(Ri, West)) 

    , ((Lef, North),(Top, East)) 
    , ((Lef, East),(Front, East))
    , ((Lef, West),(Back, West)) 
    
    , ((Ri, North),(Top, West)) 
    , ((Ri, South),(Bot, East)) 
    , ((Ri, East),(Back, East))  
    ]

reverseTuple :: (a, b) -> (b, a)
reverseTuple (a,b) = (b,a)

reverse_connections = map reverseTuple connections

forward :: GearState -> Maybe GearState
forward (GearState face dir tooth history) = 
    case (lookup (face, dir) connections) of
        Nothing -> Nothing
        (Just (new_face, new_dir)) -> Just (GearState new_face new_dir (incr tooth) ((new_face,new_dir):history))

backward :: GearState -> Maybe GearState
backward (GearState face dir tooth history) = 
    case (lookup (face, dir) reverse_connections) of
        Nothing -> Nothing
        (Just (new_face, new_dir)) -> Just (GearState new_face new_dir (decr tooth) ((new_face,new_dir):history))

{- 
forward :: GearState -> Maybe GearState
forward (GearState Top East tooth history) = Just (GearState Ri South (incr tooth) ((Top,East):history))
forward (GearState Top West tooth history) = Just (GearState Lef South (incr tooth) ((Top,West):history))

forward (GearState Bot North tooth history) = Just (GearState Back North (incr tooth) ((Bot,North):history))
forward (GearState Bot South tooth history) = Just (GearState Front North (incr tooth) ((Bot,South):history))
forward (GearState Bot West tooth history) = Just (GearState Ri North (incr tooth) ((Bot,West):history))

forward (GearState Front South tooth history) = Just (GearState Bot North (incr tooth) ((Front,South):history))
forward (GearState Front West tooth history) = Just (GearState Lef West (incr tooth) ((Front,West):history))

forward (GearState Back South tooth history) = Just (GearState Bot South (incr tooth) ((Back,South):history))
forward (GearState Back East tooth history) = Just (GearState Lef East (incr tooth) ((Back,East):history))
forward (GearState Back West tooth history) = Just (GearState Ri West (incr tooth) ((Back,West):history))

forward (GearState Lef North tooth history) = Just (GearState Top East (incr tooth) ((Lef,North):history))
forward (GearState Lef East tooth history) = Just (GearState Front East (incr tooth) ((Lef,East):history))
forward (GearState Lef West tooth history) = Just (GearState Back West (incr tooth) ((Lef,West):history))

forward (GearState Ri North tooth history) = Just (GearState Top West (incr tooth) ((Ri,North):history))
forward (GearState Ri South tooth history) = Just (GearState Bot East (incr tooth) ((Ri,South):history))
forward (GearState Ri East tooth history) = Just (GearState Back East (incr tooth) ((Ri,East):history))

forward _ = Nothing

backward :: GearState -> Maybe GearState

backward (GearState Top West tooth history) = Just (GearState Ri North (decr tooth) ((Top,West):history))
backward (GearState Top East tooth history) = Just (GearState Lef North (decr tooth) ((Top,East):history))

backward (GearState Bot South tooth history) = Just (GearState Back South (decr tooth) ((Bot,South):history))
backward (GearState Bot North tooth history) = Just (GearState Front South (decr tooth) ((Bot,North):history))
backward (GearState Bot East tooth history) = Just (GearState Ri South (decr tooth) ((Bot,East):history))

backward (GearState Front North tooth history) = Just (GearState Bot South (decr tooth) ((Front,North):history))
backward (GearState Front East tooth history) = Just (GearState Lef East (decr tooth) ((Front,East):history))

backward (GearState Back North tooth history) = Just (GearState Bot North (decr tooth) ((Back,North):history))
backward (GearState Back West tooth history) = Just (GearState Lef West (decr tooth) ((Back,West):history))
backward (GearState Back East tooth history) = Just (GearState Ri East (decr tooth) ((Back,East):history))

backward (GearState Lef South tooth history) = Just (GearState Top West (decr tooth) ((Lef,South):history))
backward (GearState Lef West tooth history) = Just (GearState Front West (decr tooth) ((Lef,West):history))
backward (GearState Lef East tooth history) = Just (GearState Back East (decr tooth) ((Lef,East):history))

backward (GearState Ri South tooth history) = Just (GearState Top East (decr tooth) ((Ri,South):history))
backward (GearState Ri North tooth history) = Just (GearState Bot West (decr tooth) ((Ri,North):history))
backward (GearState Ri West tooth history) = Just (GearState Back West (decr tooth) ((Ri,West):history))

backward _ = Nothing -}


mod5 :: Int -> Int
mod5 = (flip mod) 5

incr :: Int -> Int
incr x = mod5 (x+1)

decr :: Int -> Int
decr x = mod5 (x-1)

isWinningState :: GearState -> Bool
isWinningState (GearState Top North 0 _) = True
isWinningState _ = False

getNextStates :: GearState -> [GearState]
getNextStates state = catMaybes [(turn state), (forward state), (backward state)]

getNewFrontier :: [GearState] -> [GearState]
getNewFrontier frontier = (concatMap getNextStates frontier)

checkFrontier :: [GearState] -> Maybe GearState
checkFrontier frontier = find isWinningState frontier

search :: Int -> [GearState] -> Maybe GearState
search 0 _ = Nothing
search depth frontier = 
    case (checkFrontier frontier) of 
        Nothing -> (search (depth - 1) (getNewFrontier frontier)) 
        (Just state) -> Just state
