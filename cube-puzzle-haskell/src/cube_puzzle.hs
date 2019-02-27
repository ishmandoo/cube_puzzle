import Data.Maybe (catMaybes)
import Data.List (find)

data Face = Top | Bot | Lef | Ri | Front | Back deriving (Show, Eq)
data Dir = North | South | East | West deriving (Show, Eq)

data GearState = GearState{ face :: Face, dir :: Dir, tooth :: Int, history :: [(Face, Dir)] } deriving Show

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


turn :: GearState -> Maybe GearState
turn (GearState face North tooth history) = Just (GearState face East tooth ((face,North):history))
turn (GearState face East tooth history) = Just (GearState face North tooth ((face,East):history))

turn (GearState face South tooth history) = Just (GearState face West tooth ((face,South):history))
turn (GearState face West tooth history) = Just (GearState face South tooth ((face,West):history))

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
