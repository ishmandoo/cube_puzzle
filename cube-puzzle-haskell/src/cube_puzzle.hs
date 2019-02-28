import Data.Maybe (catMaybes)
import Data.List (find)

data Face = Top | Bot | Lef | Ri | Front | Back deriving (Show, Eq)
data Dir = North | South | East | West deriving (Show, Eq)

data GearState = GearState{ face :: Face, dir :: Dir, tooth :: Int, history :: [(Face, Dir)] } deriving Show

-- a list defining connections between faces
connections = 
    [ ((Top, East),(Ri, South))
    , ((Top, West),(Lef, South))

    , ((Bot, North),(Front, West))
    , ((Bot, South),(Back, East))
    , ((Bot, West),(Ri, North)) 

    , ((Front, South),(Bot, North)) 
    , ((Front, West),(Back, West)) 

    , ((Back, South),(Bot, North)) 
    , ((Back, East),(Ri, East)) 
    , ((Back, West),(Lef, West)) 

    , ((Lef, North),(Top, East)) 
    , ((Lef, East),(Front, East))
    , ((Lef, West),(Back, West)) 
    
    , ((Ri, North),(Top, West)) 
    , ((Ri, South),(Bot, East)) 
    , ((Ri, East),(Back, East))  
    ]

-- a function for reversing tuples
reverseTuple :: (a, b) -> (b, a)
reverseTuple (a,b) = (b,a)

-- mapping the reverse tuple function over the connections gives the connections for reverse gear moves
reverse_connections = map reverseTuple connections

-- a function which applies a forward move to the gear
forward :: GearState -> Maybe GearState
forward (GearState face dir tooth history) = 
    case (lookup (face, dir) connections) of -- looks for a connection
        Nothing -> Nothing -- if no forward connection exists from the current state
        (Just (new_face, new_dir)) -> Just (GearState new_face new_dir (incr tooth) ((new_face,new_dir):history)) -- returns a new state, incrementing the tooth and adding to the history

-- a function which applies a backward move to the gear, if possible
backward :: GearState -> Maybe GearState
backward (GearState face dir tooth history) = 
    case (lookup (face, dir) reverse_connections) of
        Nothing -> Nothing
        (Just (new_face, new_dir)) -> Just (GearState new_face new_dir (decr tooth) ((new_face,new_dir):history))

-- a function which applies a turn to the gear, if possible
turn :: GearState -> Maybe GearState
turn (GearState face North tooth history) = Just (GearState face East tooth ((face,North):history))
turn (GearState face East tooth history) = Just (GearState face North tooth ((face,East):history))

turn (GearState face South tooth history) = Just (GearState face West tooth ((face,South):history))
turn (GearState face West tooth history) = Just (GearState face South tooth ((face,West):history))

-- some mod 5 math for the teeth
mod5 :: Int -> Int
mod5 = (flip mod) 5

incr :: Int -> Int
incr x = mod5 (x+1)

decr :: Int -> Int
decr x = mod5 (x-1)

-- determines if the gear can leave the cube
isWinningState :: GearState -> Bool
isWinningState (GearState Top North 0 _) = True
isWinningState _ = False

-- creates a list of all the states accessible from the current state
getNextStates :: GearState -> [GearState]
getNextStates state = catMaybes [(turn state), (forward state), (backward state)] -- removes Nothings and strips the values out of Just

-- creates a new search frontier by replacing each state in the search frontier with all the states accessable from that state and flattening
getNewFrontier :: [GearState] -> [GearState]
getNewFrontier frontier = (concatMap getNextStates frontier)

-- searches the frontier for the winning state
checkFrontier :: [GearState] -> Maybe GearState
checkFrontier frontier = find isWinningState frontier

-- recursively searches for a winning state and expands the search frontier, subject to a maximum search depth
search :: Int -> [GearState] -> Maybe GearState
search 0 _ = Nothing
search depth frontier = 
    case (checkFrontier frontier) of 
        Nothing -> (search (depth - 1) (getNewFrontier frontier)) 
        (Just state) -> Just state
