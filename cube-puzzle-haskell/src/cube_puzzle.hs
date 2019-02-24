data Direction = North | South | East | West
data GearState = GearState { face :: Face, tooth :: Tooth, dir :: Direction}
type Tooth = Int
data Face = Face { name :: String, connections :: [Connection] }
data Face = Top | TopR | Bot | BotR | Left | LeftR | Right | RightR | Front | FrontR | Back | BackR
data Connection = Connection { other :: Face, dir :: Direction, invert :: Bool }

getConnectedState :: GearState -> Connection -> GearState
getConnectedState gear connection{dir=Forward, invert=True} = GearState { face=(other connection), tooth=(Mod4 ((tooth gear) + 1), history=(name (face gear)):(history gear) }

getNextStates :: GearState -> [GearState]
getNextStates (GearState Top tooth dir) =

getNextPositions :: Face -> Dir -> [(Face, Dir)]
getNextPositions Top Forward = []


data Face = Top | TopR | Bot | BotR | Left | LeftR | Right | RightR | Front | FrontR | Back | BackR



data Face = Top | Bot | Left | Right | Front | Back
data GearState = { face :: Face, dir :: Dir, tooth :: Int }

turn :: Face -> Dir -> Maybe (Face, Dir)
turn face North = Just (face, East)
turn face East = Just (face, North)

turn face South = Just (face, West)
turn face West = Just (face, South)


forward :: Face -> Dir -> Maybe (Face, Dir)
forward Top North = Nothing
forward Top South = Nothing
forward Top West = Just (Left, West)
forward Top East = Just (Right, East)

backward :: Face -> Dir -> Maybe (Face, Dir)

mod5 :: Int -> Int
mod5 = (flip mod) 5

incr :: Int -> Int
incr x = mod5 (x+1)

decr :: Int -> Int
decr x = mod5 (x-1)

getNextStates :: GearState -> [GearState]
getNextStates (GearState face dir tooth) = [
    let (new_face new_dir) = (forward face dir) in (GearState new_face, new_dir, incr tooth), 
    let (new_face new_dir) = (backward face dir) in (GearState new_face, new_dir, decr tooth), 
    let (new_face new_dir) = (rotate face dir) in (GearState new_face, new_dir, tooth), 
    ]