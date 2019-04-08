module Balls ( run )
where

import qualified Graphics.Gloss as G
import Graphics.Gloss.Data.ViewPort

import qualified Utils.Geometry as UG
import qualified Physics.Kinematics as PK

data BallState = BallState {
      radius :: Float
    , acc :: (Float, Float)
    , vel :: (Float, Float)
    , position :: UG.Point
    , prevPositions :: [UG.Point]
    , velCoeff :: Float
    , mass :: Float
}

ax :: BallState -> Float
ax state = (fst . acc) state
ay state = (snd . acc) state
vx state = (fst . vel) state
vy state = (snd . vel) state
x state = (fst . position) state
y state = (snd . position) state

data GameState = GameState {
      ballStates :: [BallState]
    , rigidLines :: [UG.Line]
}

infinity = 9999999999999

width = 500
height = 500
fps = 30 :: Int

widthHeight :: (Int, Int)
widthHeight = (width, height)

gravity :: Float
gravity = 8.0 / (fromIntegral fps :: Float)

ballState :: BallState
ballState = BallState {
      radius = 10
    , acc = (0, -gravity)
    , vel = (0, 0)
    , position = (200, 100)
    , prevPositions = []
    , velCoeff = 0.7
    , mass = 1
}

ballstates = map (\x -> ballState {position = (x, x) }) (take 40 [-500, -475..])
sceneLines = [((300, 300), (400, 400))]

initialState :: GameState
initialState = GameState {
      ballStates = ballstates
    , rigidLines = sceneLines
}

threshold =  1.8 * gravity

thresholdedVel :: Float -> Float
thresholdedVel v = if v < threshold then 0 else v

resultantVelocity :: UG.Point -> GameState -> BallState -> UG.Point
resultantVelocity newPos@(newX, newY) gState bState = foldl velIfCollide (vel bState) filteredBalls
    where filteredBalls = filter (\x -> position x /= newPos) (ballStates gState)
          velIfCollide velocity b2 | UG.circlesCollide (newX, newY, radius bState) (x b2, y b2, radius b2) = (0, 0)
                             | otherwise = fst $ PK.collide (mass bState, vel bState) (mass b2, vel b2)

nextBallState :: Float -> GameState -> BallState -> BallState
nextBallState sec gState currState = currState {
          position = newPos
        , prevPositions = position currState : prevPositions currState
        , vel = newVel
    }
    where (x, y) = position currState
          newPos = position currState `UG.add` vel currState
          newVel = acc currState `UG.add` resultantVelocity newPos gState currState

nextState :: Float -> GameState -> GameState
nextState sec currState = currState {
    ballStates = map (nextBallState sec currState) $ ballStates currState
}

renderLine :: UG.Line -> G.Picture
renderLine (start, end) = G.Color G.blue $ G.Line [start, end]

renderState :: GameState -> G.Picture
renderState currState = G.Pictures (map renderLine (rigidLines currState) ++ balls)
    where ballstates = ballStates currState
          renderBall (BallState r acc vel (x, y) pp vc m) = G.Color G.red $ G.translate x y $ G.ThickCircle 0 (2*r)
          balls = map renderBall ballstates

updateGame :: ViewPort -> Float -> GameState -> GameState
updateGame v = nextState

run :: IO ()
-- run = G.display (G.InWindow "Random Circles" widthHeight (20,20)) G.black $ renderState initialState
run = G.simulate
    (G.InWindow "Random Circles" widthHeight (20,20))
    G.black
    fps
    initialState
    renderState
    updateGame
