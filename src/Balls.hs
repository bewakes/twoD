module Balls ( run )
where

import qualified Graphics.Gloss as G
import Graphics.Gloss.Data.ViewPort

import qualified Utils.Geometry as UG

data BallState = BallState {
      ballRadius :: Float
    , ax :: Float -- x acceleration
    , ay :: Float -- y acceleration
    , vx :: Float -- x velocity
    , vy :: Float -- y velocity
    , currentPosition :: UG.Point
    , prevPositions :: [UG.Point]
    , velCoeff :: Float
}

data GameState = GameState {
      ballStates :: [BallState]
    , groundHeight :: Float
}

width = 500
height = 500
fps = 30 :: Int

widthHeight :: (Int, Int)
widthHeight = (width, height)

gravity :: Float
gravity = 8.0 / (fromIntegral fps :: Float)

ballState :: BallState
ballState = BallState {
      ballRadius = 10
    , ax = 0
    , ay = -gravity
    , vx = 0
    , vy = 0
    , currentPosition = (200, 100)
    , prevPositions = []
    , velCoeff = 0.7
}

ballstates = map (\x -> ballState {currentPosition = (x, x) }) (take 40 [-500, -475..])

initialState :: GameState
initialState = GameState {
      ballStates = ballstates
    , groundHeight = -500
}

threshold = 4 * gravity

thresholdedVel :: Float -> Float
thresholdedVel v = if v < threshold then 0 else v

shouldBounce :: UG.Point -> BallState -> GameState -> Bool
shouldBounce newPos currState gameSate = ballYPos <= groundHeight gameSate + ballRadius currState
    where ballYPos = snd newPos 

nextBallState :: Float -> GameState -> BallState -> BallState
nextBallState sec gState currState = currState {
          currentPosition = if bounce then (x, gHeight + ballRadius currState) else newPos
        , prevPositions = currentPosition currState : prevPositions currState
        , vy = if bounce then thresholdedVel (- vy currState * coeff) else newVy 
    }
    where newVy = vy currState + ay currState
          (x, y) = currentPosition currState
          newPos = (x, y + vy currState)
          gHeight = groundHeight gState
          bounce = shouldBounce newPos currState gState
          coeff = velCoeff currState

nextState :: Float -> GameState -> GameState
nextState sec currState = currState {
    ballStates = map (nextBallState sec currState) $ ballStates currState
}

renderGround :: GameState -> G.Picture
renderGround currState = G.Color G.blue $ G.Line [(-w, h), (w, h)]
    where w = fromIntegral width :: Float
          h = groundHeight currState

renderState :: GameState -> G.Picture
renderState currState = G.Pictures (renderGround currState : balls)
    where ballstates = ballStates currState
          renderBall (BallState r ax ay vx vy (x, y) pp vc) = G.Color G.red $ G.translate x y $ G.ThickCircle 0 (2*r)
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
