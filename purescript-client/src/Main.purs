module Main where

import Prelude

import CS150241Project.GameEngine (startNetworkGame)
import CS150241Project.Graphics (clearCanvas, drawImageScaled, drawRect, drawRectOutline, drawText)
import CS150241Project.Networking (Message, PlayerId(..))
import Data.Array (all, deleteAt, elem, filter, findIndex, length, slice, updateAt, zipWith, (!!), (..))
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber, floor)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Graphics.Canvas as Canvas
import Simple.JSON as JSON

cols :: Int
cols = 10

rows :: Int
rows = 10

-- Multiple actions can be done per turn
actionsPerTurn :: Int
actionsPerTurn = 3

capturedPieceGap :: Number
capturedPieceGap = 10.0

minBoardLength :: Number
minBoardLength = 600.00

maxBoardLength :: Number
maxBoardLength = 1000.00

capturedPanelFontSize :: Int
capturedPanelFontSize = 14

capturedPanelTextboxOffset :: Number
capturedPanelTextboxOffset = 20.0

capturedPanelButtonOffset :: Number
capturedPanelButtonOffset = 50.0

tileLength :: Number
tileLength = Number.floor $ min (minBoardLength / (toNumber $ min cols rows)) (maxBoardLength / (toNumber $ max cols rows))

capturedPanelWidth :: Number
capturedPanelWidth = max (tileLength + 20.0) 150.0

maxCapturedPerPage :: Int
maxCapturedPerPage = floor ((capturedPanelHeight - capturedPanelButtonOffset) / (tileLength + capturedPieceGap))

boardWidth :: Number
boardWidth = Number.floor $ tileLength * (toNumber cols)

boardHeight :: Number
boardHeight = Number.floor $ tileLength * (toNumber rows)

capturedPanelHeight :: Number
capturedPanelHeight = boardHeight

width :: Number
width = boardWidth + capturedPanelWidth

height :: Number
height = max boardHeight capturedPanelHeight

fps :: Int
fps = 60

-- Helper Funtions

-- This can't be a type-class due to the "Orphan Instance" error
-- While I could have written a newtype, I just didn't feel it was worth the mess that comes with it
isSamePlayer :: PlayerId -> PlayerId -> Boolean
isSamePlayer a b =
  case a, b of
    Player1, Player1 -> true
    Player2, Player2 -> true
    _, _ -> false

getEnemyPlayer :: PlayerId -> PlayerId
getEnemyPlayer player =
  case player of
    Player1 -> Player2
    Player2 -> Player1

mirror :: forall a. Ring a => a -> a -> a -> a
mirror bottom top x = top - (x - bottom)

-- Captured Panel UI Definitions
type CapturedPanel =
  { x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  , color :: String
  , slotGap :: Number
  , capturedPieceSlots :: Array CapturedPieceSlot
  , buttons :: Array Button
  , pageText :: Maybe Text
  , fontSize :: Int
  , textboxOffset :: Number
  , buttonOffset :: Number
  , maxCapturedPerPage :: Int
  , currentPage :: Int
  , currentPageCount :: Int
  }

type CapturedPieceSlot =
  { x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  }

type Text =
  { x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  , text :: String
  , textColor :: String
  , font :: String
  , fontSize :: Int
  }

-- Note: Button onClick handling is weird due to compiler limitations on circular/self-referential typing
data ButtonActions
  = PreviousPage
  | NextPage

type Button =
  { x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  , text :: String
  , textColor :: String
  , font :: String
  , fontSize :: Int
  , onClickAction :: ButtonActions
  }

initializeCapturedPanel :: CapturedPanel
initializeCapturedPanel =
  let
    capturedPanel :: CapturedPanel
    capturedPanel =
      { x: boardWidth
      , y: 0.0
      , width: capturedPanelWidth
      , height: capturedPanelHeight
      , color: "sandybrown"
      , slotGap: capturedPieceGap
      , capturedPieceSlots: []
      , buttons: []
      , pageText: Nothing
      , fontSize: capturedPanelFontSize
      , textboxOffset: capturedPanelTextboxOffset
      , buttonOffset: capturedPanelButtonOffset
      , maxCapturedPerPage: maxCapturedPerPage
      , currentPage: 0
      , currentPageCount: 1
      }

    capturedPieceSlots :: Array CapturedPieceSlot
    capturedPieceSlots =
      let
        boxWidth = tileLength
        boxHeight = (tileLength * (toNumber capturedPanel.maxCapturedPerPage))
          + (capturedPanel.slotGap * (toNumber $ capturedPanel.maxCapturedPerPage - 1))
        boxX = boardWidth + (capturedPanel.width / 2.0) - (boxWidth / 2.0)
        boxY = ((capturedPanel.height - capturedPanel.buttonOffset) / 2.0) - (boxHeight / 2.0)
      in
        map
          ( \index ->
              { x: boxX
              , y: boxY + ((tileLength + capturedPanel.slotGap) * (toNumber index))
              , width: tileLength
              , height: tileLength
              }
          )
          (0 .. (capturedPanel.maxCapturedPerPage - 1))

    capturedPanelPageText :: Maybe Text
    capturedPanelPageText =
      let
        textWidth = capturedPanel.width
        fontSize = capturedPanel.fontSize
        textHeight = toNumber fontSize
      in
        Just
          { x: capturedPanel.x
          , y: capturedPanel.height - capturedPanel.textboxOffset
          , width: textWidth
          , height: textHeight
          , text: "Page 1 of 1"
          , textColor: "black"
          , font: "arial"
          , fontSize
          }

    capturedPanelButtons :: Array Button
    capturedPanelButtons =
      let
        buttonWidth = capturedPanel.width / 2.0
        fontSize = capturedPanel.fontSize
        buttonHeight = 2.0 * toNumber fontSize
      in
        [ { x: capturedPanel.x
          , y: capturedPanel.height - capturedPanel.buttonOffset
          , width: buttonWidth
          , height: buttonHeight
          , text: "<<< Prev"
          , textColor: "black"
          , font: "arial"
          , fontSize
          , onClickAction: PreviousPage
          }
        , { x: capturedPanel.x + buttonWidth
          , y: capturedPanel.height - capturedPanel.buttonOffset
          , width: buttonWidth
          , height: buttonHeight
          , text: "Next >>>"
          , textColor: "black"
          , font: "arial"
          , fontSize
          , onClickAction: NextPage
          }
        ]
  in
    capturedPanel { capturedPieceSlots = capturedPieceSlots, pageText = capturedPanelPageText, buttons = capturedPanelButtons }

-- Board Definitions
data PieceKind
  = Grail
  | Lance
  | Pawn
  | FlagLeft
  | FlagRight
  | Sword
  | Bow
  | Dagger

derive instance Generic PieceKind _

instance Show PieceKind where
  show = genericShow

type PieceInfo =
  { pieceKind :: PieceKind
  , movements :: Array Location
  , isProtected :: Boolean
  }

type Piece =
  { info :: PieceInfo
  , player :: PlayerId
  , location :: Location
  }

type CapturedPiece =
  { info :: PieceInfo
  , player :: PlayerId
  }

-- Prefering this over `show` due to JSON parsing and flexibility
-- Also not in the type-class due to issues with the `fromString` function
-- Nonetheless, this still adheres to OCP as it is easily extendable
pieceKindToString :: PieceKind -> String
pieceKindToString Grail = "Grail"
pieceKindToString Lance = "Lance"
pieceKindToString Pawn = "Pawn"
pieceKindToString FlagRight = "FlagRight"
pieceKindToString FlagLeft = "FlagLeft"
pieceKindToString Sword = "Sword"
pieceKindToString Bow = "Bow"
pieceKindToString Dagger = "Dagger"

pieceKindFromString :: String -> Maybe PieceKind
pieceKindFromString "Grail" = Just Grail
pieceKindFromString "Lance" = Just Lance
pieceKindFromString "Pawn" = Just Pawn
pieceKindFromString "FlagRight" = Just FlagRight
pieceKindFromString "FlagLeft" = Just FlagLeft
pieceKindFromString "Sword" = Just Sword
pieceKindFromString "Bow" = Just Bow
pieceKindFromString "Dagger" = Just Dagger
pieceKindFromString _ = Nothing

-- The Type Class is largely unneccessary, but it was added anyways
class Pieceable k where
  getPieceInfo :: k -> PieceInfo
  createPiece :: k -> PlayerId -> Location -> Piece
  getImagePath :: k -> PlayerId -> String

instance Pieceable PieceKind where
  getPieceInfo Grail =
    let
      drs = ((-1) .. 1)
      dcs = ((-1) .. 1)

      movements =
        map (\dr -> map (\dc -> newLocation dr dc) dcs) drs
          # foldl (<>) []
          # filter (_ /= newLocation 0 0)
    in
      { pieceKind: Grail
      , movements
      , isProtected: false
      }
  getPieceInfo Lance =
    let
      movements =
        [ newLocation (-1) (-1)
        , newLocation (-1) 1
        , newLocation 1 (-1)
        , newLocation 1 1
        ]
    in
      { pieceKind: Lance
      , movements
      , isProtected: false
      }
  getPieceInfo Pawn =
    let
      movements = [ newLocation (-1) 0 ]
    in
      { pieceKind: Pawn
      , movements
      , isProtected: false
      }
  getPieceInfo FlagRight =
    let
      drs = ((-1) .. 1)
      dcs = (0 .. 1)

      movements =
        map (\dr -> map (\dc -> newLocation dr dc) dcs) drs
          # foldl (<>) []
          # filter (_ /= newLocation 0 0)
    in
      { pieceKind: FlagRight
      , movements
      , isProtected: true
      }
  getPieceInfo FlagLeft =
    let
      drs = ((-1) .. 1)
      dcs = ((-1) .. 0)

      movements =
        map (\dr -> map (\dc -> newLocation dr dc) dcs) drs
          # foldl (<>) []
          # filter (_ /= newLocation 0 0)
    in
      { pieceKind: FlagLeft
      , movements
      , isProtected: true
      }
  getPieceInfo Sword =
    let
      movements =
        [ newLocation (-1) (-1)
        , newLocation (-1) 0
        , newLocation (-1) 1
        ]
    in
      { pieceKind: Sword
      , movements
      , isProtected: false
      }
  -- Note: It is intended that Bow and Dagger jump pieces
  getPieceInfo Bow =
    let
      movements =
        [ newLocation (-1) (-1)
        , newLocation (-1) 0
        , newLocation (-2) 0
        , newLocation (-1) 1
        , newLocation 1 0
        ]
    in
      { pieceKind: Bow
      , movements
      , isProtected: false
      }
  getPieceInfo Dagger =
    let
      movements =
        [ newLocation (-1) 0
        , newLocation 0 (-1)
        , newLocation 0 1
        , newLocation 1 0
        , newLocation 2 0
        ]
    in
      { pieceKind: Dagger
      , movements
      , isProtected: false
      }

  createPiece pk player location =
    { info: getPieceInfo pk
    , player
    , location
    }

  -- This additionally allows separate images for different players
  getImagePath Bow Player1 = "assets/white/bow.png"
  getImagePath Bow Player2 = "assets/black/bow.png"
  getImagePath Dagger Player1 = "assets/white/dagger.png"
  getImagePath Dagger Player2 = "assets/black/dagger.png"
  getImagePath Grail Player1 = "assets/white/grail.png"
  getImagePath Grail Player2 = "assets/black/grail.png"
  getImagePath Lance Player1 = "assets/white/lance.png"
  getImagePath Lance Player2 = "assets/black/lance.png"
  getImagePath Pawn Player1 = "assets/white/pawn.png"
  getImagePath Pawn Player2 = "assets/black/pawn.png"
  getImagePath FlagLeft Player1 = "assets/white/flag-left.png"
  getImagePath FlagLeft Player2 = "assets/black/flag-left.png"
  getImagePath FlagRight Player1 = "assets/white/flag-right.png"
  getImagePath FlagRight Player2 = "assets/black/flag-right.png"
  getImagePath Sword Player1 = "assets/white/sword.png"
  getImagePath Sword Player2 = "assets/black/sword.png"

--Temporary assets both below and above. I moved imagePaths here to easily double check what images we pass into startNetworkGame
imagePaths :: Array String
imagePaths =
  [ "assets/white/bow.png"
  , "assets/black/bow.png"
  , "assets/white/dagger.png"
  , "assets/black/dagger.png"
  , "assets/white/grail.png"
  , "assets/black/grail.png"
  , "assets/white/lance.png"
  , "assets/black/lance.png"
  , "assets/white/pawn.png"
  , "assets/black/pawn.png"
  , "assets/white/flag-left.png"
  , "assets/black/flag-left.png"
  , "assets/white/flag-right.png"
  , "assets/black/flag-right.png"
  , "assets/white/sword.png"
  , "assets/black/sword.png"
  ]

type Location =
  { row :: Int
  , col :: Int
  }

newLocation :: Int -> Int -> Location
newLocation row col =
  { row
  , col
  }

-- Mirrors absolute board locations relative to board center
mirrorLocation :: Location -> Location
mirrorLocation location =
  { row: mirror 0 (rows - 1) location.row
  , col: mirror 0 (cols - 1) location.col
  }

mirrorLocationVertically :: Location -> Location
mirrorLocationVertically location =
  { row: mirror 0 (rows - 1) location.row
  , col: location.col
  }

translateLocation :: Int -> Int -> Location -> Location
translateLocation deltaX deltaY location =
  { row: location.row + deltaX
  , col: location.col + deltaY
  }

-- Mirrors deltas relative to y = 0
mirrorDeltaVertically :: Location -> Location
mirrorDeltaVertically location =
  { row: mirror (1 - rows) (rows - 1) location.row
  , col: location.col
  }

-- Mirrors images relative to x = 0
drawHorizontallyMirroredImage
  :: Canvas.Context2D
  -> Canvas.CanvasImageSource
  -> { x :: Number
     , y :: Number
     , imgWidth :: Number
     , imgHeight :: Number
     }
  -> Effect Unit

drawHorizontallyMirroredImage ctx img { x, y, imgWidth, imgHeight } = do
  Canvas.save ctx
  Canvas.translate ctx { translateX: x + imgWidth, translateY: y }
  Canvas.scale ctx { scaleX: (-1.0), scaleY: 1.0 }
  drawImageScaled ctx img { x: 0.0, y: 0.0, width: imgWidth, height: imgHeight }
  Canvas.restore ctx

posToLocation :: Int -> Int -> Location
posToLocation y x =
  let
    row = (y - 10) / (floor tileLength)
    col = (x - 10) / (floor tileLength)
  in
    { row, col }

locationInBounds :: Location -> Boolean
locationInBounds location =
  elem location.row (0 .. (rows - 1)) && elem location.col (0 .. (cols - 1))

getPieceAtLocation :: Array Piece -> Location -> Maybe Piece
getPieceAtLocation pieces location =
  loop 0
  where
  loop :: Int -> Maybe Piece
  loop i =
    case pieces !! i of
      Just piece ->
        if piece.location == location then
          Just piece
        else
          loop (i + 1)
      Nothing -> Nothing

getPieceIndex :: Array Piece -> Piece -> Maybe Int
getPieceIndex pieces targetPiece =
  loop 0
  where
  loop :: Int -> Maybe Int
  loop i =
    case pieces !! i of
      Just piece ->
        if piece.location == targetPiece.location then
          Just i
        else
          loop (i + 1)
      Nothing -> Nothing

getAllMovements :: PlayerId -> Location -> Array Location -> Array Location
getAllMovements player location deltas =
  case player of
    Player1 -> deltas <#> (add location)
    Player2 -> deltas <#> mirrorDeltaVertically <#> (add location)

-- Game Over Definitions
data GameOverState
  = Winner PlayerId
  | Draw
  | None

-- Message Formats

type MovePayload =
  { message_type :: String
  , message_content ::
      { move_src :: Location
      , move_dest :: Location
      }
  }

createMovePayload :: Location -> Location -> MovePayload
createMovePayload srcLocation destLocation =
  { message_type: "move"
  , message_content:
      { move_src: srcLocation
      , move_dest: destLocation
      }
  }

type PlacePayload =
  { message_type :: String
  , message_content ::
      { place_piece_kind :: String
      , place_dest :: Location
      }
  }

createPlacePayload :: PieceKind -> Location -> PlacePayload
createPlacePayload pieceKind destLocation =
  { message_type: "place"
  , message_content:
      { place_piece_kind: pieceKindToString pieceKind
      , place_dest: destLocation
      }
  }

type PingPayload =
  { message_type :: String
  , message_content :: {}
  }

createPingPayload :: PingPayload
createPingPayload =
  { message_type: "ping"
  , message_content: {}
  }

-- Game state and logic
type GameState =
  { tickCount :: Int
  , pieces :: Array Piece
  , capturedPieces :: Array CapturedPiece
  , player :: PlayerId
  , currentPlayer :: PlayerId
  , turn :: Int
  , action :: Int
  , activePieceIndex :: Maybe Int
  , activeCapturedPieceIndex :: Maybe Int
  , capturedPanel :: CapturedPanel
  , gameOverState :: GameOverState
  , lastReceivedMessage :: Maybe Message
  , sentPing :: Boolean
  , receivedPing :: Boolean
  , debugString :: String
  }

updateTurnActions :: GameState -> GameState
updateTurnActions state =
  if state.action + 1 <= actionsPerTurn then
    state { action = state.action + 1 }
  else
    state { turn = state.turn + 1, action = 1, currentPlayer = getEnemyPlayer state.currentPlayer }

initialState :: Effect GameState
initialState = do
  pieces <- pure $
    [
      -- Player 1
      createPiece Pawn Player1 (newLocation 7 0)
    , createPiece Pawn Player1 (newLocation 7 1)
    , createPiece Pawn Player1 (newLocation 7 2)
    , createPiece Pawn Player1 (newLocation 7 3)
    , createPiece Pawn Player1 (newLocation 7 4)
    , createPiece Pawn Player1 (newLocation 7 5)
    , createPiece Pawn Player1 (newLocation 7 6)
    , createPiece Pawn Player1 (newLocation 7 7)
    , createPiece Pawn Player1 (newLocation 7 8)
    , createPiece Pawn Player1 (newLocation 7 9)
    , createPiece Lance Player1 (newLocation 8 4)
    , createPiece Lance Player1 (newLocation 8 5)
    , createPiece Dagger Player1 (newLocation 8 3)
    , createPiece Dagger Player1 (newLocation 8 6)
    , createPiece Sword Player1 (newLocation 8 2)
    , createPiece Sword Player1 (newLocation 8 7)
    , createPiece Grail Player1 (newLocation 9 2)
    , createPiece Grail Player1 (newLocation 9 7)
    , createPiece Bow Player1 (newLocation 9 3)
    , createPiece Bow Player1 (newLocation 9 6)
    , createPiece FlagRight Player1 (newLocation 9 0)
    , createPiece FlagLeft Player1 (newLocation 9 9)
    ,
      -- Player 2
      createPiece Pawn Player2 (newLocation 2 0)
    , createPiece Pawn Player2 (newLocation 2 1)
    , createPiece Pawn Player2 (newLocation 2 2)
    , createPiece Pawn Player2 (newLocation 2 3)
    , createPiece Pawn Player2 (newLocation 2 4)
    , createPiece Pawn Player2 (newLocation 2 5)
    , createPiece Pawn Player2 (newLocation 2 6)
    , createPiece Pawn Player2 (newLocation 2 7)
    , createPiece Pawn Player2 (newLocation 2 8)
    , createPiece Pawn Player2 (newLocation 2 9)
    , createPiece Lance Player2 (newLocation 1 4)
    , createPiece Lance Player2 (newLocation 1 5)
    , createPiece Dagger Player2 (newLocation 1 3)
    , createPiece Dagger Player2 (newLocation 1 6)
    , createPiece Sword Player2 (newLocation 1 2)
    , createPiece Sword Player2 (newLocation 1 7)
    , createPiece Grail Player2 (newLocation 0 2)
    , createPiece Grail Player2 (newLocation 0 7)
    , createPiece Bow Player2 (newLocation 0 3)
    , createPiece Bow Player2 (newLocation 0 6)
    , createPiece FlagRight Player2 (newLocation 0 0)
    , createPiece FlagLeft Player2 (newLocation 0 9)
    ]

  pure
    { tickCount: 0
    , pieces
    , capturedPieces: []
    , player: Player1
    , currentPlayer: Player1
    , turn: 1
    , action: 1
    , activePieceIndex: Nothing
    , activeCapturedPieceIndex: Nothing
    , capturedPanel: initializeCapturedPanel
    , gameOverState: None
    , lastReceivedMessage: Nothing
    , sentPing: false
    , receivedPing: false
    , debugString: ""
    }

checkGameOver :: GameState -> GameState
checkGameOver state =
  let
    -- Brute force method
    protectedPieces = filter (_.info.isProtected) state.pieces
    player = state.player
    enemyPlayer = getEnemyPlayer player

    playerProtectedPieces = filter (\p -> isSamePlayer player p.player) protectedPieces
    enemyProtectedPieces = filter (\p -> isSamePlayer enemyPlayer p.player) protectedPieces

    playerProtectedMovements =
      map (\p -> getAllMovements player p.location p.info.movements) playerProtectedPieces
        # foldl (<>) []
        # filter locationInBounds
    enemyProtectedMovements =
      map (\p -> getAllMovements enemyPlayer p.location p.info.movements) enemyProtectedPieces
        # foldl (<>) []
        # filter locationInBounds

    allPieceLocations = map (_.location) state.pieces

    isPlayerWinning = all (_ == true) $ map (\loc -> elem loc allPieceLocations) enemyProtectedMovements
    isEnemyWinning = all (_ == true) $ map (\loc -> elem loc allPieceLocations) playerProtectedMovements

    gameOverState =
      case isPlayerWinning, isEnemyWinning of
        true, true -> Draw
        true, false -> Winner player
        false, true -> Winner enemyPlayer
        false, false -> None
  in
    state { gameOverState = gameOverState }

onTick :: (String -> Effect Unit) -> GameState -> Effect GameState
onTick send gameState = do
  if (not gameState.sentPing) then do
    send $ JSON.writeJSON $ createPingPayload
    pure $ gameState { tickCount = gameState.tickCount + 1, sentPing = true }
  else
    pure $ gameState { tickCount = gameState.tickCount + 1 }

onMouseDown :: (String -> Effect Unit) -> { x :: Int, y :: Int } -> GameState -> Effect GameState
onMouseDown send { x, y } gameState =
  case gameState.gameOverState of
    None ->
      if isSamePlayer gameState.player gameState.currentPlayer then
        (pure gameState)
          <#> checkClickCapturedPanel
          # checkClickBoard
          <#> checkGameOver
      else
        (pure gameState)
          <#> checkClickCapturedPanel
    _ ->
      pure gameState

  where
  nx = toNumber (x - 10)
  ny = toNumber (y - 10)

  checkClickButton :: GameState -> Button -> GameState
  checkClickButton state button' =
    if isClickingButton button' then
      case button'.onClickAction of
        PreviousPage ->
          let
            newPage = state.capturedPanel.currentPage - 1
            clampedNewPage = clamp 0 (state.capturedPanel.currentPageCount - 1) newPage
          in
            state { capturedPanel { currentPage = clampedNewPage } }
        NextPage ->
          let
            newPage = state.capturedPanel.currentPage + 1
            clampedNewPage = clamp 0 (state.capturedPanel.currentPageCount - 1) newPage
          in
            state { capturedPanel { currentPage = clampedNewPage } }
    else
      state
    where
    isClickingButton :: Button -> Boolean
    isClickingButton button =
      buttonLeft <= nx
        && nx <= buttonRight
        && buttonTop <= ny
        && ny <= buttonBottom
      where
      buttonLeft = button.x
      buttonRight = button.x + button.width
      buttonTop = button.y
      buttonBottom = button.y + button.height

  checkClickCapturedPieces :: GameState -> GameState
  checkClickCapturedPieces state =
    let
      isClickingCapturedPiece :: CapturedPieceSlot -> Boolean
      isClickingCapturedPiece slot =
        slotLeft <= nx
          && nx <= slotRight
          && slotTop <= ny
          && ny <= slotBottom
        where
        slotLeft = slot.x
        slotRight = slot.x + slot.width
        slotTop = slot.y
        slotBottom = slot.y + slot.height

      pageOffset = state.capturedPanel.currentPage * state.capturedPanel.maxCapturedPerPage

      mIndex =
        map isClickingCapturedPiece state.capturedPanel.capturedPieceSlots
          # findIndex (_ == true)
    in
      if isSamePlayer state.player state.currentPlayer && mIndex /= state.activeCapturedPieceIndex then
        case mIndex of
          Just index ->
            case state.capturedPieces !! (pageOffset + index) of
              Just _ -> state { activeCapturedPieceIndex = Just (pageOffset + index), activePieceIndex = Nothing }
              Nothing -> state
          Nothing -> state
      else
        state { activeCapturedPieceIndex = Nothing }

  checkClickCapturedPanel :: GameState -> GameState
  checkClickCapturedPanel state =
    let
      buttons = state.capturedPanel.buttons
    in
      foldl checkClickButton state buttons
        # checkClickCapturedPieces

  placeCapturedPiece :: Location -> Int -> Effect GameState -> Effect GameState
  placeCapturedPiece clickLocation index eState = do
    state <- eState

    let
      mResult :: Maybe { newState :: GameState, place_piece_kind :: Maybe PieceKind, place_dest :: Maybe Location }
      mResult = do
        capturedPiece <- state.capturedPieces !! index
        -- Assuming that we also cannot place a piece 
        -- that blocks the movement of our own protected piece
        protectedPieces <- pure $ filter (_.info.isProtected) state.pieces
        allPieceLocations <- pure $ map (_.location) state.pieces
        possibleProtectedMovements <-
          map (\p -> getAllMovements p.player p.location p.info.movements) protectedPieces
            # foldl (<>) []
            # pure
        invalidLocations <- pure $ allPieceLocations <> possibleProtectedMovements

        if locationInBounds clickLocation && not (elem clickLocation invalidLocations) then do
          newCapturedPieces <- deleteAt index state.capturedPieces

          let
            newPieces = state.pieces <> [ createPiece capturedPiece.info.pieceKind capturedPiece.player clickLocation ]
            newCount = length newCapturedPieces
            newPageCount = max 1 (1 + (newCount - 1) / state.capturedPanel.maxCapturedPerPage)
            newPage = min state.capturedPanel.currentPage (newPageCount - 1)

          newState <- pure $ (updateTurnActions state)
            { pieces = newPieces
            , capturedPieces = newCapturedPieces
            , activeCapturedPieceIndex = Nothing
            , capturedPanel { currentPage = newPage, currentPageCount = newPageCount }
            }

          Just
            { newState: newState
            , place_piece_kind: Just capturedPiece.info.pieceKind
            , place_dest: Just clickLocation
            }
        else if locationInBounds clickLocation then
          -- If clicking on the board but not on a placable tile, 
          -- deselect then attempt to select a new piece
          Just
            { newState: selectPiece clickLocation (state { activeCapturedPieceIndex = Nothing })
            , place_piece_kind: Nothing
            , place_dest: Nothing
            }
        else
          Just
            { newState: state
            , place_piece_kind: Nothing
            , place_dest: Nothing
            }

    case mResult of
      Just result ->
        case result.place_piece_kind, result.place_dest of
          Just kind, Just dest -> do
            send $ JSON.writeJSON $ createPlacePayload kind (translateLocation 1 1 $ mirrorLocationVertically dest)
            pure result.newState
          _, _ -> pure result.newState
      Nothing -> pure state

  movePiece :: Location -> Int -> Effect GameState -> Effect GameState
  movePiece clickLocation index eState = do
    state <- eState

    let
      mResult :: Maybe { newState :: GameState, move_src :: Maybe Location, move_dest :: Maybe Location }
      mResult = do
        piece <- state.pieces !! index
        possibleMovements <- pure $ getAllMovements state.player piece.location piece.info.movements # (filter locationInBounds)

        if piece.location /= clickLocation && elem clickLocation possibleMovements then do
          piecesAfterMove <- updateAt index (piece { location = clickLocation }) state.pieces
          case getPieceAtLocation state.pieces clickLocation of
            Just destPiece -> do
              if not (piece.info.isProtected || destPiece.info.isProtected || isSamePlayer destPiece.player state.currentPlayer) then do
                capturedIndex <- getPieceIndex state.pieces destPiece
                piecesAfterCapture <- deleteAt capturedIndex piecesAfterMove

                let
                  newCapturedPieces = state.capturedPieces <> [ { info: destPiece.info, player: state.currentPlayer } ]
                  newCount = length newCapturedPieces
                  newPageCount = 1 + (newCount - 1) / state.capturedPanel.maxCapturedPerPage

                newState <- pure $ (updateTurnActions state)
                  { pieces = piecesAfterCapture
                  , capturedPieces = state.capturedPieces <> [ { info: destPiece.info, player: state.currentPlayer } ]
                  , activePieceIndex = Nothing
                  , capturedPanel { currentPageCount = newPageCount }
                  }

                Just
                  { newState: newState
                  , move_src: Just piece.location
                  , move_dest: Just clickLocation
                  }
              else
                Just
                  { newState: state { activePieceIndex = Nothing }
                  , move_src: Nothing
                  , move_dest: Nothing
                  }
            Nothing ->
              Just
                { newState: (updateTurnActions state) { pieces = piecesAfterMove, activePieceIndex = Nothing }
                , move_src: Just piece.location
                , move_dest: Just clickLocation
                }
        else
          Just
            { newState: state { activePieceIndex = Nothing }
            , move_src: Nothing
            , move_dest: Nothing
            }

    case mResult of
      Just result ->
        case result.move_src, result.move_dest of
          Just src, Just dest -> do
            send $ JSON.writeJSON $ createMovePayload (translateLocation 1 1 $ mirrorLocationVertically src) (translateLocation 1 1 $ mirrorLocationVertically dest)
            pure result.newState
          _, _ -> pure result.newState
      Nothing -> pure state

  selectPiece :: Location -> GameState -> GameState
  selectPiece clickLocation state =
    let
      foundPiece = getPieceAtLocation state.pieces clickLocation
    in
      case foundPiece of
        Just piece ->
          if isSamePlayer piece.player state.player then
            case getPieceIndex state.pieces piece of
              Just index -> state { activePieceIndex = Just index, activeCapturedPieceIndex = Nothing }
              Nothing -> state
          else
            state
        Nothing -> state

  checkClickBoard :: Effect GameState -> Effect GameState
  checkClickBoard eState = do
    state <- eState

    let
      clickLocation = case state.player of
        Player1 -> posToLocation y x
        Player2 -> mirrorLocation $ posToLocation y x

    case state.activePieceIndex, state.activeCapturedPieceIndex of
      Just index, Nothing -> movePiece clickLocation index eState
      Nothing, Just index -> placeCapturedPiece clickLocation index eState
      _, _ -> pure $ selectPiece clickLocation state

onKeyDown :: (String -> Effect Unit) -> String -> GameState -> Effect GameState
onKeyDown _ _ gameState = pure gameState

onKeyUp :: (String -> Effect Unit) -> String -> GameState -> Effect GameState
onKeyUp _ _ gameState = pure gameState

onMessage :: (String -> Effect Unit) -> Message -> GameState -> Effect GameState
onMessage _ message gameState = do
  log $ "Received message: " <> show message

  -- This method is not particularly extendable,
  -- but since this isn't a requirement I'll just leave it as is
  (pure gameState)
    <#> receiveMoveMessage
    <#> receivePlaceMessage
    <#> receivePingMessage
    <#> checkGameOver

  where
  receiveMoveMessage :: GameState -> GameState
  receiveMoveMessage state =
    case JSON.readJSON message.payload of
      Right (payload :: MovePayload) ->
        if not (isSamePlayer message.playerId state.player) && payload.message_type == "move" then
          handleMoveMessage payload state
        else state
      _ -> state

  handleMoveMessage :: MovePayload -> GameState -> GameState
  handleMoveMessage payload state =
    let
      { move_src, move_dest } = payload.message_content
      src = mirrorLocationVertically $ translateLocation (-1) (-1) move_src
      dest = mirrorLocationVertically $ translateLocation (-1) (-1) move_dest

      initialPieces = state.pieces

      mNewState =
        if src /= dest then do
          piece <- getPieceAtLocation initialPieces src
          index <- getPieceIndex initialPieces piece
          piecesAfterMove <- updateAt index (piece { location = dest }) initialPieces

          case getPieceAtLocation initialPieces dest of
            Just capturedPiece -> do
              capturedIndex <- getPieceIndex initialPieces capturedPiece
              piecesAfterCapture <- deleteAt capturedIndex piecesAfterMove
              pure $ (updateTurnActions state) { pieces = piecesAfterCapture }
            Nothing ->
              pure $ (updateTurnActions state) { pieces = piecesAfterMove }
        else
          Nothing
    in
      case mNewState of
        Just newState -> newState
        Nothing -> state

  receivePlaceMessage :: GameState -> GameState
  receivePlaceMessage state =
    case JSON.readJSON message.payload of
      Right (payload :: PlacePayload) ->
        if not (isSamePlayer message.playerId state.player) && payload.message_type == "place" then
          handlePlaceMessage payload message.playerId state
        else state
      _ -> state

  handlePlaceMessage :: PlacePayload -> PlayerId -> GameState -> GameState
  handlePlaceMessage payload player state =
    let
      { place_piece_kind, place_dest } = payload.message_content
      dest = mirrorLocationVertically $ translateLocation (-1) (-1) place_dest

      newState = case pieceKindFromString place_piece_kind of
        Just pieceKind ->
          let
            newPiece = createPiece pieceKind player dest
          in
            (updateTurnActions state) { pieces = state.pieces <> [ newPiece ] }
        Nothing -> state
    in
      newState

  -- Not a reliable method of handling connections
  -- It would be better if I had direct access to the game engine methods
  -- However, this should suffice at least
  receivePingMessage :: GameState -> GameState
  receivePingMessage state =
    if not state.receivedPing then
      case JSON.readJSON message.payload of
        Right (payload :: PingPayload) ->
          if payload.message_type == "ping" then
            state { player = message.playerId, receivedPing = true }
          else state
        _ -> state
    else state

onRender :: Map.Map String Canvas.CanvasImageSource -> Canvas.Context2D -> GameState -> Effect Unit
onRender images ctx gameState = do
  renderGame

  where
  renderGame :: Effect Unit
  renderGame = do
    clearCanvas ctx { color: "black", width, height }
    renderBoard gameState.player gameState.pieces gameState.activePieceIndex
    renderCapturedPanel gameState.capturedPanel gameState.capturedPieces gameState.activeCapturedPieceIndex gameState.player
    renderGameOver gameState.gameOverState

  renderTile :: Int -> Int -> Effect Unit
  renderTile r c =
    let
      color =
        if mod (r + c) 2 == 0 then
          "lightgreen"
        else
          "lightyellow"
    in
      drawRect ctx
        { x: (toNumber c) * tileLength
        , y: (toNumber r) * tileLength
        , width: tileLength
        , height: tileLength
        , color
        }

  renderRow :: Int -> Effect Unit
  renderRow row =
    let
      cs = (0 .. (cols - 1))
    in
      foldl (<>) (pure unit) $ (renderTile row) <$> cs

  renderGrid :: Effect Unit
  renderGrid =
    let
      rs = (0 .. (rows - 1))
    in
      foldl (<>) (pure unit) $ renderRow <$> rs

  renderPiece :: PlayerId -> Piece -> Effect Unit
  renderPiece player piece =
    let
      location = case player of
        Player1 -> piece.location
        Player2 -> mirrorLocation piece.location
      x = tileLength * (toNumber location.col)
      y = tileLength * (toNumber location.row)

      lookupResult = Map.lookup (getImagePath piece.info.pieceKind piece.player) images
    in
      case lookupResult of
        Nothing -> pure unit
        Just img ->
          case player of
            Player1 -> drawImageScaled ctx img { x, y, width: tileLength, height: tileLength }
            Player2 -> drawHorizontallyMirroredImage ctx img { x, y, imgWidth: tileLength, imgHeight: tileLength }

  renderPieces :: PlayerId -> Array Piece -> Effect Unit
  renderPieces player pieces =
    foldl (<>) (pure unit) $ (renderPiece player) <$> pieces

  renderActivePiece :: PlayerId -> Array Piece -> Maybe Int -> Effect Unit
  renderActivePiece player pieces mIndex =
    case mIndex of
      Just index ->
        case pieces !! index of
          Just activePiece ->
            let
              location = case player of
                Player1 -> activePiece.location
                Player2 -> mirrorLocation activePiece.location
              x = tileLength * (toNumber location.col)
              y = tileLength * (toNumber location.row)
            in
              drawRectOutline ctx { x, y, width: tileLength, height: tileLength, color: "white" }
          Nothing -> pure unit
      Nothing -> pure unit

  renderBoard :: PlayerId -> Array Piece -> Maybe Int -> Effect Unit
  renderBoard player pieces mIndex = do
    renderGrid
    renderPieces player pieces
    renderActivePiece player pieces mIndex

  renderCapturedPiece :: CapturedPieceSlot -> CapturedPiece -> Effect Unit
  renderCapturedPiece slot capturedPiece =
    let
      lookupResult = Map.lookup (getImagePath capturedPiece.info.pieceKind capturedPiece.player) images
    in
      case lookupResult of
        Nothing -> pure unit
        Just img -> drawImageScaled ctx img { x: slot.x, y: slot.y, width: slot.width, height: slot.height }

  renderCapturedPieces :: Array CapturedPieceSlot -> Array CapturedPiece -> Int -> Effect Unit
  renderCapturedPieces capturedPieceSlots capturedPieces page =
    let
      pageStart = page * maxCapturedPerPage
      pageEnd = pageStart + maxCapturedPerPage
      pagedCapturedPieces = slice pageStart pageEnd capturedPieces
    in
      foldl (<>) (pure unit) $ zipWith renderCapturedPiece capturedPieceSlots pagedCapturedPieces

  renderActiveCapturedPiece :: Array CapturedPieceSlot -> Maybe Int -> Int -> Int -> Effect Unit
  renderActiveCapturedPiece slots mIndex page maxPerPage =
    case mIndex of
      Just index ->
        let
          pageIndex = index / maxPerPage
          slotIndex = mod index maxPerPage
        in
          if page == pageIndex then
            case slots !! slotIndex of
              Just slot ->
                drawRectOutline ctx { x: slot.x, y: slot.y, width: slot.width, height: slot.height, color: "white" }
              Nothing -> pure unit
          else pure unit
      Nothing -> pure unit

  renderCapturedPanelButton :: Button -> Effect Unit
  renderCapturedPanelButton panelButton = do
    drawText ctx
      { x: panelButton.x + panelButton.width / 2.0
      , y: panelButton.y + panelButton.height / 2.0 + (toNumber panelButton.fontSize) / 2.0
      , text: panelButton.text
      , color: panelButton.textColor
      , font: panelButton.font
      , size: panelButton.fontSize
      }

  renderCapturedPanelButtons :: Array Button -> Effect Unit
  renderCapturedPanelButtons panelButtons =
    foldl (<>) (pure unit) $ renderCapturedPanelButton <$> panelButtons

  renderPageText :: Maybe Text -> Int -> Int -> Effect Unit
  renderPageText mPageText page pageCount =
    case mPageText of
      Just pageText ->
        drawText ctx
          { x: pageText.x + pageText.width / 2.0
          , y: pageText.y + pageText.height / 2.0 + (toNumber pageText.fontSize) / 2.0
          , text: "Page " <> show (page + 1) <> " of " <> show pageCount
          , color: pageText.textColor
          , font: pageText.font
          , size: pageText.fontSize
          }
      Nothing -> pure unit

  renderCapturedPanel :: CapturedPanel -> Array CapturedPiece -> Maybe Int -> PlayerId -> Effect Unit
  renderCapturedPanel capturedPanel capturedPieces activeCapturedPieceIndex player = do
    let
      panelColor = case player of
        Player1 -> "sandybrown"
        Player2 -> "deepskyblue"

    drawRect ctx { x: capturedPanel.x, y: capturedPanel.y, width: capturedPanel.width, height: capturedPanel.height, color: panelColor }
    renderPageText capturedPanel.pageText capturedPanel.currentPage capturedPanel.currentPageCount
    renderCapturedPanelButtons capturedPanel.buttons
    renderCapturedPieces capturedPanel.capturedPieceSlots capturedPieces capturedPanel.currentPage
    renderActiveCapturedPiece capturedPanel.capturedPieceSlots activeCapturedPieceIndex capturedPanel.currentPage capturedPanel.maxCapturedPerPage

  renderGameOver :: GameOverState -> Effect Unit
  renderGameOver gameOverState =
    let
      mGameOverText = case gameOverState of
        Winner winner ->
          case winner of
            Player1 -> Just "Player 1 (Orange) Wins"
            Player2 -> Just "Player 2 (Blue) Wins"
        Draw -> Just "Draw"
        None -> Nothing

      size = 18
    in
      case mGameOverText of
        Just text -> do
          drawRect ctx
            { x: 0.0
            , y: 0.0
            , width
            , height
            , color: "#000000AA"
            }

          drawText ctx
            { x: width / 2.0
            , y: height / 2.0 + (toNumber size) / 2.0
            , text
            , color: "white"
            , font: "arial"
            , size
            }
        Nothing -> pure unit

main :: Effect Unit
main =
  startNetworkGame
    { initialState
    , onTick
    , onMouseDown
    , onKeyDown
    , onKeyUp
    , onRender
    , onMessage
    , fps
    , width
    , height
    , ipAddress: "localhost"
    , port: 15000
    , imagePaths
    }
