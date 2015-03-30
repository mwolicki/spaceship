import Graphics.Element (..)
import Graphics.Collage(..)
import Signal
import Signal (..)
import Text
import Time
import Window
import Keyboard
import Color(rgb)
import Array(..)
import List
import Debug

{-- Part 1: Model the user input ----------------------------------------------

What information do you need to represent all relevant user input?

Task: Redefine `UserInput` to include all of the information you need.
      Redefine `userInput` to be a signal that correctly models the user
      input as described by `UserInput`.

------------------------------------------------------------------------------}

type MoveInstruction = Right | Left | None

type alias UserInput = {
  move : MoveInstruction, 
  spacePressed: Bool }


keyboardInput = (\{x,y}->case x of 
                           1 -> Left
                           (-1) -> Right
                           _ -> None) <~ Keyboard.arrows 
                                        
                                        

userInput : Signal UserInput
userInput = (\inst space->{move = inst, spacePressed = space}) <~ keyboardInput ~ Keyboard.space


type alias Input = { timeDelta : Float, userInput : UserInput }



{-- Part 2: Model the game ----------------------------------------------------

What information do you need to represent the entire game?

Tasks: Redefine `GameState` to represent your particular game.
       Redefine `defaultGame` to represent your initial game state.

For example, if you want to represent many objects that just have a position,
your GameState might just be a list of coordinates and your default game might
be an empty list (no objects at the start):

    type GameState = { objects : [(Float,Float)] }
    defaultGame = { objects = [] }

------------------------------------------------------------------------------}

type alias LaserState = {x:Int, y:Int, yv:Int}

type alias GameState = {
  lasers : List LaserState,
  fireBanned: Int,
  x : Int,
  maxX : Int,
  move : MoveInstruction
}

defaultGame : GameState
defaultGame = { x = 0, maxX =0, move = None, fireBanned=0, lasers = [] }



{-- Part 3: Update the game ---------------------------------------------------

How does the game step from one state to another based on user input?

Task: redefine `stepGame` to use the UserInput and GameState
      you defined in parts 1 and 2. Maybe use some helper functions
      to break up the work, stepping smaller parts of the game.

------------------------------------------------------------------------------}
moveShip: UserInput -> GameState -> GameState
moveShip {move} gameState = 
  { gameState | x <- case move of
                      Right -> gameState.x-10
                      Left ->  gameState.x+10
                      _ -> gameState.x
  }
  
  
fireLaser: UserInput -> GameState -> GameState
fireLaser {spacePressed} gs =
  if | (spacePressed && gs.fireBanned<=0) -> { gs | fireBanned <- 7,
                                                    lasers <- {x = gs.x,y=0,yv=7}::gs.lasers }
     | (gs.fireBanned>0) -> { gs | fireBanned <- gs.fireBanned - 1 }
                                           
     | True -> gs

moveLaser: GameState -> GameState
moveLaser gs = 
  if |((List.length gs.lasers) == 0) -> gs
     | True ->  { gs | lasers <- gs.lasers |> List.map (\l -> {l | y<- l.y + l.yv}) |> List.filter (\l -> l.y<900) }


stepGame : Input -> GameState -> GameState
stepGame {timeDelta,userInput} gameState =
    fireLaser userInput gameState |> moveLaser |> moveShip userInput |> Debug.watch "gameState"



{-- Part 4: Display the game --------------------------------------------------

How should the GameState be displayed to the user?

Task: redefine `display` to use the GameState you defined in part 2.

------------------------------------------------------------------------------}

display : (Int,Int) -> GameState -> Element
display (w,h) gameState =
    let x = (toFloat(w)/2.0) in
    let y = (toFloat(h)/2.0) in
    let list = [  
     fittedImage w h "img/galaxy.png" 
       |> toForm,
     image 100 100 "img/destroyer.png" 
       |> toForm 
       |> move (gameState.x |> toFloat,-y + 35),
     image 50 75 "img/shuttlenoweps.png" 
       |> toForm 
       |> move (gameState.x |> toFloat, y - 35)] in
     let lasers = List.map(\l -> 
       rect 5.0 15.0 |> filled (rgb 0 255 0) |> move (toFloat l.x, -y + 50 + (toFloat l.y))) gameState.lasers in
      collage w h (List.append list lasers)
   

{-- That's all folks! ---------------------------------------------------------

The following code puts it all together and shows it on screen.

------------------------------------------------------------------------------}

delta : Signal Float
delta = Time.fps 25


input : Signal Input
input = Debug.watch "arrows" <~Signal.sampleOn delta (Signal.map2 Input delta userInput)


gameState : Signal GameState
gameState = Signal.foldp stepGame defaultGame input



main : Signal Element
main = Signal.map2 display Window.dimensions gameState
