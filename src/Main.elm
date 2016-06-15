import Cmd          exposing (..)
import InputHandler exposing (..)
import Node         exposing (..)
import Render       exposing (..)
import Stack        exposing (..)

import Char                 exposing (fromCode)
import Color                exposing (..)
import Keyboard
import Lazy                 exposing (force)
import Signal               exposing (..)
import String
import Text                 exposing (..)

scene = Signal.map 
    (\children ->
        {nodeType = Rect
            {rectDef
            | extents = (Fill 1.0, Fill 1.0)
            ,dir = Down 0.0
            ,children = children
            }
        ,status = Enabled
        }
    ) cmdScenes

cmdScenes : Signal (List Node)
cmdScenes = Signal.map cmdScene cmdStates

cmdScene : (CmdControlState, String) -> List Node
cmdScene (controlState, ps) = 
    [   {nodeType = Rect
            {rectDef
            | extents = (Fill 1.0, Fix 25.0)
            ,dir = Right 0.0
            ,border = Just {thickness = All 3.0, color = grey}
            ,children = 
                (Stack.toList controlState.nodes) 
                    ++ (Stack.toList controlState.tempNodes)
            }
        ,status = Enabled
        }
        ,{nodeType = Rect
            {rectDef
            | extents = (Fill 1.0, Fill 1.0)
            ,dir = Right 0.0
            ,children = controlState.scene
            }
        ,status = Enabled
        }
    ]

initState : (CmdControlState, List Int)
initState = 
     -- aplying the folding function to get initial nodes
    cmdState 0
    (
        {state = [] 
        ,nodes = []
        ,tempNodes = []
        ,inputHandler = actionTypeCmds
        ,scene = []
        }
    ,[]
    )

cmdStates : Signal (CmdControlState, String)
cmdStates = Signal.foldp 
    -- folding function
    cmdState
    initState
    -- feeding signal
    Keyboard.presses
    -- convert char codes to string
    |> Signal.map (\(state, ps) -> (state, intListToString ps))

intListToString : List Int -> String
intListToString charCodes = String.fromList (List.map fromCode charCodes)

stringToIntList : String -> List Int
stringToIntList str = String.toList str |> List.map Char.toCode

cmdState : Int -> (CmdControlState, List Int) 
   -> (CmdControlState, List Int)
cmdState press (controlState, ps) =
    let input = pressesToString press ps
        a = Debug.log "press" press
        fromString str = String.toList str
            |> List.map Char.toCode
        process = 
            let (controlState, chars) = processCmd controlState input
            in  (controlState, stringToIntList chars)
    in -- Enter
        if press == 13
            then case tryExecuteCmd controlState of
                Just newState -> (newState, [])
                Nothing -> process
        else process

pressesToString : Int -> List Int -> String
pressesToString press ps = press :: (List.reverse ps)
        |> List.map fromCode 
        |> List.reverse
        |> String.fromList

tryExecuteCmd : CmdControlState -> Maybe (CmdControlState)
tryExecuteCmd controlState = 
    let a = Debug.log "a" controlState.state
    in case toList controlState.state of
        [InsertAction, RectangleAction rectDef] -> Just
            {controlState
            |scene = 
                {nodeType = Rect rectDef
                ,status = Enabled
                } :: controlState.scene
            }
        _ -> Nothing

processCmd : CmdControlState -> String 
   -> (CmdControlState, String)
processCmd controlState input = case controlState.inputHandler of
    InputHandler ih -> force ih input controlState

main = render scene
{-
TODO
 * fix relatives and popups order
 * make border list of Inner | Outer | Middle
-}
