module Main (..) where

import Cmd exposing (..)
import Node exposing (..)
import Render exposing (..)
import Stack exposing (..)
import Char exposing (fromCode)
import Color exposing (..)
import Graphics.Element exposing (Element)
import Keyboard
import Lazy exposing (force)
import Signal exposing (..)
import String


scene : Signal Node
scene =
    Signal.map
        (\children ->
            { nodeType =
                Rect
                    { rectDef
                        | extents = ( Fill 1.0, Fill 1.0 )
                        , dir = Down 0.0
                        , children = children
                    }
            , status = Enabled
            }
        )
        sceneNodes


sceneNodes : Signal (List Node)
sceneNodes =
    Signal.map cmdStateToNodes cmdStates


cmdStateToNodes : ( ControlState, String ) -> List Node
cmdStateToNodes ( controlState, ps ) =
    [ { nodeType =
            Rect
                { rectDef
                    | extents = ( Fill 1.0, Fix 25.0 )
                    , dir = Right 0.0
                    , border = Just { thickness = All 3.0, color = grey }
                    , children =
                        (Stack.toList controlState.nodes)
                            ++ (Stack.toList controlState.tempNodes)
                }
      , status = Enabled
      }
    , { nodeType =
            Rect
                { rectDef
                    | extents = ( Fill 1.0, Fill 1.0 )
                    , dir = Down 0.0
                    , children = controlState.scene
                }
      , status = Enabled
      }
    ]


initCmdState : ( ControlState, List Int )
initCmdState =
    let
        initControlState =
            { state = []
            , nodes = []
            , tempNodes = []
            , inputHandler = actionTypeCmds
            , scene = []
            }

        -- aplying the folding function to get initial nodes
    in
        processCmd initControlState 0 ( initControlState, [] )


cmdStates : Signal ( ControlState, String )
cmdStates =
    Signal.foldp
        -- folding function
        (processCmd <| fst initCmdState)
        initCmdState
        -- feeding signal
        Keyboard.presses
        -- convert char codes to string
        |>
            Signal.map (\( state, ps ) -> ( state, intListToString ps ))


intListToString : List Int -> String
intListToString charCodes =
    String.fromList (List.map fromCode charCodes)


stringToIntList : String -> List Int
stringToIntList str =
    String.toList str |> List.map Char.toCode


processCmd :
    ControlState
    -> Int
    -> ( ControlState, List Int )
    -> ( ControlState, List Int )
processCmd initControlState keyPress ( controlState, ps ) =
    let
        input =
            pressesToString keyPress ps

        a =
            Debug.log "press" keyPress

        fromString str =
            String.toList str
                |> List.map Char.toCode

        process =
            let
                ( controlState, chars ) =
                    applyInputHandler controlState input
            in
                ( controlState, stringToIntList chars )
    in
        -- Enter
        if keyPress == 13 then
            case tryExecuteCmd initControlState controlState of
                Just newState ->
                    ( newState, [] )

                Nothing ->
                    process
        else
            process


pressesToString : Int -> List Int -> String
pressesToString press ps =
    press
        :: (List.reverse ps)
        |> List.map fromCode
        |> List.reverse
        |> String.fromList


tryExecuteCmd : ControlState -> ControlState -> Maybe (ControlState)
tryExecuteCmd initControlState controlState =
    let
        a =
            Debug.log "a" controlState.state
    in
        case toList controlState.state of
            [ InsertAction, RectangleAction rectDef ] ->
                Just
                    { controlState
                        | state = []
                        , nodes = initControlState.nodes
                        , tempNodes = initControlState.tempNodes
                        , inputHandler = initControlState.inputHandler
                        , scene =
                            { nodeType = Rect rectDef
                            , status = Enabled
                            }
                                :: controlState.scene
                    }

            _ ->
                Nothing


applyInputHandler :
    ControlState
    -> String
    -> ( ControlState, String )
applyInputHandler controlState input =
    case controlState.inputHandler of
        InputHandler ih ->
            force ih input controlState


main : Signal Element
main =
    render scene



{-
   TODO
    * fix relatives and popups order
    * make border list of Inner | Outer | Middle
-}
