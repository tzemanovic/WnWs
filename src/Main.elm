import Cmd          exposing (..)
import InputHandler exposing (..)
import Node         exposing (..)
import Render       exposing (..)
import Stack        exposing (..)

import Char                 exposing (fromCode)
import Color                exposing (..)
import Graphics.Input.Field exposing (Content, noContent)
import Keyboard
import Signal               exposing (..)
import String
import Text                 exposing (..)

type CmdState
    = ChangeState
    | DeleteState
    | InsertState
    | AppendState
    | RectangleState RectDef
    | TextState
    | ExtentsState
    | FloatState Float
    | DirectionState

cmds : Cmd CmdState
cmds = toSus 
    [   {name = "change"
        ,cmdState = ChangeState
        ,common = {after = "", next = Nothing}
        }
        ,{name = "delete"
        ,cmdState = DeleteState
        ,common = {after = "", next = Nothing}
        }
        ,{name = "insert"
        ,cmdState = InsertState
        ,common = {after = " ", next = Just createCmds}
        }
        ,{name = "append"
        ,cmdState = AppendState
        ,common = {after = "", next = Just createCmds}
        }
    ]

createCmds : Cmd CmdState
createCmds = toSus 
    [   {name = "rectangle"
        ,cmdState = RectangleState rectDef
        ,common = {after = " with ", next = Just rectangleCmds}
        }
        ,{name = "text"
        ,cmdState = TextState
        ,common = {after = "", next = Nothing}
        }
    ]

rectangleCmds : Cmd CmdState
rectangleCmds = toSus 
    [   {name = "extents"
        ,cmdState = ExtentsState
        ,common = {after = "=(", next = Just fstExtentCmd}
        }
        ,{name = "direction"
        ,cmdState = DirectionState
        ,common = {after = "", next = Nothing}
        }
    ]

fstExtentCmd : Cmd CmdState
fstExtentCmd = Float' 
    {value = ""
    ,common = {after = ", ", next = Just sndExtentCmd}
    }

sndExtentCmd : Cmd CmdState
sndExtentCmd = Float' 
    {value = ""
    ,common = {after = ")", next = Nothing}
    }

scene' = Signal.map 
    (\children ->
        {nodeType = Rect
            {rectDef
            | extents = (Fill 1.0, Fill 1.0)
            ,dir = Down 0.0
            ,children = [children]
            }
        ,status = Enabled
        }
    ) cmdScenes

cmdScenes : Signal Node
cmdScenes = Signal.map cmdScene (cmdStates (Just cmds))

cmdScene : (CmdControlState, String) -> Node
cmdScene (controlState, ps) = 
    {nodeType = Rect
        {rectDef
        | extents = (Fill 1.0, Fix 25.0)
        ,dir = Right 0.0
        ,border = Just {thickness = All 3.0, color = grey}
        ,children = Stack.toList controlState.nodes
        }
    ,status = Enabled
    }

type alias CmdControlState =
    {states : Stack CmdState
    ,nodes : Stack Node
    ,cmd : Maybe (Cmd CmdState)
    }

cmdStates : Maybe (Cmd CmdState) -> Signal (CmdControlState, String)
cmdStates cmd = Signal.foldp 
    -- folding function
    cmdState
    -- initial state
    (   {states = [] 
        ,nodes = [ susChildren cmd]
        ,cmd = cmd
        }
    ,[]
    ) 
    -- feeding signal
    Keyboard.presses
    -- convert char codes to string
    |> Signal.map (\(state, ps) -> (state, intListToString ps))

intListToString : List Int -> String
intListToString charCodes = String.fromList (List.map Char.fromCode charCodes)

stringToIntList : String -> List Int
stringToIntList str = String.toList str |> List.map Char.toCode

cmdState : Int -> (CmdControlState, List Int) -> (CmdControlState, List Int)
cmdState press (controlState, ps) =
    let input = pressesToString press ps
        a = Debug.log "press" press
        fromString str = String.toList str
            |> List.map Char.toCode
        nodesTail = pop controlState.nodes |> snd
    in -- Enter
        if press == 12
            -- TODO: try to execute the command, clear chars
            then (controlState, []) 
        -- Spacebar or Comma
        else if press == 32 || press == 44
            then case tryConfirmCmd controlState of
                Just newState -> (newState, [])
                Nothing -> processCmd controlState input
            else processCmd controlState input

tryConfirmCmd : CmdControlState -> Maybe CmdControlState
tryConfirmCmd controlState = case controlState.cmd of
    Just (Float' state) -> case String.toFloat state.value of
        Ok f -> -- valid Float number
            let nextCmd = state.common.next
                after = 
                    {nodeType = 
                        Text (textDef state.common.after)
                    ,status = Enabled}
            in Just 
                {states = push (FloatState f) controlState.states 
                ,nodes = push emptyNode
                        <| push after controlState.nodes
                ,cmd = nextCmd
                }
        _ -> Nothing -- not a valid Float number
    _ -> Nothing

processCmd : CmdControlState -> String -> (CmdControlState, List Int)
processCmd controlState input = 
    case controlState.cmd of
        Just (Sus state susCmds) -> 
            processSusCmd state susCmds controlState input
        Just (Float' state) -> 
            processFloatCmd state controlState input
        _ -> (controlState, [])

processSusCmd : SusState CmdState -> List (SusCmd CmdState) 
    -> CmdControlState -> String -> (CmdControlState, List Int)
processSusCmd susState susCmds controlState input =
    let str = input |> matchSus (cmdShorts susCmds)
        mc = matchCount str susCmds
        chars = stringToIntList str
        cmd = Just <| Sus mc susCmds
        after m = 
            {nodeType = Text (textDef m.common.after)
            ,status = Enabled}
        nodesTail = pop controlState.nodes |> snd
    in case mc of
        Ambiguous str ->
            (   {states = controlState.states
                ,nodes = push (susChildren cmd) nodesTail
                ,cmd = cmd
                }
            , chars)
        Match match ->
            (   {states = push (match.cmdState) controlState.states
                ,nodes = push (susChildren match.common.next)
                        <| push (after match)
                        <| push (susChildren cmd) nodesTail
                ,cmd = match.common.next
                }
            , [])

processFloatCmd : FloatCmd CmdState -> CmdControlState -> String
    -> (CmdControlState, List Int)
processFloatCmd floatState controlState input =
    let str = filterFloat input
        chars = stringToIntList str
        cmd = Just <| Float' {floatState | value = str}
        nodesTail = pop controlState.nodes |> snd
    in (    {states = controlState.states
            ,nodes = push
                    {nodeType = Text (textDef str)
                    ,status = Enabled}
                    nodesTail
            ,cmd = cmd
            }
        , chars)

pressesToString : Int -> List Int -> String
pressesToString press ps = press :: (List.reverse ps)
        |> List.map Char.fromCode 
        |> List.reverse
        |> String.fromList

susChildren : Maybe (Cmd CmdState) -> Node
susChildren cmd = case cmd of
    Just (Sus state c) ->
        let text = case state of
                Ambiguous str -> str
                Match match -> match.name
        in {nodeType = Rect
                {rectDef
                | extents = (Fit, Fit)
                ,dir = Right 0.0
                ,children = 
                    [{nodeType = Text 
                        <| textDef 
                        <| if String.isEmpty text then " " else text
                    ,status = Enabled
                    }]
                ,relatives = relatives' state c}
            ,status = Enabled
            }
    Just (Float' state) ->
        let text = state.value
        in {nodeType = Rect
                {rectDef
                | extents = (Fit, Fit)
                ,dir = Right 0.0
                ,children = 
                    [{nodeType = Text 
                        <| textDef 
                        <| if String.isEmpty text then " " else text
                    ,status = Enabled
                    }]
                }
            ,status = Enabled
            }
    _ -> emptyNode

relatives' : SusState CmdState -> List (SusCmd CmdState) -> List (Node, Sizes)
relatives' state options = case state of
    Match _ -> []
    Ambiguous text -> 
        [({nodeType = Rect
            {rectDef
            | extents = (Fit, Fit) 
            ,dir = Down 0.0
            ,border = Nothing
            ,children = List.map 
                (\o ->
                    {nodeType = Rect
                        {rectDef
                        | extents = (Fit, Fit)
                        ,dir = Right 0.0
                        ,border = Just 
                            {thickness = TRBL 0.0 0.0 3.0 0.0
                            ,color = relBsClr}
                        ,children =
                            [{nodeType = Text 
                                {text = append 
                                    (Text.color shortClr 
                                        (fromString o.short))
                                    (Text.color afterShortClr 
                                        (fromString o.afterShort))
                                } 
                            ,status = Enabled
                            }]
                        } 
                    ,status = if String.startsWith text o.short 
                        then Enabled else Disabled
                    }
               ) options
            }
        ,status = Enabled}, (0.0, 0.0))]

matchCount : String -> List (SusCmd CmdState) -> SusState CmdState
matchCount str options =
    if String.isEmpty str
    then Ambiguous str
    else case List.filter (\o -> String.startsWith str o.short) options of
        x :: [] -> Match x
        _ -> Ambiguous str

matchSus : List String -> String -> String
matchSus shortOptions string = 
    let matching = List.any (\short -> 
            String.startsWith string short) shortOptions
    in if matching then string else ""

space = {nodeType = Text <| textDef " ", status = Enabled}

emptyNode = 
    {nodeType = Text (textDef "")
    ,status = Disabled}

shortClr        = rgb 000 000 000
afterShortClr   = rgb 125 125 125
relBsClr        = rgb 225 225 225
bsClr           = rgb 000 000 000

main = render scene'
{-
TODO
 * fix relatives and popups order
 * make border list of Inner | Outer | Middle
-}
