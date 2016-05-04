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
    | RectangleState
    | TextState
    | ExtentsState
    | FloatState Float
    | DirectionState

cmds : Cmd CmdState
cmds = toSus 
    [{name = "change"
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
    }]

createCmds : Cmd CmdState
createCmds = toSus 
    [{name = "rectangle"
    ,cmdState = RectangleState
    ,common = {after = " with ", next = Just rectangleCmds}
    }
    ,{name = "text"
    ,cmdState = TextState
    ,common = {after = "", next = Nothing}
    }]

rectangleCmds : Cmd CmdState
rectangleCmds = toSus 
    [{name = "extents"
    ,cmdState = ExtentsState
    ,common = {after = "=(", next = Just fstExtentCmd}
    }
    ,{name = "direction"
    ,cmdState = DirectionState
    ,common = {after = "", next = Nothing}
    }]

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
cmdScenes = Signal.map 
    (\((state, cs, cmd), ps) ->
        {nodeType = Rect
            {rectDef
            | extents = (Fill 1.0, Fix 25.0)
            ,dir = Right 0.0
            ,border = Just {thickness = All 3.0, color = grey}
            ,children = Stack.toList cs
            }
        ,status = Enabled
        }
    ) (cmdStates (Just cmds))

cmdStates : Maybe (Cmd CmdState)
   -> Signal ((Stack CmdState, Stack Node, Maybe (Cmd CmdState)), String)
cmdStates cmd = Signal.foldp (\press ((cmdState, children, cmd'), ps) -> 
        let toString = press :: (List.reverse ps)
                |> List.map Char.fromCode 
                |> List.reverse
                |> String.fromList
            a = Debug.log "press" press
            fromString str = String.toList str
                |> List.map Char.toCode
            childrenTail = pop children |> snd
            cs = case cmd' of
                Just (Sus state c) -> 
                    let str = toString |> matchSus (cmdShorts c)
                        mc = matchCount str c
                        chars = fromString str
                        cmd'' = Just <| Sus mc c
                        after m = 
                            {nodeType = Text (textDef m.common.after)
                            ,status = Enabled}
                    in case mc of
                        Ambiguous str ->
                            ((cmdState
                                ,push (susChildren cmd'') childrenTail
                                ,cmd')
                            ,chars)
                        Match match ->
                            ((push (match.cmdState) cmdState
                                ,push (susChildren match.common.next)
                                     <| push (after match)
                                     <| push (susChildren cmd'')
                                        childrenTail
                                ,match.common.next)
                            ,[])
                Just (Float' state) -> 
                    case (press, String.toFloat state.value) of
                        (32, Ok f) ->
                            -- Spacebar
                            let cmd'' = state.common.next
                                after = 
                                    {nodeType = 
                                        Text (textDef state.common.after)
                                    ,status = Enabled}
                            in ((push (FloatState f) cmdState
                                    ,push (emptyNode)
                                        <| push (after) children
                                    ,cmd'')
                               ,[])
                        (44, Ok f) -> 
                            -- Comma
                            let cmd'' = state.common.next
                                after = 
                                    {nodeType = 
                                        Text (textDef state.common.after)
                                    ,status = Enabled}
                            in ((push (FloatState f) cmdState
                                    ,push (emptyNode)
                                        <| push (after) children
                                    ,cmd'')
                                ,[])
                        _ ->
                            let str = filterFloat toString
                                chars = fromString str
                                cmd'' = Just <| Float' {state | value = str}
                            in ((cmdState
                                    ,push
                                        {nodeType = Text (textDef str)
                                        ,status = Enabled}
                                        childrenTail
                                    ,cmd'')
                                ,chars)
                _ -> (([], [], cmd'), [])
        in cs
   ) (([], [ susChildren cmd], cmd), []) 
        Keyboard.presses
        |> Signal.map (\(state, ps) ->
            (state, String.fromList (List.map Char.fromCode ps)))

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
