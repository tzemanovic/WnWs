module
    Cmd
    ( Action(..)
    , InputHandler(..)
    , CmdCommon
    , ControlState
    , SusCmd
    , SusState(..)
    , FloatCmd
    , actionTypeCmds
    )
    where

import InputHandler exposing (..)
import Node exposing (..)
import Stack exposing (..)
import Color exposing (..)
import Lazy exposing (Lazy, force, lazy)
import String exposing (left)
import Text exposing (..)


type Action
    = ChangeAction
    | DeleteAction
    | InsertAction
    | AppendAction
    | RectangleAction RectDef
    | SelectionAction Int
    | TextAction


type InputHandler
    = InputHandler
        (Lazy
            (String
             -- input
             -> ControlState
                -- last state
             -> ( ControlState, String )
             -- new state, folding input
            )
        )


type alias CmdCommon =
    { textAfter : String
    }


type alias ControlState =
    { state : Stack Action
    , nodes :
        Stack Node
        -- temporary nodes for cmd, removed when moving on to next cmd
    , tempNodes : Stack Node
    , inputHandler : InputHandler
    , scene : List Node
    }


type alias SusCmd =
    { name : String
    , nodes : List Node
    , short : String
    , stateHandler : Stack Action -> Stack Action
    , next : Maybe InputHandler
    , common : CmdCommon
    }


type SusState
    = Ambiguous String
    | Match SusCmd


type alias FloatCmd =
    { next : InputHandler
    , stateHandler : Float -> Stack Action -> Stack Action
    , before : String
    , common : CmdCommon
    }


actionTypeCmds : InputHandler
actionTypeCmds =
    InputHandler
        (lazy
            (\() ->
                \input controlState ->
                    let
                        cmds : List SusCmd
                        cmds =
                            [ { name = "append"
                              , nodes = [ textNode <| susText "" "a" "ppend after" ]
                              , short = "a"
                              , stateHandler = push AppendAction
                              , next = Just createCmds
                              , common =
                                    { textAfter = " "
                                    }
                              }
                            , { name = "change"
                              , nodes = [ textNode <| susText "" "c" "hange" ]
                              , short = "c"
                              , stateHandler = push ChangeAction
                              , next = Just createCmds
                              , common =
                                    { textAfter = " "
                                    }
                              }
                            , { name = "delete"
                              , nodes = [ textNode <| susText "" "d" "elete" ]
                              , short = "d"
                              , stateHandler = push DeleteAction
                              , next = Just createCmds
                              , common =
                                    { textAfter = " "
                                    }
                              }
                            , { name = "insert"
                              , nodes = [ textNode <| susText "" "i" "nsert before" ]
                              , short = "i"
                              , stateHandler = push InsertAction
                              , next = Just createCmds
                              , common =
                                    { textAfter = " "
                                    }
                              }
                            ]
                    in
                        susHandler input controlState cmds
            )
        )


textClr : Color
textClr =
    rgb 125 125 125


shortTextClr : Color
shortTextClr =
    rgb 0 0 0


valueClr : Color
valueClr =
    rgb 0 128 171


relBsClr : Color
relBsClr =
    rgb 225 225 225



-- INTERNAL


susHandler :
    String
    -> ControlState
    -> List SusCmd
    -> ( ControlState, String )
susHandler input controlState cmds =
    let
        match : SusState
        match =
            matchingCmd input cmds
    in
        case match of
            Ambiguous matchedInput ->
                ( { controlState
                    | tempNodes = [ cmdsToNodesAmbiguous matchedInput cmds ]
                  }
                , matchedInput
                )

            Match match ->
                let
                    state : Stack Action
                    state =
                        match.stateHandler controlState.state

                    after : Node
                    after =
                        textNode
                            (Text.color textClr
                                (fromString match.common.textAfter)
                            )
                in
                    case match.next of
                        Just (InputHandler ih) ->
                            --nextControlState : ControlState
                            let
                                ( nextControlState, _ ) =
                                    force ih
                                        ""
                                        { controlState
                                            | state = state
                                        }

                                nodes : Stack Node
                                nodes =
                                    push after <|
                                        push (cmdsToNodesMatch match.name cmds)
                                            controlState.nodes
                            in
                                ( { controlState
                                    | state = state
                                    , nodes = nodes
                                    , tempNodes = nextControlState.tempNodes
                                    , inputHandler = InputHandler ih
                                  }
                                , ""
                                )

                        _ ->
                            let
                                nodes : Stack Node
                                nodes =
                                    push after <|
                                        push (cmdsToNodesMatch match.name cmds)
                                            controlState.nodes
                            in
                                ( { controlState
                                    | state = state
                                    , nodes = nodes
                                    , tempNodes = []
                                  }
                                , ""
                                )


susHandlerTemp :
    String
    -> ControlState
    -> List SusCmd
    -> String
    -> ( ControlState, String )
susHandlerTemp input controlState cmds prefix =
    let
        match : SusState
        match =
            matchingCmd input cmds

        before : Node
        before =
            textNode (Text.color textClr (fromString prefix))
    in
        case match of
            Ambiguous matchedInput ->
                ( { controlState
                    | tempNodes =
                        push (cmdsToNodesAmbiguous matchedInput cmds)
                            [ before ]
                  }
                , matchedInput
                )

            Match match ->
                let
                    state : Stack Action
                    state =
                        match.stateHandler controlState.state
                in
                    case match.next of
                        Just (InputHandler ih) ->
                            --nextControlState : ControlState
                            let
                                ( nextControlState, _ ) =
                                    force ih
                                        ""
                                        { controlState
                                            | state = state
                                        }
                            in
                                ( { controlState
                                    | state = state
                                    , tempNodes = nextControlState.tempNodes
                                    , inputHandler = InputHandler ih
                                  }
                                , ""
                                )

                        _ ->
                            let
                                nodes : Stack Node
                                nodes =
                                    [ cmdsToNodesMatch match.name cmds ]
                            in
                                ( { controlState
                                    | state = state
                                    , tempNodes = nodes
                                  }
                                , ""
                                )


floatHandler :
    String
    -> ControlState
    -> FloatCmd
    -> ( ControlState, String )
floatHandler input controlState cmd =
    let
        handleOtherInput =
            let
                floatInput =
                    filterFloat input

                showInput =
                    if String.isEmpty floatInput then
                        "_"
                    else
                        floatInput
            in
                ( { controlState
                    | tempNodes =
                        [ textNode
                            (append
                                (append
                                    (Text.color textClr <|
                                        fromString cmd.before
                                    )
                                    (Text.color shortTextClr <|
                                        fromString showInput
                                    )
                                )
                                (Text.color textClr <|
                                    fromString cmd.common.textAfter
                                )
                            )
                        ]
                  }
                , floatInput
                )
    in
        case confirmed input of
            Just input ->
                case ( String.toFloat input, cmd.next ) of
                    ( Ok value, InputHandler ih ) ->
                        let
                            state =
                                cmd.stateHandler value controlState.state

                            --nextControlState : ControlState
                            ( nextControlState, _ ) =
                                force ih
                                    ""
                                    { controlState
                                        | state = state
                                    }
                        in
                            ( { controlState
                                | state = state
                                , tempNodes = nextControlState.tempNodes
                                , inputHandler = cmd.next
                              }
                            , ""
                            )

                    _ ->
                        handleOtherInput

            Nothing ->
                handleOtherInput


susText : String -> String -> String -> Text
susText before short after =
    append
        (append
            (Text.color textClr (fromString before))
            (Text.color shortTextClr (fromString short))
        )
        (Text.color textClr (fromString after))


appendValue : Text -> String -> Text
appendValue text value =
    append text
        (Text.color valueClr (fromString value))



-- returns the original string if there is a matching cmd, else empty string


matchingCmd : String -> List SusCmd -> SusState
matchingCmd input options =
    if String.isEmpty input then
        Ambiguous input
    else
        case List.filter (\o -> String.startsWith input o.short) options of
            cmd :: [] ->
                Match cmd

            -- no match
            [] ->
                Ambiguous ""

            -- ambiguous match
            _ ->
                Ambiguous input


extentStr : Extent -> String
extentStr extent =
    case extent of
        Fix x ->
            "fixed " ++ toString x ++ "px"

        Fit ->
            "fit children"

        Fill x ->
            "fill " ++ toString (x * 100) ++ "% of parent"


dirStr : Direction -> String
dirStr dir =
    case dir of
        Up spacing ->
            "up, spacing=" ++ (toString spacing) ++ "px"

        Down spacing ->
            "down, spacing=" ++ (toString spacing) ++ "px"

        Left spacing ->
            "left, spacing=" ++ (toString spacing) ++ "px"

        Right spacing ->
            "right, spacing=" ++ (toString spacing) ++ "px"

        In ->
            "in"

        Out ->
            "out"


bgsRects : List Background -> Node
bgsRects bgs =
    case bgs of
        [] ->
            textNode <| Text.color valueClr <| fromString "none"

        _ ->
            let
                nodes =
                    List.map bgRect bgs
            in
                { nodeType =
                    Rect
                        { rectDef
                            | extents = ( Fit, Fit )
                            , dir = Right 1.0
                            , children = nodes
                        }
                , status = Enabled
                }


bgRect : Background -> Node
bgRect bg =
    { nodeType =
        Rect
            { rectDef
                | extents = ( Fix 18.0, Fix 18.0 )
                , border = Just { thickness = All 1.0, color = Color.black }
                , bgs = [ bg ]
            }
    , status = Enabled
    }


bgStr : Background -> String
bgStr bg =
    case bg of
        Filled color ->
            "filled"

        Textured image ->
            "textured"

        Gradient gradient ->
            "gradient"


setExtent : Float -> Extent -> Extent
setExtent val e =
    case e of
        Fill _ ->
            Fill (val / 100.0)

        Fit ->
            Fit

        Fix _ ->
            Fix val


setSpacing : Float -> Direction -> Direction
setSpacing val d =
    case d of
        Up _ ->
            Up val

        Down _ ->
            Down val

        Left _ ->
            Left val

        Right _ ->
            Right val

        In ->
            In

        Out ->
            Out



-- returns rest of the input if the last input was confirm key


confirmed : String -> Maybe String
confirmed input =
    case List.reverse (String.toList input) of
        ' ' :: rest ->
            Just (String.fromList (List.reverse rest))

        ',' :: rest ->
            Just (String.fromList (List.reverse rest))

        _ ->
            Nothing


cmdsToNodesAmbiguous : String -> List SusCmd -> Node
cmdsToNodesAmbiguous input cmds =
    { nodeType =
        Rect
            { rectDef
                | extents = ( Fit, Fit )
                , dir = Right 0.0
                , children =
                    [ textNode
                        (Text.color textClr
                            (fromString
                                (if String.isEmpty input then
                                    " "
                                 else
                                    input
                                )
                            )
                        )
                    ]
                , relatives = cmdToNodesRelatives input cmds
            }
    , status = Enabled
    }


cmdsToNodesMatch : String -> List SusCmd -> Node
cmdsToNodesMatch input cmds =
    { nodeType =
        Rect
            { rectDef
                | extents = ( Fit, Fit )
                , dir = Right 0.0
                , children =
                    [ textNode
                        (Text.color textClr
                            (fromString
                                (if String.isEmpty input then
                                    " "
                                 else
                                    input
                                )
                            )
                        )
                    ]
            }
    , status = Enabled
    }


cmdToNodesRelatives : String -> List SusCmd -> List ( Node, Sizes )
cmdToNodesRelatives input cmds =
    [ ( { nodeType =
            Rect
                { rectDef
                    | extents = ( Fit, Fit )
                    , dir = Down 0.0
                    , border = Nothing
                    , children =
                        List.map
                            (\cmd ->
                                { nodeType =
                                    Rect
                                        { rectDef
                                            | extents = ( Fit, Fit )
                                            , dir = Right 0.0
                                            , bgs = [ Filled (Color.rgba 255 255 255 0.9) ]
                                            , border =
                                                Just
                                                    { thickness = TRBL 0.0 0.0 3.0 0.0
                                                    , color = relBsClr
                                                    }
                                            , children = cmd.nodes
                                        }
                                , status =
                                    if String.startsWith input cmd.short then
                                        Enabled
                                    else
                                        Disabled
                                }
                            )
                            cmds
                }
        , status = Enabled
        }
      , ( 0.0, 0.0 )
      )
    ]


createCmds : InputHandler
createCmds =
    InputHandler
        (lazy
            (\() ->
                \input controlState ->
                    let
                        cmds : List SusCmd
                        cmds =
                            [ { name = "rectangle"
                              , nodes = [ textNode <| susText "" "r" "ectangle" ]
                              , short = "r"
                              , stateHandler =
                                    push
                                        (RectangleAction
                                            --TODO remove bgs
                                            { rectDef | bgs = [ Filled Color.red, Filled Color.blue ] }
                                        )
                              , next = Just rectangleCmds
                              , common =
                                    { textAfter = " with "
                                    }
                              }
                            , { name = "text"
                              , nodes = [ textNode <| susText "" "t" "ext" ]
                              , short = "t"
                              , stateHandler = identity
                              , next = Nothing
                              , common =
                                    { textAfter = " "
                                    }
                              }
                            ]
                    in
                        susHandler input controlState cmds
            )
        )


rectangleCmds : InputHandler
rectangleCmds =
    InputHandler
        (lazy
            (\() ->
                \input controlState ->
                    let
                        ( lastState, stateTail ) =
                            pop controlState.state

                        cmds : List SusCmd
                        cmds =
                            [ { name = "width"
                              , nodes =
                                    [ textNode <|
                                        case lastState of
                                            Just (RectangleAction rectDef) ->
                                                appendValue (susText "" "w" "idth=") <|
                                                    extentStr <|
                                                        fst rectDef.extents

                                            _ ->
                                                susText "" "w" "idth"
                                    ]
                              , short = "w"
                              , stateHandler = identity
                              , next = Just widthTypeCmds
                              , common =
                                    { textAfter = "="
                                    }
                              }
                            , { name = "height"
                              , nodes =
                                    [ textNode <|
                                        case lastState of
                                            Just (RectangleAction rectDef) ->
                                                appendValue (susText "" "h" "eight=") <|
                                                    extentStr <|
                                                        snd rectDef.extents

                                            _ ->
                                                susText "" "h" "eight"
                                    ]
                              , short = "h"
                              , stateHandler = identity
                              , next = Just heightTypeCmds
                              , common =
                                    { textAfter = " "
                                    }
                              }
                            , { name = "direction"
                              , nodes =
                                    [ textNode <|
                                        case lastState of
                                            Just (RectangleAction rectDef) ->
                                                appendValue (susText "" "d" "irection of children=") <|
                                                    dirStr <|
                                                        rectDef.dir

                                            _ ->
                                                susText "" "d" "irection of children"
                                    ]
                              , short = "d"
                              , stateHandler = identity
                              , next = Just directionCmds
                              , common =
                                    { textAfter = " "
                                    }
                              }
                            , { name = "backgrounds"
                              , nodes =
                                    case lastState of
                                        Just (RectangleAction rectDef) ->
                                            [ textNode <| susText "" "b" "ackgrounds="
                                            , bgsRects <| rectDef.bgs
                                            ]

                                        _ ->
                                            [ textNode <| susText "" "b" "ackgrounds" ]
                              , short = "b"
                              , stateHandler = identity
                              , next = Just bgsCmds
                              , common =
                                    { textAfter = " "
                                    }
                              }
                            ]
                    in
                        susHandlerTemp input controlState cmds ""
            )
        )


widthTypeCmds : InputHandler
widthTypeCmds =
    InputHandler
        (lazy
            (\() ->
                \input controlState ->
                    let
                        cmds : List SusCmd
                        cmds =
                            [ { name = "fixed"
                              , nodes = [ textNode <| susText "fi" "x" "ed" ]
                              , short = "x"
                              , stateHandler =
                                    \state ->
                                        let
                                            ( last, stateTail ) =
                                                pop state
                                        in
                                            case last of
                                                Just (RectangleAction rectDef) ->
                                                    push
                                                        (RectangleAction
                                                            { rectDef
                                                                | extents = ( Fix 0.0, snd rectDef.extents )
                                                            }
                                                        )
                                                        stateTail

                                                _ ->
                                                    state
                              , next = Just (widthCmd "px" "fixed ")
                              , common =
                                    { textAfter = " "
                                    }
                              }
                            , { name = "fit"
                              , nodes = [ textNode <| susText "fi" "t" " children" ]
                              , short = "t"
                              , stateHandler =
                                    \state ->
                                        let
                                            ( last, stateTail ) =
                                                pop state
                                        in
                                            case last of
                                                Just (RectangleAction rectDef) ->
                                                    push
                                                        (RectangleAction
                                                            { rectDef
                                                                | extents = ( Fit, snd rectDef.extents )
                                                            }
                                                        )
                                                        stateTail

                                                _ ->
                                                    state
                              , next = Just rectangleCmds
                              , common =
                                    { textAfter = " "
                                    }
                              }
                            , { name = "fill"
                              , nodes = [ textNode <| susText "fi" "l" "l parent" ]
                              , short = "l"
                              , stateHandler =
                                    \state ->
                                        let
                                            ( last, stateTail ) =
                                                pop state
                                        in
                                            case last of
                                                Just (RectangleAction rectDef) ->
                                                    push
                                                        (RectangleAction
                                                            { rectDef
                                                                | extents = ( Fill 0.0, snd rectDef.extents )
                                                            }
                                                        )
                                                        stateTail

                                                _ ->
                                                    state
                              , next = Just (widthCmd "% of parent" "fill ")
                              , common =
                                    { textAfter = " "
                                    }
                              }
                            ]
                    in
                        susHandlerTemp input controlState cmds "width="
            )
        )


widthCmd : String -> String -> InputHandler
widthCmd unit extentType =
    InputHandler
        (lazy
            (\() ->
                \input controlState ->
                    let
                        cmd =
                            { next = rectangleCmds
                            , stateHandler =
                                \value state ->
                                    let
                                        ( last, stateTail ) =
                                            pop state
                                    in
                                        case last of
                                            Just (RectangleAction rectDef) ->
                                                push
                                                    (RectangleAction
                                                        { rectDef
                                                            | extents =
                                                                ( setExtent value <|
                                                                    fst rectDef.extents
                                                                , snd rectDef.extents
                                                                )
                                                        }
                                                    )
                                                    stateTail

                                            _ ->
                                                state
                            , before = "width=" ++ extentType
                            , common = { textAfter = unit }
                            }
                    in
                        floatHandler input controlState cmd
            )
        )


heightTypeCmds : InputHandler
heightTypeCmds =
    InputHandler
        (lazy
            (\() ->
                \input controlState ->
                    let
                        cmds : List SusCmd
                        cmds =
                            [ { name = "fixed"
                              , nodes = [ textNode <| susText "fi" "x" "ed" ]
                              , short = "x"
                              , stateHandler =
                                    \state ->
                                        let
                                            ( last, stateTail ) =
                                                pop state
                                        in
                                            case last of
                                                Just (RectangleAction rectDef) ->
                                                    push
                                                        (RectangleAction
                                                            { rectDef
                                                                | extents = ( fst rectDef.extents, Fix 0.0 )
                                                            }
                                                        )
                                                        stateTail

                                                _ ->
                                                    state
                              , next = Just (heightCmd "px" "fixed")
                              , common =
                                    { textAfter = " "
                                    }
                              }
                            , { name = "fit"
                              , nodes = [ textNode <| susText "fi" "t" " children" ]
                              , short = "t"
                              , stateHandler =
                                    \state ->
                                        let
                                            ( last, stateTail ) =
                                                pop state
                                        in
                                            case last of
                                                Just (RectangleAction rectDef) ->
                                                    push
                                                        (RectangleAction
                                                            { rectDef
                                                                | extents = ( fst rectDef.extents, Fit )
                                                            }
                                                        )
                                                        stateTail

                                                _ ->
                                                    state
                              , next = Just rectangleCmds
                              , common =
                                    { textAfter = " "
                                    }
                              }
                            , { name = "fill"
                              , nodes = [ textNode <| susText "fi" "l" "l parent" ]
                              , short = "l"
                              , stateHandler =
                                    \state ->
                                        let
                                            ( last, stateTail ) =
                                                pop state
                                        in
                                            case last of
                                                Just (RectangleAction rectDef) ->
                                                    push
                                                        (RectangleAction
                                                            { rectDef
                                                                | extents = ( fst rectDef.extents, Fill 0.0 )
                                                            }
                                                        )
                                                        stateTail

                                                _ ->
                                                    state
                              , next = Just (heightCmd "% of parent" "fill")
                              , common =
                                    { textAfter = " "
                                    }
                              }
                            ]
                    in
                        susHandlerTemp input controlState cmds "height="
            )
        )


heightCmd : String -> String -> InputHandler
heightCmd unit extentType =
    InputHandler
        (lazy
            (\() ->
                \input controlState ->
                    let
                        cmd =
                            { next = rectangleCmds
                            , stateHandler =
                                \value state ->
                                    let
                                        ( last, stateTail ) =
                                            pop state
                                    in
                                        case last of
                                            Just (RectangleAction rectDef) ->
                                                push
                                                    (RectangleAction
                                                        { rectDef
                                                            | extents =
                                                                ( fst rectDef.extents
                                                                , setExtent value <|
                                                                    snd rectDef.extents
                                                                )
                                                        }
                                                    )
                                                    stateTail

                                            _ ->
                                                state
                            , before = "height=" ++ extentType ++ " "
                            , common = { textAfter = unit }
                            }
                    in
                        floatHandler input controlState cmd
            )
        )


directionCmds : InputHandler
directionCmds =
    InputHandler
        (lazy
            (\() ->
                \input controlState ->
                    let
                        cmds : List SusCmd
                        cmds =
                            [ { name = "up"
                              , nodes = [ textNode <| susText "" "u" "p" ]
                              , short = "u"
                              , stateHandler =
                                    \state ->
                                        let
                                            ( last, stateTail ) =
                                                pop state
                                        in
                                            case last of
                                                Just (RectangleAction rectDef) ->
                                                    push
                                                        (RectangleAction
                                                            { rectDef
                                                                | dir = Up 0.0
                                                            }
                                                        )
                                                        stateTail

                                                _ ->
                                                    state
                              , next = Just (spacingCmd "up")
                              , common =
                                    { textAfter = " "
                                    }
                              }
                            , { name = "down"
                              , nodes = [ textNode <| susText "" "d" "own" ]
                              , short = "d"
                              , stateHandler =
                                    \state ->
                                        let
                                            ( last, stateTail ) =
                                                pop state
                                        in
                                            case last of
                                                Just (RectangleAction rectDef) ->
                                                    push
                                                        (RectangleAction
                                                            { rectDef
                                                                | dir = Down 0.0
                                                            }
                                                        )
                                                        stateTail

                                                _ ->
                                                    state
                              , next = Just (spacingCmd "down")
                              , common =
                                    { textAfter = " "
                                    }
                              }
                            , { name = "left"
                              , nodes = [ textNode <| susText "" "l" "eft" ]
                              , short = "l"
                              , stateHandler =
                                    \state ->
                                        let
                                            ( last, stateTail ) =
                                                pop state
                                        in
                                            case last of
                                                Just (RectangleAction rectDef) ->
                                                    push
                                                        (RectangleAction
                                                            { rectDef
                                                                | dir = Left 0.0
                                                            }
                                                        )
                                                        stateTail

                                                _ ->
                                                    state
                              , next = Just (spacingCmd "left")
                              , common =
                                    { textAfter = " "
                                    }
                              }
                            , { name = "right"
                              , nodes = [ textNode <| susText "" "r" "ight" ]
                              , short = "r"
                              , stateHandler =
                                    \state ->
                                        let
                                            ( last, stateTail ) =
                                                pop state
                                        in
                                            case last of
                                                Just (RectangleAction rectDef) ->
                                                    push
                                                        (RectangleAction
                                                            { rectDef
                                                                | dir = Right 0.0
                                                            }
                                                        )
                                                        stateTail

                                                _ ->
                                                    state
                              , next = Just (spacingCmd "right")
                              , common =
                                    { textAfter = " "
                                    }
                              }
                            , { name = "in"
                              , nodes = [ textNode <| susText "" "i" "n" ]
                              , short = "i"
                              , stateHandler =
                                    \state ->
                                        let
                                            ( last, stateTail ) =
                                                pop state
                                        in
                                            case last of
                                                Just (RectangleAction rectDef) ->
                                                    push
                                                        (RectangleAction
                                                            { rectDef
                                                                | dir = In
                                                            }
                                                        )
                                                        stateTail

                                                _ ->
                                                    state
                              , next = Just rectangleCmds
                              , common =
                                    { textAfter = " "
                                    }
                              }
                            , { name = "out"
                              , nodes = [ textNode <| susText "" "o" "ut" ]
                              , short = "o"
                              , stateHandler =
                                    \state ->
                                        let
                                            ( last, stateTail ) =
                                                pop state
                                        in
                                            case last of
                                                Just (RectangleAction rectDef) ->
                                                    push
                                                        (RectangleAction
                                                            { rectDef
                                                                | dir = Out
                                                            }
                                                        )
                                                        stateTail

                                                _ ->
                                                    state
                              , next = Just rectangleCmds
                              , common =
                                    { textAfter = " "
                                    }
                              }
                            ]
                    in
                        susHandlerTemp input controlState cmds "direction="
            )
        )


spacingCmd : String -> InputHandler
spacingCmd dirType =
    InputHandler
        (lazy
            (\() ->
                \input controlState ->
                    let
                        cmd =
                            { next = rectangleCmds
                            , stateHandler =
                                \value state ->
                                    let
                                        ( last, stateTail ) =
                                            pop state
                                    in
                                        case last of
                                            Just (RectangleAction rectDef) ->
                                                push
                                                    (RectangleAction
                                                        { rectDef
                                                            | dir = setSpacing value rectDef.dir
                                                        }
                                                    )
                                                    stateTail

                                            _ ->
                                                state
                            , before = "direction=" ++ dirType ++ ", spacing="
                            , common = { textAfter = "px" }
                            }
                    in
                        floatHandler input controlState cmd
            )
        )



{-
   - colors in left column with selection and j/k to move
   - actions in right column with append/change/delete/insert
-}
--TODO remove SelectionAction after finished


bgsCmds : InputHandler
bgsCmds =
    InputHandler
        (lazy
            (\() ->
                \input controlState ->
                    let
                        ( lastState, stateTail ) =
                            pop controlState.state

                        ( lastState', stateTail' ) =
                            pop stateTail

                        process :
                            RectDef
                            -> Int
                            -> Stack Action
                            -> ( Stack Node, Stack Action )
                        process rectDef selection otherStates =
                            let
                                bgNodes : List Node
                                bgNodes =
                                    List.map bgRect rectDef.bgs

                                bgs : Node
                                bgs =
                                    { nodeType =
                                        Rect
                                            { rectDef
                                                | extents = ( Fit, Fit )
                                                , dir = Right 0.0
                                                , relatives =
                                                    [ ( { nodeType =
                                                            Rect
                                                                { rectDef
                                                                    | extents = ( Fit, Fit )
                                                                    , dir = Down 0.0
                                                                    , border = Nothing
                                                                    , children =
                                                                        List.map
                                                                            (\bgNode ->
                                                                                { nodeType =
                                                                                    Rect
                                                                                        { rectDef
                                                                                            | extents = ( Fit, Fit )
                                                                                            , dir = Right 0.0
                                                                                            , bgs =
                                                                                                [ Filled
                                                                                                    (Color.rgba 255 255 255 0.9)
                                                                                                ]
                                                                                            , border =
                                                                                                Just
                                                                                                    { thickness = TRBL 0.0 0.0 3.0 0.0
                                                                                                    , color = relBsClr
                                                                                                    }
                                                                                                -- TODO show controls j/k
                                                                                            , children = [ bgNode ]
                                                                                        }
                                                                                , status = Enabled
                                                                                }
                                                                            )
                                                                            bgNodes
                                                                }
                                                        , status = Enabled
                                                        }
                                                      , ( 0.0, 0.0 )
                                                      )
                                                    ]
                                            }
                                    , status = Enabled
                                    }
                            in
                                ( Stack.fromList
                                    [ textNode <|
                                        Text.color textClr <|
                                            fromString "backgrounds="
                                    , bgs
                                    ]
                                , push (SelectionAction selection)
                                    (push (RectangleAction rectDef) otherStates)
                                )

                        --(Stack Node, Stack Action)
                        ( tempNodes, state ) =
                            case ( lastState, lastState' ) of
                                ( Just (RectangleAction rectDef), _ ) ->
                                    -- there's no selection - select first
                                    process rectDef 0 stateTail

                                ( Just (SelectionAction selection), Just (RectangleAction rectDef) ) ->
                                    process rectDef selection stateTail'

                                _ ->
                                    ( controlState.tempNodes, controlState.state )
                    in
                        ( { controlState
                            | state = state
                            , tempNodes = tempNodes
                          }
                        , ""
                        )
            )
        )
