import InputHandler exposing ( .. )
import Node         exposing ( .. )
import Render       exposing ( .. )
import SUS          exposing ( .. )

import Char                 exposing ( fromCode )
import Color                exposing ( .. )
import Graphics.Input.Field exposing ( Content, noContent )
import Keyboard
import Signal               exposing ( .. )
import String               exposing ( left )
import Text                 exposing ( .. )

suss : List String 
   -> List ( String, ( String, String ) )
suss os = List.map ( \o -> 
        let otherOs = List.filter ( ( /= ) o ) os
        in  sus 1 otherOs o ) os

sus : Int -> List String -> String 
   -> ( String, ( String, String ) )
sus chars os option =
    let short = String.left chars option
        isUnique = List.all ( \o -> String.left chars o /= short ) os
        afterShort = String.right ( String.length option - chars ) option
    in  if isUnique 
        then ( option, ( short, afterShort ) )
        else sus ( chars + 1 ) os option

shortOptions : List ( String, ( String, String ) ) -> List String 
shortOptions opts = List.map ( \( _, ( short, _ ) ) -> short ) opts

type ActionOption
    = ChangeCmd
    | DeleteCmd
    | InsertCmd ( Maybe CreateOption )
    | AppendCmd ( Maybe CreateOption )

type CreateOption
    = RectangleCmd ( Maybe RectangleOption )
    | TextCmd

type alias RectangleOption =
    { extents : Maybe ExtentsOption
    , dir : Maybe String
    }

type alias ExtentsOption =
    { w : String
    , h : String
    }

actionOpts = [ "change", "delete", "insert", "append" ]
createOpts = [ "rectangle", "text" ]
rectangleOpts = [ "extents", "direction", "border", "backgrounds" ]
extentOpts = [ "fix", "fit", "fill" ]

actionOptions : List ( String, ( String, String ) )
actionOptions = suss actionOpts
createOptions = suss createOpts
rectangleOptions = suss rectangleOpts

shortActionOpts : List String
shortActionOpts = shortOptions actionOptions
shortCreateOpts = shortOptions createOptions
shortRectangleOpts = shortOptions rectangleOptions

matchSus : List String -> String -> String
matchSus shortOptions string = 
    let matching = List.any ( \short -> 
            String.startsWith string short ) shortOptions
        str = if matching then string else ""
    in  str

matchCount : String -> List String -> SusState
matchCount str options =
    if String.isEmpty str
    then Ambiguous
    else case List.filter ( String.startsWith str ) options of
        x :: [ ] -> Match x
        _ -> Ambiguous

scene = Signal.map ( \textScene ->
    { nodeType = Rect
        { rectDef
        | extents = ( Fill 1.0, Fill 1.0 )
        , dir = Down 0.0
        , children = [
            textScene
            ]
        }
    , status = Enabled 
    } ) textScenes

actionNode cmd = 
    { nodeType = Rect
        { rectDef
        | extents = ( Fit, Fit )
        , dir = Right 0.0
        , children = 
        [
            { nodeType = Text 
                <| textDef 
                <| if String.isEmpty cmd then " " else cmd
            , status = Enabled 
            }
            , space
        ]
        , relatives = relatives actionOptions Enabled
        }
    , status = Enabled
    } 

textNode text =
    { nodeType = Rect
        { rectDef
        | extents = ( Fit, Fit )
        , dir = Right 0.0
        , children = 
        [
            { nodeType = Text 
                <| textDef 
                <| text
            , status = Enabled 
            }
        ]
        }
    , status = Enabled
    } 

createNode cmd =
    { nodeType = Rect
        { rectDef
        | extents = ( Fit, Fit )
        , dir = Right 0.0
        , children = 
        [
            { nodeType = Text 
                <| textDef cmd
            , status = Enabled 
            }
        ]
        , relatives = relatives createOptions Enabled
        }
    , status = Enabled
    } 

rectangleNode cmd =
    { nodeType = Rect
        { rectDef
        | extents = ( Fit, Fit )
        , dir = Right 0.0
        , children = 
        [
            { nodeType = Text 
                <| textDef cmd
            , status = Enabled 
            }
        ]
        , relatives = relatives rectangleOptions Enabled
        }
    , status = Enabled
    } 

commands : Signal ( Maybe ActionOption, String )
commands = Signal.foldp ( \press ( state, ps ) -> 
        let toString shortOpts = press :: ps 
                |> List.map Char.fromCode 
                |> List.reverse
                |> String.fromList
                |> matchSus shortOpts
            fromString str = String.toList str
                |> List.map Char.toCode
            cs = case state of
                Nothing -> 
                    let str = toString shortActionOpts
                        mc = matchCount str actionOpts
                        chars = fromString str
                    in  case mc of
                            Ambiguous -> ( state, chars )
                            Match "insert" -> 
                                ( Just ( InsertCmd Nothing ), [ ] )
                            Match "append" -> 
                                ( Just ( AppendCmd Nothing ), [ ] )
                            Match _ -> ( state, chars )
                Just ChangeCmd -> ( Nothing, [ ] )
                Just DeleteCmd -> ( Nothing, [ ] )
                Just ( InsertCmd s ) ->
                    let ( cState, cChars ) = create s
                    in  ( Just ( InsertCmd cState ), cChars )
                Just ( AppendCmd s ) -> 
                    let ( cState, cChars ) = create s
                    in  ( Just ( InsertCmd cState ), cChars )
            create s = case s of
                Nothing ->
                    let str = toString shortCreateOpts
                        mc = matchCount str createOpts
                        chars = fromString str
                    in  case mc of
                            Ambiguous -> ( Nothing, chars )
                            Match "rectangle" -> 
                                ( Just ( RectangleCmd Nothing ), [ ] )
                            Match _ -> ( Nothing, chars )
                Just ( RectangleCmd r ) -> ( Nothing, [] )
                Just ( TextCmd ) -> ( Nothing, [] )
            rectangle s = case s of
                Nothing ->
                    let str = toString shortCreateOpts
                        mc = matchCount str createOpts
                        chars = fromString str
                    in  case mc of
                            Ambiguous -> ( Nothing, chars )
                            Match "extents" -> 
                                ( Just ( 
                                    { extents = Nothing
                                    , dir = Nothing } 
                                ), [ ] )
                            Match _ -> ( Nothing, chars )
                _ -> ( Nothing, [] )
        in  cs
    ) ( Nothing, [ ] ) Keyboard.presses
        |> Signal.map ( \( state, ps ) -> 
            ( state, String.fromList ( List.map Char.fromCode ps ) ) )

textScenes = Signal.map ( \( state, cmd ) ->
    let cs = case state of
            Nothing -> [ actionNode cmd ]
            Just ( InsertCmd s ) -> textNode "insert " :: create s
            Just ( AppendCmd s ) -> textNode "append " :: create s
            Just s -> []
        create s = case s of
            Nothing -> [ createNode cmd ]
            Just ( RectangleCmd r ) -> textNode "rectangle " :: rectangle r
            Just ( TextCmd ) -> []
        rectangle s = case s of
            Nothing -> [ rectangleNode cmd ]
            --TODO give different relatives depending on what is already picked
            Just r -> ( case r.extents of
                    Nothing -> []
                    Just e -> [ textNode "extents " ] ) ++ -- exclude extents
                ( case r.dir of
                    Nothing -> []
                    Just d -> [ textNode d ] )
    in  { nodeType = Rect
            { rectDef
            | extents = ( Fill 1.0, Fix 25.0 )
            , dir = Right 0.0
            , border = Just { thickness = All 3.0, color = grey }
            , children = cs
            }
        , status = Enabled
        } ) commands

space = { nodeType = Text <| textDef " ", status = Enabled }

relatives : List ( String, ( String, String ) ) -> NodeStatus 
   -> List ( Node, Sizes )
relatives options status = [ 
    ( { nodeType = Rect
        { rectDef
        | extents = ( Fit, Fit ) 
        , dir = Down 0.0
        , border = Nothing
        , children =
            List.map ( \( option, ( short, afterShort ) ) ->
                { nodeType = Rect
                    { rectDef
                    | extents = ( Fit, Fit )
                    , dir = Right 0.0
                    , border = Just { thickness = TRBL 0.0 0.0 3.0 0.0
                        , color = relBsClr }
                    , children =
                        [ { nodeType = Text 
                            { text = append 
                                ( Text.color shortClr ( fromString short ) )
                                ( Text.color afterShortClr 
                                    ( fromString afterShort ) )
                            } 
                        , status = Enabled
                        } ]
                    } 
                , status = Enabled
                }
            ) options
        }
    , status = status
    }, ( 0.0, 0.0 ) ) ]

shortClr        = rgb 000 000 000
afterShortClr   = rgb 125 125 125
relBsClr        = rgb 225 225 225
bsClr           = rgb 000 000 000

main = render scene
{-
TODO
 * fix relatives and popups order
 * make border list of Inner | Outer | Middle
-}
