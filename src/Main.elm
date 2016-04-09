import InputHandler exposing ( .. )
import Node         exposing ( .. )
import Render       exposing ( .. )
import SUS          exposing ( .. )

import Color                exposing ( .. )
import Graphics.Input.Field exposing ( Content, noContent )
import Keyboard
import Signal               exposing ( .. )
import String               exposing ( left )
import Text                 exposing ( .. )

actionMB : Mailbox Content
actionMB = mailbox noContent
actionOpts = [ "change", "delete", "insert" ]

insertTypeMB : Mailbox Content
insertTypeMB = mailbox noContent
insertTypeOpts = [ "rectangle", "text" ]

rectDefMB : Mailbox Content
rectDefMB = mailbox noContent
rectDefOpts = [ "extents", "direction", "border", "backgrounds" ]

fstExtentMB : Mailbox Content
fstExtentMB = mailbox noContent
sndExtentMB : Mailbox Content
sndExtentMB = mailbox noContent
extentOpts = [ "fix", "fit", "fill" ]

sizeMB : Mailbox Content
sizeMB = mailbox noContent

matchCounts : Signal Content -> List String -> Signal SusState
matchCounts c options = Signal.map ( \c ->
    if String.isEmpty c.string
    then Ambiguous
    else case List.filter ( String.startsWith c.string ) options of
        x :: [ ] -> Match x
        _ -> Ambiguous
    ) c

actionMatchCounts = matchCounts actionMB.signal actionOpts
insertTypes : Signal ( List Node )
insertTypes = Signal.map2 ( \mc insertType -> case mc of
        Ambiguous -> [ ]
        Match "insert" -> 
            [ 
                { nodeType = SUS
                    { name = "node type"
                    , extents = ( Fix 80.0, Fix 25.0 )
                    , address = insertTypeMB.address
                    , content = insertType
                    , options = insertTypeOpts
                    }
                , status = Enabled
                }
            ] 
        _ -> [ ]
    ) actionMatchCounts insertTypeMB.signal

insertMatchCounts = matchCounts insertTypeMB.signal insertTypeOpts
defs : Signal ( List Node )
defs = Signal.map2 ( \mc rectDef -> case mc of
        Ambiguous -> [ ]
        Match "rectangle" ->
            [
                { nodeType = SUS
                    { name = "rectangle"
                    , extents = ( Fix 100.0, Fix 25.0 )
                    , address = rectDefMB.address
                    , content = rectDef
                    , options = rectDefOpts
                    }
                , status = Enabled
                }
            ]
        _ -> [ ]
    ) insertMatchCounts rectDefMB.signal

defMatchCounts = matchCounts rectDefMB.signal rectDefOpts
defValues : Signal ( List Node )
defValues = Signal.map3 ( \mc fstExtent sndExtent -> case mc of
        Ambiguous -> [ ]
        Match "extents" ->
            [
                { nodeType = Text ( textDef "(" )
                , status = Enabled }
                , { nodeType = SUS
                    { name = "width"
                    , extents = ( Fix 40.0, Fix 25.0 )
                    , address = fstExtentMB.address
                    , content = fstExtent
                    , options = extentOpts
                    }
                , status = Enabled
                }
                    {-, { nodeType = SUS
                    { name = "snd"
                    , extents = ( Fix 100.0, Fix 25.0 )
                    , address = sndExtentMB.address
                    , content = sndExtent
                    , options = extentOpts
                    }
                , status = Enabled
                }-}
            ]
        _ -> [ ]
    ) defMatchCounts fstExtentMB.signal sndExtentMB.signal

fstExtentMatchCounts = matchCounts fstExtentMB.signal extentOpts
extentInputs : Signal ( List Node )
extentInputs = Signal.map2 ( \mc size -> case mc of
        Ambiguous -> [ ]
        Match "fix" -> 
            [
                { nodeType = Rect
                    { rectDef
                    | extents = ( Fix 40.0, Fix 25.0 )
                    , children = [
                        { nodeType = InputText
                            { name = "size"
                            , handler = handleFloat sizeMB.address
                            , content = size
                            }
                        , status = Enabled }
                    ] }
                , status = Enabled }
            ]
        _ -> [ ]
    ) fstExtentMatchCounts sizeMB.signal

sus = Signal.map3 ( \scene action node -> 
    { nodeType = Rect
        { rectDef
        | extents = ( Fill 1.0, Fill 1.0 )
        , border = Nothing
        , children = [
            { nodeType = Rect
                { rectDef
                | extents = ( Fill 1.0, Fix 27.0 )
                , dir = Right 0.0
                , children = [
                    { nodeType = SUS
                        { name = "action"
                        , extents = ( Fix 60.0, Fix 25.0 )
                        , address = actionMB.address
                        , content = action
                        , options = actionOpts
                        }
                    , status = Enabled
                    } ] ++ node
                }
            , status = Enabled
            }
            , scene ]
        }
    , status = Enabled
    }
    ) scenes actionMB.signal nodes
        
nodes : Signal ( List Node )
nodes = insertTypes +++ defs +++ defValues +++ extentInputs

{-
nodes = mergeNodes insertTypes defs
    << mergeNodes defValues
    <| mergeNodes extentInputs
-}

( +++ ) : Signal ( List Node ) -> Signal ( List Node ) 
   -> Signal ( List Node )
( +++ ) xs ys = Signal.map2 ( \x y -> x ++ y ) xs ys

scenes = Signal.map ( \action ->
    { nodeType = Rect
        { extents = ( Fill 1.0, Fill 1.0 )
        , dir = Down 0.0
        , border = Nothing
        , bgs = [ ]
        , children = [ ]
        , popups = [ ]
        , relatives = [ ]
    }
    , status = Enabled
    } ) actionMB.signal

main = render sus
{-
TODO
 * fix relatives and popups order
 * make border list of Inner | Outer | Middle
-}
