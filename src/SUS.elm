{-
Shortest unique substring
-}
module SUS
    ( SusState (.. )
    , susDefToNode
    ) where

import Node                 exposing ( .. )

import Color                exposing ( .. )
import Graphics.Input.Field exposing ( Content )
import String               exposing ( left )
import Text                 exposing ( append, fromString )

type SusState
    = Ambiguous
    | Match String

susDefToNode : SUSDef -> Node
susDefToNode def =
    let susOptions : List ( String, ( String, String ) )
        susOptions = findShortestUniqueSubstrings def.options
        shortOptions : List String 
        shortOptions = List.map ( \( _, ( short, _ ) ) -> short ) susOptions
        mc : SusState
        mc = matchCount def.content def.options
        content = def.content
        replacedContent : Content
        replacedContent = case mc of
            Ambiguous -> content
            Match x -> { content | string = x }
        optionsStatus : NodeStatus
        optionsStatus = case mc of
            Ambiguous -> Enabled
            Match _ -> Disabled
        relatives : List ( Node, Sizes )
        relatives = [ 
            ( { nodeType = Rect
                { rectDef
                | extents = ( fst def.extents, Fit ) 
                , dir = Down 0.0
                , border = Nothing
                , children =
                    List.map ( \( option, ( short, afterShort ) ) ->
                        { nodeType = Rect
                            { rectDef
                            | extents = def.extents
                            , dir = Right 0.0
                            , bgs = [ highlightMatch short def.content ]
                            , children =
                                [ { nodeType = Text 
                                    { text = append 
                                        ( Text.color red ( fromString short ) )
                                        ( fromString afterShort )
                                    } 
                                , status = Enabled
                                } ]
                            } 
                        , status = optionsStatus
                        }
                    ) susOptions
                }
            , status = Enabled
            }, ( 0.0, 25.0 ) ) ]
    in  { nodeType = Rect
            { rectDef
            | extents = def.extents 
            , children =
                [
                    { nodeType = InputText 
                        { name = def.name
                        , handler = Signal.message ( Signal.forwardTo 
                            def.address ( matchSus shortOptions ) )
                        , content = replacedContent
                        }
                    , status = Enabled
                    }
                ]
            , relatives = relatives
            }
        , status = Enabled
        }

-- INTERNAL

matchCount : Content -> List String -> SusState
matchCount c options =
    if String.isEmpty c.string
    then Ambiguous
    else case List.filter ( String.startsWith c.string ) options of
        x :: [ ] -> Match x
        _ -> Ambiguous

matchSus : List String -> Content -> Content
matchSus shortOptions content = 
    let matching = List.any ( \short -> 
            String.startsWith content.string short ) shortOptions
        str = if matching then content.string else ""
    in  { content | string = str }

highlightMatch : String -> Content -> Background
highlightMatch short content =
    if not ( String.isEmpty content.string ) && 
        ( String.startsWith content.string short )
    then Filled green
    else Filled white

enabledMatch : String -> Content -> NodeStatus
enabledMatch short content =
    if ( String.isEmpty content.string ) ||
        ( String.startsWith content.string short )
    then Enabled
    else Disabled

findShortestUniqueSubstrings : List String 
   -> List ( String, ( String, String ) )
findShortestUniqueSubstrings os = List.map ( \o -> 
        let otherOs = List.filter ( ( /= ) o ) os
        in  findShortestUniqueSubstring 1 otherOs o ) os

findShortestUniqueSubstring : Int -> List String -> String 
   -> ( String, ( String, String ) )
findShortestUniqueSubstring chars os option =
    let short = String.left chars option
        isUnique = List.all ( \o -> String.left chars o /= short ) os
        afterShort = String.right ( String.length option - chars ) option
    in  if isUnique 
        then ( option, ( short, afterShort ) )
        else findShortestUniqueSubstring ( chars + 1 ) os option
