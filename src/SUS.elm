{-
Shortest unique substring
-}
module SUS
    ( susDefToNode
    ) where

import Node                 exposing ( .. )

import Color                exposing ( .. )
import Graphics.Input.Field exposing ( Content )
import String               exposing ( left )
import Text                 exposing ( append, fromString )

susDefToNode : SUSDef -> Node
susDefToNode def =
    let susOptions : List ( String, ( String, String ) )
        susOptions = findShortestUniqueSubstrings def.options
        shortOptions : List String 
        shortOptions = List.map ( \( _, ( short, _ ) ) -> short ) susOptions
        relatives : List ( Node, Sizes )
        relatives = [ 
            ( { nodeType = Rect
                { rectDef
                | extents = ( Fix 200.0, Fit )
                , dir = Down 0.0
                , border = Nothing
                , children =
                    List.map ( \( option, ( short, afterShort ) ) ->
                        { nodeType = Rect
                            { rectDef
                            | extents = ( Fix 200.0, Fix 25.0 )
                            , dir = Right 0.0
                            , bgs =
                                [ Signal.map ( highlightMatch short ) 
                                    def.content ]
                            , children =
                                [ { nodeType = Text 
                                    { text = append 
                                        ( Text.color red ( fromString short ) )
                                        ( fromString afterShort )
                                    } 
                                } ]
                            } 
                        }
                    ) susOptions
                }
            }, ( 0.0, 25.0 ) ) ]
    in  { nodeType = Rect
            { rectDef
            | extents = ( Fix 200.0, Fix 25.0 ) 
            , children =
                [
                    { nodeType = InputText 
                        { name = def.name
                        , handler = Signal.message ( Signal.forwardTo 
                            def.address ( matchSus shortOptions ) )
                        , content = def.content }
                    }
                ]
            , relatives = relatives
            }
        }

-- INTERNAL

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
