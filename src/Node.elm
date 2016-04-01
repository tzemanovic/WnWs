module Node
    ( Node
    , NodeType      ( .. )
    , RectDef
    , TextDef
    , InputTextDef
    , SUSDef
    , Size
    , Sizes
    , ISize
    , ISizes
    , Ratio
    , Spacing
    , Direction     ( .. )
    , Extent        ( .. )
    , Align         ( .. )
    , DirProp       ( .. )
    , BorderStyle
    , Background    ( .. )
    , Which
    , rectDef
    , textDef
    , extentIsFill
    , extentIsFix
    , fixSize
    , borderSize
    ) where

import Color                exposing ( Color, Gradient )
import Graphics.Input.Field exposing ( Content )
import Maybe                exposing ( map, withDefault )
import Signal               exposing ( Mailbox, Message )
import Text                 exposing ( Text, fromString )

-- NODE TYPES

type alias Node =
    { nodeType : NodeType
    }

type NodeType 
    = Rect RectDef
    | Text TextDef
    | InputText InputTextDef
    | SUS SUSDef

type alias RectDef =
    { extents : ( Extent, Extent )
    , dir : Direction
    -- inner border
    , border : Maybe BorderStyle
    , bgs : List ( Signal Background )
    -- children align to Top-Left corner minus the borders
    , children : List Node
    , popups : List Node
    -- relatives align to Top-Left corner with the borders
    , relatives : List ( Node, Sizes )
    }

-- Text extents are always ( Fit, Fit )
type alias TextDef =
    { text : Text
    }

type alias InputTextDef =
    { name : String
    , handler : ( Content -> Message )
    , content : Signal Content
    }
{-
type alias InputTextDef =
    { name : String
    , handler : ( String -> Message )
    , content : Signal String
    }
-}

type alias SUSDef =
    { name : String
    , address : Signal.Address Content
    , content : Signal Content
    , options : List String
    }

-- NODE PROPERTIES

type alias Size = Float
type alias Sizes = ( Size, Size )
type alias ISize = Int
type alias ISizes = ( ISize, ISize )
type alias Ratio = Float
type alias Spacing = Float

type Direction 
    = Up Spacing
    | Down Spacing
    | Left Spacing
    | Right Spacing
    | In 
    | Out

type Extent 
    = Fix Size
    | Fit
    | Fill Ratio

type Align 
    = TopLeft | TopMiddle | TopRight
    | MiddleLeft | Middle | MiddleRight
    | BottomLeft | BottomMiddle | BottomRight

{- Directional property that can be defined once, twice or four times for 
different sides -}
type DirProp a 
    = All a
    | HoriVert a a -- top/bottom right/left
    | TRBL a a a a -- top right bottom left

type alias BorderStyle = 
    { thickness : DirProp Size
    , color : Color
    }

type Background 
    = Filled Color
    | Textured String
    | Gradient Gradient

-- HELPER FNS

type alias Which a = ( a, a ) -> a

rectDef : RectDef
rectDef =
    { extents = ( Fill 1.0, Fill 1.0 )
    , dir = Down 0.0
    , border = Just { thickness = All 1.0, color = Color.black }
    , bgs = [ Filled Color.white |> Signal.constant ]
    , children = [ ]
    , popups =  [ ]
    , relatives = [ ]
    }

textDef : String -> TextDef
textDef str =
    { text = fromString str }

extentIsFill : Which Extent -> Node -> Bool
extentIsFill which node = which `extentOf` node |> isFill

extentIsFix : Which Extent -> Node -> Bool
extentIsFix which node = which `extentOf` node |> isFix

fixSize : Which Extent -> Node -> Float
fixSize which node = which `extentOf` node |> map fixSize' |> withDefault 0.0

borderSize : Maybe BorderStyle -> ( Size, Size, Size, Size )
borderSize bs = case bs of
        Just border -> borderSize' border.thickness
        _ -> ( 0.0, 0.0, 0.0, 0.0 )

-- INTERNAL

extentOf : Which Extent -> Node -> Maybe Extent
extentOf which node = case node.nodeType of
        Rect def -> which def.extents |> Just
        _ -> Nothing

isFill extent = withDefault False <| map isFill' extent

isFill' extent = case extent of
        Fill _ -> True
        _ -> False

isFix extent = withDefault False <| map isFix' extent

isFix' extent = case extent of
        Fix _ -> True
        _ -> False

fixSize' : Extent -> Float
fixSize' extent = case extent of
        Fix h -> h
        _ -> 0.0

borderSize' : DirProp Size -> ( Size, Size, Size, Size )
borderSize' b = case b of
        All a -> ( a, a, a, a )
        HoriVert h v -> ( h, v, h, v )
        TRBL t r b l -> ( t, r, b, l )

