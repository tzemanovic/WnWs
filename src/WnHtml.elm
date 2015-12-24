module WnHtml 
    (  NodeType( .. ) -- hide Root?
    , RootDef
    , RectDef
    , TextDef
    , Node
    , Children( .. )
    , makeScene
    , Scene
    , render
    , Extent( .. )
    , Align( .. )
     ) where

import Graphics.Element exposing ( .. )
import Graphics.Collage exposing ( .. )
import Text as CText exposing ( .. )
import Color exposing ( .. )
import Window
import List.Extra exposing ( .. )
import Html exposing ( .. )
import Html.Attributes exposing ( .. )

type Either a b
    = Left a
        | Right b 

type NodeType = Root RootDef
    | Rect RectDef
    | Text TextDef

type alias RootDef =
    { 
    }

type alias RectDef =
    { background : Color
    }

type alias TextDef =
    { text : String
    }

canHaveChildren : NodeType -> Bool
canHaveChildren t = case t of
   Root _ -> True
   Rect _ -> True
   Text _ -> False

type alias NodeID = Int

type alias Node = 
    { nodeType : NodeType
    , extents : Extents
    , id : NodeID
    , children : Children
    }

-- Recursive type ( https://github.com/elm-lang/elm-compiler/blob/master/hints/recursive-alias.md )
type Children = Flow Direction ( List Node )
    | Empty

overlay cs = Flow outward cs

rootNode : Node
rootNode = 
    { nodeType = Root {}
    , extents = ( Fill 1.0, Fill 1.0 )
    , id = 0
    , children = overlay []
    }

type alias Scene = Node

-- size of form cannot be queried easily, enhance the rendered form with it
type alias Render = Either Element ( Form, Sizes )

makeScene : List Node -> Scene
makeScene nodes = { rootNode | children = overlay nodes }


-- SCENE RENDER

render : Scene -> Signal Element
render scene = Signal.map ( renderScene scene ) Window.dimensions

renderScene : Scene -> ISizes -> Element
renderScene scene size = renderNodes size ( tupleMap ( toFloat >> Just ) size ) 
    scene |> renderToElement

renderNodes : ISizes -> ( Maybe Size, Maybe Size ) -> Node -> Render
renderNodes sceneSize ( parentW, parentH ) node = 
    let getSize extent parentSize =  case fst node.extents of
           Fix w -> Just w
           Fit -> Nothing
           Fill ratio -> parentW
        size = ( getSize ( fst node.extents ) parentW
               , getSize ( snd node.extents ) parentH )
        ( children, childrenSize ) = renderChildren sceneSize size node.children
        this = renderNode sceneSize ( parentW, parentH ) childrenSize node 
            |> renderToElement
    in flow outward [ this, renderToElement children ] |> Left

renderChildren : ISizes -> ( Maybe Size, Maybe Size ) -> Children 
   -> ( Render, ( Maybe Size, Maybe Size ) )
renderChildren sceneSize parentSize children = 
    let compose dir cs = case cs of
            [ one ] -> one
            many -> List.map renderToElement many |> flow dir |> Left
    in case children of
        Flow dir cs -> 
            let render = List.map ( renderNodes sceneSize parentSize ) cs 
                |> compose dir
            in ( render, sizeOfRender render |> tupleMap Just )
        Empty -> ( Left Graphics.Element.empty, ( Nothing, Nothing ) )

sizeOfRender : Render -> Sizes
sizeOfRender render = case render of
    Left e -> sizeOf e |> toFloat2
    Right ( f, s ) -> s

maxSizes : List ISizes -> ISizes
maxSizes sizes = 
    let max which = List.map which sizes |> List.maximum |> Maybe.withDefault 0
    in ( max fst, max snd )

renderToElement : Render -> Element
renderToElement render = case render of
    Left element -> element
    Right ( form, ( width, height ) ) -> 
        collage ( ceiling width ) ( ceiling height ) [form]

renderToForm : Render -> Form
renderToForm render = case render of
    Left element -> toForm element
    Right ( form, _ ) -> form

renderNode : ISizes -> ( Maybe Size, Maybe Size ) 
   -> ( Maybe Size, Maybe Size ) -> Node -> Render
renderNode sceneSize ( parentW, parentH ) ( childrenW, childrenH ) node = 
    let getSize extent parentSize childrenSize = case extent of
            Fix w -> Just w
            Fit -> childrenSize
            Fill ratio -> Maybe.map ( ( * ) ratio ) parentSize
        ( width, height ) = ( getSize ( fst node.extents ) parentW childrenW 
                            , getSize ( snd node.extents ) parentH childrenH )
    in case node.nodeType of
        Root def -> renderRoot def width height
        Rect def -> renderRect def width height
        Text def -> renderText def width height

renderRoot : RootDef -> Maybe Size -> Maybe Size -> Render
renderRoot def width height = 
    let rootRect =
            { background = black
            }
    in renderRect rootRect width height

renderRect : RectDef -> Maybe Size -> Maybe Size -> Render
renderRect def width height =
    case ( width, height ) of
        ( Just w, Just h ) ->
            ( rect w h |> filled def.background, ( w, h ) ) |> Right
        _ -> Left Graphics.Element.empty

renderText : TextDef -> Maybe Size -> Maybe Size -> Render
renderText def width height =
    let setWidth = case width of
            Just w -> ceiling w |> Graphics.Element.width
            _ -> identity
        setHeight = case height of
            Just h -> ceiling h |> Graphics.Element.height
            _ -> identity
    --TODO ( "word-wrap", "break-word" )?
    in fromString def.text |> leftAligned |> setWidth |> setHeight |> Left


-- NODE PROPERTIES

type alias Size = Float
type alias Sizes = ( Float, Float )
type alias ISize = Int
type alias ISizes = ( ISize, ISize )
type alias Ratio = Float
type alias Extents = ( Extent, Extent )

type Extent =
    Fix Size
    | Fit
    | Fill Ratio

type Align = TopLeft | TopMiddle | TopRight
    | MiddleLeft | Middle | MiddleRight
    | BottomLeft | BottomMiddle | BottomRight

toFloat2 : ( Int, Int ) -> ( Float, Float )
toFloat2 xy = tupleMap toFloat xy

tupleMap : ( a -> b ) -> ( a, a ) -> ( b, b )
tupleMap f ( x, y ) = ( f x, f y )
