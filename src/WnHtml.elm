module WnHtml 
        ( NodeType(..) -- hide Root?
        , RootDef
        , RectDef
        , TextDef
        , Node
        , Children(..)
        , makeScene
        , Scene
        , renderScene
        ) where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Text as CText exposing (..)
import Color exposing (..)


type NodeType = Root RootDef
        | Rect RectDef
        | Text TextDef

type alias RootDef =
        { 
        }

type alias RectDef =
        { width : Int
        , height : Int 
        , background : Color
        }

type alias TextDef =
        { text : String
        }

canHaveChildren : NodeType -> Bool
canHaveChildren t = case t of
       Root _ -> True
       Rect _ -> True
       Text _ -> True

type alias NodeID = Int

--type Node = NodeLeaf NodeDef
--        | Node NodeDef List Node

type alias Node = 
        { nodeType : NodeType
        , id : NodeID
        , children : Children
        }

-- Recursive type (https://github.com/elm-lang/elm-compiler/blob/master/hints/recursive-alias.md)
type Children = Children (List Node)

rootNode : Node
rootNode = 
        { nodeType = Root {}
        , id = 0
        , children = Children []
        }

type alias Scene = Node

makeScene : List Node -> Scene
makeScene nodes = { rootNode | children = Children nodes }


-- RENDER

renderScene : Scene -> Element 
renderScene scene = renderNode Nothing scene

renderNode : Maybe Node -> Node -> Element
renderNode parent node = 
        let children = case node.children of
       Children c -> flow down (List.map (renderNode (Just node)) c)
        in case node.nodeType of
       Root def -> renderRoot def children
       Rect def -> renderRect def children
       Text def -> renderText def

renderRoot : RootDef -> Element -> Element
renderRoot def children = children

renderRect : RectDef -> Element -> Element
renderRect def children = Graphics.Element.color def.background 
        (container def.width def.height topLeft children)

renderText : TextDef -> Element
renderText def = leftAligned (fromString def.text)

