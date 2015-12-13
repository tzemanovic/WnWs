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


type NodeType = Root RootDef
        | Rect RectDef
        | Text TextDef

type alias RootDef =
        { 
        }

type alias RectDef =
        { 
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
        | NoChildren

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

renderText : Node -> Node -> TextDef -> Element
renderText parent node def = leftAligned (fromString def.text)

renderNode : Node -> Node -> Element
renderNode parent node = case node.nodeType of
       Root def -> Graphics.Element.empty
       Rect def -> Graphics.Element.empty
       Text def -> renderText parent node def

renderScene : Scene -> Element 
renderScene scene = case scene.children of
       Children c -> flow down (List.map (renderNode scene) c)
       NoChildren -> Graphics.Element.empty
