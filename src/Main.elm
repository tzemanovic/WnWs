import Node exposing ( .. )
import Render exposing ( .. )
import Color exposing ( .. )

wncode = 
    { nodeType = Rect
        { extents = ( Fill 1.0, Fill 1.0 )
        , dir = Down
        , border = Just { thickness = TRBL 50.0 25.0 5.0 10.0
            , color = Color.red }
        , bgs = [ Filled Color.grey ]
        , children = 
            [ 
                { nodeType = Rect
                    { extents = ( Fix 450.0, Fix 300.0 )
                    , dir = Down
                    , border = Nothing
                    , bgs = [ Textured "http://placehold.it/450x300" ]
                    , children = 
                        [
                            { nodeType = Text { text = "1" } }
                        ]
                    }
                }
                , { nodeType = Rect
                    { extents = ( Fill 1.0, Fix 50.0 )
                    , dir = Right
                    , border = Just { thickness = HoriVert 2.5 5.0
                        , color = Color.blue }
                    , bgs = [ Gradient grad ]
                    , children = 
                        [
                            { nodeType = Text { text = "222" }
                            }
                            , { nodeType = Text { text = "333" }
                            }
                        ]
                    }
                }
            ]
        }
    }

grad : Gradient
grad = linear (0,60) (0,-60)
      [ (0, rgb 0 171 235)
      , (0.79, white)
      , (0.8, rgb 38 192 0)
      , (1, white)
      ]

main = render wncode
{-
TODO
 * padding, childrenSpacing (there is no margin)
 * popup children
-}
