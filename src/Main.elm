import Node exposing ( .. )
import Render exposing ( .. )
import Color

wncode = 
    { nodeType = Rect
        { extents = ( Fill 1.0, Fill 1.0 )
        , dir = Down
        , border = Just { thickness = TRBL 50.0 25.0 5.0 10.0
            , color = Color.red }
        , children = 
            [ 
                { nodeType = Rect
                    { extents = ( Fill 1.0, Fill 1.0 )
                    , dir = Down
                    , border = Nothing
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

main = render wncode
{-
TODO
 * padding, background, childrenSpacing (there is no margin)
 * popup children
-}
