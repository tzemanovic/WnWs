import Node exposing ( .. )
import Render exposing ( .. )
import Color

wncode = 
    { nodeType = Rect
        { extents = ( Fill 1.0, Fill 1.0 )
        , dir = Down
        , border = Just { thickness = 5.0, color = Color.red }
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
                    , border = Just { thickness = 2.5, color = Color.blue }
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
 * padding, border, background (there is no margin)
 * make border sum of 1, 2 or 4 sizes (all the same, top/bottom and left/right, 
 or top, right, bottom, left), same type can be applied to padding later
 * popup children
-}
