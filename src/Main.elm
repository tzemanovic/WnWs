import Node exposing ( .. )
import Render exposing ( .. )

wncode = 
    { nodeType = Rect
        { extents = ( Fill 1.0, Fill 1.0 )
        , dir = Down
        , children = 
            [ 
                { nodeType = Rect
                    { extents = ( Fill 1.0, Fill 1.0 )
                    , dir = Down
                    , children = 
                        [
                            { nodeType = Text { text = "1" } }
                        ]
                    }
                }
                , { nodeType = Rect
                    { extents = ( Fill 1.0, Fix 50.0 )
                    , dir = Right
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
--TODO
-- padding, border and margin?
-- popup children
