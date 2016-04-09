module InputHandler
    ( Handler
    , handleFloat
    ) where

import Char                 exposing ( isDigit )
import Graphics.Input.Field exposing ( Content )
import List                 exposing ( foldl, reverse )
import Signal               exposing ( Address, Message, forwardTo, message )
import String               exposing ( toList, fromList )

type alias Handler = Content -> Message

handleFloat : Address Content -> Handler
handleFloat address = message ( forwardTo address handleFloat' )

-- INTERNAL

handleFloat' : Content -> Content
handleFloat' content = 
    let chars = toList content.string
        ( _, filteredChars ) = foldl ( \n ( hasDot, acc ) -> 
            if n == '.' 
            then case hasDot of
                True -> ( hasDot, acc )
                False -> ( True, n :: acc )
            else if isDigit n
            then ( hasDot, n :: acc )
            else ( hasDot, acc )
            ) ( False, [ ] ) chars
        str = fromList ( reverse ( filteredChars ) )
    in  { content | string = str }

