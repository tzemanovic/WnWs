module InputHandler
    (Handler
    ,handleFloat
    ,filterFloat
    ) where

import Char                 exposing (isDigit)
import Graphics.Input.Field exposing (Content)
import List                 exposing (foldl, reverse)
import Signal               exposing (Address, Message, forwardTo, message)
import String               exposing (toList, fromList)

type alias Handler = Content -> Message

handleFloat : Address Content -> Handler
handleFloat address = message (forwardTo address handleFloat')

filterFloat : String -> String
filterFloat str = 
    let chars = toList str
        (_, filteredChars) = foldl 
            (\n (hasDot, acc) -> 
                if n == '.' 
                then case hasDot of
                    True -> (hasDot, acc)
                    False -> (True, n :: acc)
                else if isDigit n
                then (hasDot, n :: acc)
                else (hasDot, acc)
            ) (False, []) chars
    in fromList (reverse (filteredChars))

-- INTERNAL

handleFloat' : Content -> Content
handleFloat' content = 
    let str = filterFloat content.string
    in  {content | string = str}

