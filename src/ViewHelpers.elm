module ViewHelpers exposing (truncateText)

-- CONSTANTS


maximumNumberOfChars : Int
maximumNumberOfChars =
    500



-- HELPERS
{-
   Sometimes the text could be quite long so it won't fit in the container.
   For such cases we provide UI helper to cut extra symbols. It's not only
   cut everything after N-th char but also keeps the word undamaged.

   For example:
   ...hello wo[<- 500 symbols here]rld

   In such case the method returns not the "hello wo..." but "hello..." to avoid
   such not-friendly tails.
-}


truncateText : String -> String
truncateText text =
    let
        spaceIndexes =
            String.indexes " " text

        isEndedWithSpace =
            List.member maximumNumberOfChars spaceIndexes

        previousSpaceIndex =
            List.foldr
                (\index accum ->
                    if accum >= maximumNumberOfChars && index < maximumNumberOfChars then
                        index

                    else
                        accum
                )
                maximumNumberOfChars
                spaceIndexes

        truncated =
            String.slice
                0
                (if isEndedWithSpace then
                    maximumNumberOfChars

                 else
                    previousSpaceIndex
                )
                text
    in
    if String.length text < maximumNumberOfChars then
        text

    else
        truncated ++ "..."
