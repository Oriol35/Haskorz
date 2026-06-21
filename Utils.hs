module Utils ( todo, maybeHead ) where

-- This function throws an exception notifying that something hasn't been implemented yet
todo :: a
todo = error "TODO..."

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x
