module Data.List.Extended
  ( latest,
  )
where

import Data.List (sort)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

latest :: (Ord a) => [a] -> Maybe a
latest = safeHead . sort
