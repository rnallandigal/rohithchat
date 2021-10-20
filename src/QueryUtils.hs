{-# LANGUAGE OverloadedStrings #-}

module QueryUtils where

import Data.Maybe
import Data.Text (Text, intercalate)
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField

import Model (Pagination(..))

defPagination :: Pagination
defPagination = Pagination (Just 5) (Just 0)

constraintQ :: (ToField a) => Text -> Maybe a -> Maybe (Text, NamedParam)
constraintQ k = fmap $ \v -> (k <> " = :" <> k, (":" <> k) := v)

whereQ :: [(Text, NamedParam)] -> (Text, [NamedParam])
whereQ cs = let (cq, cn) = unzip cs in
    if null cq then ("", [])
    else ("where " <> (intercalate " and " cq), cn)

paginationQ :: Pagination -> (Text, [NamedParam])
paginationQ (Pagination s n) =
    let q = "limit :limit offset :offset"
        pSize = fromMaybe 5 s
        offset = pSize * (fromMaybe 0 n)
    in  (q, [":limit" := pSize, ":offset" := offset] )

selectQ :: Text                     -- table name ("users")
        -> [(Text, NamedParam)]     -- constaints list (["id = :id", id])
        -> Pagination               -- pagination
        -> Text                     -- suffix (order by)
        -> (Query, [NamedParam])    -- named query
selectQ t cs p o =
    let (wq, wn) = whereQ cs
        (pq, pn) = paginationQ p
    in  (Query $ intercalate " " ["select * from " <> t, wq, o, pq], wn ++ pn)
