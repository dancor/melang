module Util.CoverZip where

coverUncons :: [Maybe a] -> (Maybe a, [Maybe a])
coverUncons (x:r) = (x, r)
coverUncons _ = (Nothing, [])

coverZipWith8
    :: (Maybe a -> Maybe b -> Maybe c -> Maybe d
       -> Maybe e -> Maybe f -> Maybe g -> Maybe h
       -> i)
    -> [Maybe a] -> [Maybe b] -> [Maybe c] -> [Maybe d]
    -> [Maybe e] -> [Maybe f] -> [Maybe g] -> [Maybe h]
    -> [i]
coverZipWith8 f [] [] [] [] [] [] [] l8 =
    map (f Nothing Nothing Nothing Nothing Nothing Nothing Nothing) l8
coverZipWith8 f [] [] [] [] [] [] l7 l8 =
    coverZipWith (f Nothing Nothing Nothing Nothing Nothing Nothing) l7 l8
coverZipWith8 f [] [] [] [] [] l6 l7 l8 =
    coverZipWith3 (f Nothing Nothing Nothing Nothing Nothing) l6 l7 l8
coverZipWith8 f [] [] [] [] l5 l6 l7 l8 =
    coverZipWith4 (f Nothing Nothing Nothing Nothing) l5 l6 l7 l8
coverZipWith8 f [] [] [] l4 l5 l6 l7 l8 =
    coverZipWith5 (f Nothing Nothing Nothing) l4 l5 l6 l7 l8
coverZipWith8 f [] [] l3 l4 l5 l6 l7 l8 =
    coverZipWith6 (f Nothing Nothing) l3 l4 l5 l6 l7 l8
coverZipWith8 f [] l2 l3 l4 l5 l6 l7 l8 =
    coverZipWith7 (f Nothing) l2 l3 l4 l5 l6 l7 l8
coverZipWith8 f (x1:r1) l2 l3 l4 l5 l6 l7 l8 =
    f x1 x2 x3 x4 x5 x6 x7 x8 :
    coverZipWith8 f r1 r2 r3 r4 r5 r6 r7 r8
  where
    (x2, r2) = coverUncons l2
    (x3, r3) = coverUncons l3
    (x4, r4) = coverUncons l4
    (x5, r5) = coverUncons l5
    (x6, r6) = coverUncons l6
    (x7, r7) = coverUncons l7
    (x8, r8) = coverUncons l8

coverZipWith7
    :: (Maybe a -> Maybe b -> Maybe c -> Maybe d
       -> Maybe e -> Maybe f -> Maybe g -> h)
    -> [Maybe a] -> [Maybe b] -> [Maybe c] -> [Maybe d]
    -> [Maybe e] -> [Maybe f] -> [Maybe g] -> [h]
coverZipWith7 f [] [] [] [] [] [] l7 =
    map (f Nothing Nothing Nothing Nothing Nothing Nothing) l7
coverZipWith7 f [] [] [] [] [] l6 l7 =
    coverZipWith (f Nothing Nothing Nothing Nothing Nothing) l6 l7
coverZipWith7 f [] [] [] [] l5 l6 l7 =
    coverZipWith3 (f Nothing Nothing Nothing Nothing) l5 l6 l7
coverZipWith7 f [] [] [] l4 l5 l6 l7 =
    coverZipWith4 (f Nothing Nothing Nothing) l4 l5 l6 l7
coverZipWith7 f [] [] l3 l4 l5 l6 l7 =
    coverZipWith5 (f Nothing Nothing) l3 l4 l5 l6 l7
coverZipWith7 f [] l2 l3 l4 l5 l6 l7 =
    coverZipWith6 (f Nothing) l2 l3 l4 l5 l6 l7
coverZipWith7 f (x1:r1) l2 l3 l4 l5 l6 l7 =
    f x1 x2 x3 x4 x5 x6 x7 :
    coverZipWith7 f r1 r2 r3 r4 r5 r6 r7
  where
    (x2, r2) = coverUncons l2
    (x3, r3) = coverUncons l3
    (x4, r4) = coverUncons l4
    (x5, r5) = coverUncons l5
    (x6, r6) = coverUncons l6
    (x7, r7) = coverUncons l7

coverZipWith6
    :: (Maybe a -> Maybe b -> Maybe c -> Maybe d
       -> Maybe e -> Maybe f -> g)
    -> [Maybe a] -> [Maybe b] -> [Maybe c] -> [Maybe d]
    -> [Maybe e] -> [Maybe f] -> [g]
coverZipWith6 f [] [] [] [] [] l6 =
    map (f Nothing Nothing Nothing Nothing Nothing) l6
coverZipWith6 f [] [] [] [] l5 l6 =
    coverZipWith (f Nothing Nothing Nothing Nothing) l5 l6
coverZipWith6 f [] [] [] l4 l5 l6 =
    coverZipWith3 (f Nothing Nothing Nothing) l4 l5 l6
coverZipWith6 f [] [] l3 l4 l5 l6 =
    coverZipWith4 (f Nothing Nothing) l3 l4 l5 l6
coverZipWith6 f [] l2 l3 l4 l5 l6 =
    coverZipWith5 (f Nothing) l2 l3 l4 l5 l6
coverZipWith6 f (x1:r1) l2 l3 l4 l5 l6 =
    f x1 x2 x3 x4 x5 x6 :
    coverZipWith6 f r1 r2 r3 r4 r5 r6
  where
    (x2, r2) = coverUncons l2
    (x3, r3) = coverUncons l3
    (x4, r4) = coverUncons l4
    (x5, r5) = coverUncons l5
    (x6, r6) = coverUncons l6

coverZipWith5
    :: (Maybe a -> Maybe b -> Maybe c -> Maybe d
       -> Maybe e -> f)
    -> [Maybe a] -> [Maybe b] -> [Maybe c] -> [Maybe d]
    -> [Maybe e] -> [f]
coverZipWith5 f [] [] [] [] l5 =
    map (f Nothing Nothing Nothing Nothing) l5
coverZipWith5 f [] [] [] l4 l5 =
    coverZipWith (f Nothing Nothing Nothing) l4 l5
coverZipWith5 f [] [] l3 l4 l5 =
    coverZipWith3 (f Nothing Nothing) l3 l4 l5
coverZipWith5 f [] l2 l3 l4 l5 =
    coverZipWith4 (f Nothing) l2 l3 l4 l5
coverZipWith5 f (x1:r1) l2 l3 l4 l5 =
    f x1 x2 x3 x4 x5 :
    coverZipWith5 f r1 r2 r3 r4 r5
  where
    (x2, r2) = coverUncons l2
    (x3, r3) = coverUncons l3
    (x4, r4) = coverUncons l4
    (x5, r5) = coverUncons l5

coverZipWith4
    :: (Maybe a -> Maybe b -> Maybe c -> Maybe d -> e)
    -> [Maybe a] -> [Maybe b] -> [Maybe c] -> [Maybe d] -> [e]
coverZipWith4 f [] [] [] l4 =
    map (f Nothing Nothing Nothing) l4
coverZipWith4 f [] [] l3 l4 =
    coverZipWith (f Nothing Nothing) l3 l4
coverZipWith4 f [] l2 l3 l4 =
    coverZipWith3 (f Nothing) l2 l3 l4
coverZipWith4 f (x1:r1) l2 l3 l4 =
    f x1 x2 x3 x4 :
    coverZipWith4 f r1 r2 r3 r4
  where
    (x2, r2) = coverUncons l2
    (x3, r3) = coverUncons l3
    (x4, r4) = coverUncons l4

coverZipWith3
    :: (Maybe a -> Maybe b -> Maybe c -> d)
    -> [Maybe a] -> [Maybe b] -> [Maybe c] -> [d]
coverZipWith3 f [] [] l3 =
    map (f Nothing Nothing) l3
coverZipWith3 f [] l2 l3 =
    coverZipWith (f Nothing) l2 l3
coverZipWith3 f (x1:r1) l2 l3 =
    f x1 x2 x3 :
    coverZipWith3 f r1 r2 r3
  where
    (x2, r2) = coverUncons l2
    (x3, r3) = coverUncons l3

coverZipWith
    :: (Maybe a -> Maybe b -> c)
    -> [Maybe a] -> [Maybe b] -> [c]
coverZipWith f [] l2 =
    map (f Nothing) l2
coverZipWith f (x1:r1) l2 =
    f x1 x2 :
    coverZipWith f r1 r2
  where
    (x2, r2) = coverUncons l2
