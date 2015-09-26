import Data.Number.CReal

data Plane = XY | YZ | XZ deriving Show
data Vec = Vec CReal CReal deriving Show
data Angle = Angle CReal deriving Show
data Flip
  = Yes
  | No
  deriving (Show, Eq)
-- | Translation
data Trs = Trs Vec deriving Show
-- | Rotation
data Rot = Rot Angle deriving Show 
-- | Reflection or not
data Ref = Ref Flip deriving Show

data Matrix = Mat CReal CReal CReal CReal

reflectAcrossX :: Matrix
reflectAcrossX = Mat 1 0 0 (-1)

instance Show Matrix where
  show (Mat a b c d) = show a ++ " " ++ show b ++ "\n" ++
                       show c ++ " " ++ show d

instance Num Vec where
  (Vec x1 y1) + (Vec x2 y2) = Vec (x1 + x2) (y1 + y2)
  (Vec x1 y1) - (Vec x2 y2) = Vec (x1 - x2) (y1 - y2)

instance Num Trs where
  (Trs t1) + (Trs t2) = Trs $ t1 + t2
  (Trs t1) - (Trs t2) = Trs $ t1 - t2

instance Num Angle where
  (Angle a1) + (Angle a2) = Angle (a1 + a2)
  (Angle a1) - (Angle a2) = Angle (a1 - a2)

instance Num Rot where
  (Rot a1) + (Rot a2) = Rot $ a1 + a2
  (Rot a1) - (Rot a2) = Rot $ a1 - a2

instance Num Ref where
  (Ref a) + (Ref b)
    | a == b = Ref No
    | otherwise = Ref Yes

class BasicIsom a  where
  (.+) :: a -> a -> a
  rotBy :: Rot -> a -> a
  refBy :: Ref -> a -> a
  trsBy :: Trs -> a -> a

instance BasicIsom Trs where
  (Trs v1) .+ (Trs v2) = Trs $ v1 + v2
  trsBy = (.+)
  rotBy rot (Trs v) =
    Trs $ prod (rotMatrix rot) v
  refBy (Ref Yes) (Trs v) =
    Trs $ prod reflectAcrossX v
  refBy _ x = x

instance BasicIsom Rot where
  (Rot a) .+ (Rot b) = Rot $ a + b
  trsBy = const id
  rotBy = (.+)
  refBy (Ref Yes) (Rot (Angle a)) =
    Rot . Angle $ (-a)

instance BasicIsom Ref where
  (Ref a) .+ (Ref b)
    | a == b = Ref No
    | otherwise = Ref Yes
  trsBy = const id
  rotBy = const id
  refBy = (.+)

reflectRot :: Ref -> Rot -> Rot
reflectRot (Ref Yes) (Rot (Angle x)) =
  Rot . Angle $ (-x)
reflectRot _ x = x

rotMatrix :: Rot -> Matrix
rotMatrix (Rot (Angle a)) =
  Mat (cos a)
      (-(sin a))
      (sin a)
      (cos a)

prod :: Matrix -> Vec -> Vec
prod (Mat a b c d) (Vec x y) =
  Vec (a * x + b * y)
      (c * x + d * y)

data Isom = Isom Trs Rot Ref

mkIsom :: Vec -> Angle -> Flip -> Isom
mkIsom v a r =
  Isom (Trs v)
       (Rot a)
       (Ref r)

-- | Compose two isometries.
-- This is done by starting with t1 r1 f1 t2 r2 f2 and reordering to combine
-- similar isometries. Each time one isometry "passes through" another, the
-- second one acts on the first. (Artin explains this far better than I can.)

compose :: Isom -> Isom -> Isom
compose (Isom trs1 rot1 ref1) (Isom trs2 rot2 ref2) =
  Isom (trs1 + trs2'') (rot1 + rot2') (ref1 + ref2)
  where trs2' = reflectTrs ref1 trs2
        trs2'' = rotateBy rot1 trs2''
        rot2' = reflectRot ref1 rot2

main = undefined
