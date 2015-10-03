import Data.Number.CReal

data Plane = XY | YZ | XZ deriving Show

data Vec = Vec CReal CReal

[vi,vj,v0,v11] =
  map (uncurry Vec)
      [(1,0),(0,1),(0,0),(1,1)]
  
data Angle = Angle CReal

data Flip = Yes | No deriving (Show, Eq)

-- | Translation
data Trs = Trs Vec
trs0 = Trs v0

-- | Rotation
data Rot = Rot Angle

[rot0,rot30,rot45,rot60,rot90] =
  map (\x -> Rot . Angle $ pi / x)
      [0,6,4,3,2]

-- | Reflection or not
data Ref = Ref Flip deriving Show

[ref0,ref1] = map Ref [Yes, No]

-- | Matrices and a few functions
data Matrix = Mat CReal CReal CReal CReal

reflectAcrossX :: Matrix
reflectAcrossX = Mat 1 0 0 (-1)

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

instance Show Matrix where
  show (Mat a b c d) = show a ++ " " ++ show b ++ "\n" ++
                       show c ++ " " ++ show d

instance Show Angle where
  show (Angle a) = "Angle " ++ showCReal 2 a

instance Show Rot where
  show (Rot a) = "(Rot (" ++ show a ++ "))"

instance Show Vec where
  show (Vec x y) = "Vec " ++ showCReal 2 x ++ " " ++ showCReal 2 y

instance Show Trs where
  show (Trs v) = "(Trs (" ++ show v ++ "))"

instance Num Vec where
  (Vec x1 y1) + (Vec x2 y2) =
    Vec (x1 + x2)
        (y1 + y2)
  (Vec x1 y1) - (Vec x2 y2) =
    Vec (x1 - x2)
        (y1 - y2)

instance Num Angle where
  (Angle a1) + (Angle a2) = Angle (a1 + a2)
  (Angle a1) - (Angle a2) = Angle (a1 - a2)

class BasicIsom a  where
  (.+) :: a -> a -> a
  trsBy :: Trs -> a -> a
  rotBy :: Rot -> a -> a
  refBy :: Ref -> a -> a
  toIsom :: a -> Isom
  apply :: a -> Vec -> Vec
  inverse :: a -> a

instance BasicIsom Trs where
  (Trs v1) .+ (Trs v2) = Trs $ v1 + v2
  trsBy = (.+)
  rotBy rot (Trs v) =
    Trs $ prod (rotMatrix rot) v
  refBy (Ref Yes) (Trs v) =
    Trs $ prod reflectAcrossX v
  refBy _ x = x
  toIsom trs = Isom trs rot0 ref0
  apply trs vec =
    let (Trs vec') = trsBy trs (Trs vec)
    in vec'
  inverse (Trs (Vec x y)) = Trs $ Vec (-x) (-y)

instance BasicIsom Rot where
  (Rot a) .+ (Rot b) = Rot $ a + b
  trsBy = const id
  rotBy = (.+)
  refBy (Ref Yes) (Rot (Angle a)) =
    Rot . Angle $ (-a)
  toIsom rot = Isom trs0 rot ref0
  apply rot vec =
    let (Trs vec') = rotBy rot (Trs vec)
    in vec'
  inverse (Rot (Angle a)) = Rot $ Angle (-a)

instance BasicIsom Ref where
  (Ref a) .+ (Ref b)
    | a == b = Ref No
    | otherwise = Ref Yes
  trsBy = const id
  rotBy = const id
  refBy = (.+)
  toIsom = Isom trs0 rot0 -- MOAR ETA REDUCE!
  apply ref vec =
    let (Trs vec') = refBy ref (Trs vec)
    in vec'
  inverse (Ref a) = Ref $ case a of
                             Yes -> No
                             No  -> Yes

data Isom = Isom Trs Rot Ref deriving Show

mkIsom :: Vec -> Angle -> Flip -> Isom
mkIsom v a r =
  Isom (Trs v)
       (Rot a)
       (Ref r)

(#) = flip (.)

transform :: Isom -> Vec -> Vec
transform (Isom t r f) = apply t # apply r # apply f

transformSeq :: [Isom] -> Vec -> Vec
transformSeq = flip $ foldl (flip transform)

transformTimes :: Int -> Isom -> Vec -> Vec
transformTimes = (transformSeq .) . replicate -- thanks, Blunt!

-- | Compose two isometries.
-- This is done by starting with t1 r1 f1 t2 r2 f2 and reordering to combine
-- similar isometries. Each time one isometry "passes through" another, the
-- second one acts on the first. (Artin explains this far better than I can.)

compose :: Isom -> Isom -> Isom
compose (Isom trs1 rot1 ref1) (Isom trs2 rot2 ref2) =
  Isom (trs1 .+ trs2'') (rot1 .+ rot2') (ref1 .+ ref2)
  where trs2' = refBy ref1 trs2
        trs2'' = rotBy rot1 trs2'
        rot2' = refBy ref1 rot2

main = undefined
