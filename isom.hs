import Data.Number.CReal

data Plane = XY | YZ | XZ deriving Show
data Vec = Vec CReal CReal deriving Show
data Angle = Angle CReal deriving Show
type Flip = Bool
-- | Translation
data Trs = Trs Vec deriving Show
-- | Rotation
data Rot = Rot Angle deriving Show 
-- | Reflection or not
data Ref = Ref Flip deriving Show

-- These are the generators of the group M.
data BasicIsom
  = T Trs -- translations by a vector
  | R Rot -- rotations by some angle
  | F Ref -- reflections (about the x-axis)
  deriving (Show)

add :: BasicIsom -> BasicIsom -> BasicIsom

add (T (Trs (Vec x1 y1))) (T (Trs (Vec x2 y2))) =
  T . Trs $
  Vec (x1 + x2)
      (y1 + y2)

add (R (Rot (Angle a1))) (R (Rot (Angle a2))) =
  R . Rot $ (Angle (a1 + a2))

add (F (Ref a)) (F (Ref b)) = F . Ref $ a /= b -- nice little xor, huh?

-- | Push the translations to the front.
reorderTrs :: [BasicIsom] -> [BasicIsom]

-- Collapse consecutive anything
reorderTrs (xs:a@(T _):b@(T _):ys) =
  reorderTrs (xs : (add a b) : ys)

reorderTrs [a@(T _),b@(T _)] = [add a b]

reorderTrs (xs:a@(R _):b@(R _):ys) =
  reorderTrs (xs : (add a b) : ys)

reorderTrs (xs:a@(F _):b@(F _):ys) =
  reorderTrs (xs : (add a b) : ys)

-- Skip nop-reflections
reorderTrs (xs:r@(F a):t@(T s):ys) =
  case a of
    Ref True ->
      reorderTrs (xs : T (reflectL a s) : r : ys)
    Ref False -> reorderTrs (xs : t : ys)

-- Skip nop-rotations as well
reorderTrs (xs:r@(R rot):t@(T trs):ys) =
  case rot of
    Rot (Angle 0) -> reorderTrs (xs : t : ys)
    _ ->
      reorderTrs (xs : T (rotateL trs rot) : r : ys)

reorderTrs l = l

data Isom = Isom Trs Rot Ref

mkIsom :: Vec -> Angle -> Flip -> Isom
mkIsom v a r = Isom (Trs v) (Rot a) (Ref r)

rotate :: Vec -> Angle -> Vec
rotate v (Angle a) = prod (rotMatrix (Angle a)) v

rotateL :: Trs -> Rot -> Trs
rotateL (Trs v) (Rot a) = Trs $ rotate v a

data Matrix = Mat CReal CReal CReal CReal

instance Show Matrix where
  show (Mat a b c d) = (show a) ++ " " ++ (show b) ++ "\n" ++
                       (show c) ++ " " ++ (show d)

apply :: Rot -> Vec -> Vec
apply (Rot (Angle a)) = prod (rotMatrix (Angle a))

reflect :: Ref -> Vec -> Vec
reflect (Ref True) = prod (Mat 1 0 0 (-1))
reflect _ = id

reflectL :: Ref -> Trs -> Trs
reflectL a (Trs v1) = Trs $ reflect a v1

rotMatrix :: Angle -> Matrix
rotMatrix (Angle a) = Mat (cos a) (- (sin a)) (sin a) (cos a)

prod :: Matrix -> Vec -> Vec
prod (Mat a b c d) (Vec x y) = Vec (a * x + b * y) (c * x + d * y)

main = undefined
