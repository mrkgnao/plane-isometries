import Data.Number.CReal

data Plane = XY | YZ | XZ deriving Show
data Vec = Vec CReal CReal deriving Show
data Angle = Angle CReal deriving Show
-- | Translation
data Trs = Trs Vec deriving Show
-- | Rotation
data Rot = Rot Angle deriving Show 
-- | Reflection or not
data Ref = Ref | NoRef deriving Show

-- These are the generators of the group M.
data BasicIsometry = T Trs -- translations by a vector
                   | R Rot -- rotations by some angle
                   | F Ref -- reflections (about the x-axis)
                   deriving Show

-- We can now build up more complex isometries.
-- First define a datatype for an isometry that can (possibly)
-- translate, rotate and reflect a vector.
data Isometry = Isometry Trs Rot Ref deriving Show

-- We must be able to compose two isometries.
-- Since we impose an explicit ordering (translation first, then
-- the rotation, then the reflection), we can make do with fewer
-- relations than actually needed to compute in M.
compose :: Isometry -> Isometry -> Isometry

-- For isometries with no reflections, we turn (t1 r1) (t2 r2) into
-- (t1 t2') (r1 + r2). That is, to swap t2 and r1, we must first rotate
-- t2 by the angle specified by r1.
compose (Isometry (Trs v1) r1 NoRef) (Isometry (Trs v2) r2 NoRef) =
  Isometry (Trs (v1 .+ v2')) (r1 ^+ r2) NoRef
  where v2' = apply r2 v2

rotate :: Vec -> Angle -> Vec
rotate v (Angle a) = prod (rotMatrix (Angle a)) v

(.+) :: Vec -> Vec -> Vec
(Vec x1 y1) .+ (Vec x2 y2) = Vec (x1 + y1) (x2 + y2)

(^+) :: Rot -> Rot -> Rot
(Rot (Angle a1)) ^+ (Rot (Angle a2)) = Rot (Angle (a1 + a2))

data Matrix = Mat CReal CReal CReal CReal

instance Show Matrix where
  show (Mat a b c d) = (show a) ++ " " ++ (show b) ++ "\n" ++
                       (show c) ++ " " ++ (show d)

apply :: Rot -> Vec -> Vec
apply (Rot (Angle a)) = prod (rotMatrix (Angle a))

rotMatrix :: Angle -> Matrix
rotMatrix (Angle a) = Mat (cos a) (- (sin a)) (sin a) (cos a)

prod :: Matrix -> Vec -> Vec
prod (Mat a b c d) (Vec x y) = Vec (a * x + b * y) (c * x + d * y)

main = undefined
