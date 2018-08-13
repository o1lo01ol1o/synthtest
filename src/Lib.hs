{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Arrows, BangPatterns,
  FlexibleContexts, FunctionalDependencies, ScopedTypeVariables,
  TypeFamilies #-}

module Lib
  ( sineWave
  , runSignal
  , Interpreter(..)
  , AccSample(..)
  , Signal
  , funToTable
  , Clock(..)
  , AudRate
  , Table(..)
  , Backend(..)
  , arrange
  , unsafeFrameToQueue
  , generateSample
  ) where
import Control.Arrow
import Control.Arrow.Operations
import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits (KnownNat, natVal, Nat)
import Control.Category
import Utils
import Debug.Trace (trace)
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.IO.Data.Vector.Storable as AIO
import qualified Data.Array.Accelerate.Interpreter as AI
import qualified Data.Array.Accelerate.LLVM.Native as AL
import qualified Data.Array.Accelerate.Array.Sugar as AS hiding (Elt)
import qualified Data.StorableVector                 as SV
import qualified Data.StorableVector.ST.Strict       as SVST
import qualified Data.StorableVector.Base            as SVB
import qualified Data.Vector.Storable                as DVS
import Control.Concurrent.STM.TBQueue
import           Foreign.Storable                    (Storable)
import Control.Concurrent.STM hiding (check)
import           Foreign.C.Types                     (CFloat (..))
import           Data.Coerce                         (coerce)
import           Control.Monad.ST.Strict             as ST
import           Control.Monad                       (liftM)
import           Debug.Trace                         (trace)
data AudRate

newtype SF a b = SF
  { runSF :: (a -> (b, SF a b))
  }

newtype ArrowP a p b c = ArrowP
  { strip :: a b c
  }

instance Category a => Category (ArrowP a p) where
  id = ArrowP Control.Category.id
  ArrowP g . ArrowP f = ArrowP (g Control.Category.. f)

instance Arrow a => Arrow (ArrowP a p) where
  arr f = ArrowP (arr f)
  first (ArrowP f) = ArrowP (first f)

instance ArrowLoop a => ArrowLoop (ArrowP a p) where
  loop (ArrowP f) = ArrowP (loop f)
   
instance ArrowCircuit a => ArrowCircuit (ArrowP a p) where
  delay i = ArrowP (delay i)

instance ArrowChoice a => ArrowChoice (ArrowP a p) where
  left (ArrowP f) = ArrowP (left f)
  ArrowP f ||| ArrowP g = ArrowP (f ||| g)

instance Category SF where
  id = SF h
    where
      h x = (x, SF h)
  g . f = SF (h f g)
    where
      h f g x =
        let (y, f') = runSF f x
            (z, g') = runSF g y
        in f' `seq` g' `seq` (z, SF (h f' g'))

instance Arrow SF where
  arr f = g
    where
      g = SF (\x -> (f x, g))
  first f = SF (g f)
    where
      g f (x, z) = f' `seq` ((y, z), SF (g f'))
        where
          (y, f') = runSF f x
  f &&& g = SF (h f g)
    where
      h f g x =
        let (y, f') = runSF f x
            (z, g') = runSF g x
        in ((y, z), SF (h f' g'))
  f *** g = SF (h f g)
    where
      h f g x =
        let (y, f') = runSF f (fst x)
            (z, g') = runSF g (snd x)
        in ((y, z), SF (h f' g'))


instance ArrowLoop SF where
  loop sf = SF (g sf)
    where
      g f x = f' `seq` (y, SF (g f'))
        where
          ((y, z), f') = runSF f (x, z)

instance ArrowChoice SF where
  left sf = SF (g sf)
    where
      g f x =
        case x of
          Left a ->
            let (y, f') = runSF f a
            in f' `seq` (Left y, SF (g f'))
          Right b -> (Right b, SF (g f))

instance ArrowCircuit SF where
  delay i = SF (f i)
    where
      f i x = (i, SF (f x))

type Signal clk a b = ArrowP SF clk a b
type SigFun clk a b = ArrowP SF clk a b

class AudioSample a where
  zero :: a
  mix :: a -> a -> a
  collapse :: a -> [Double]
  numChans :: a -> Int
      
type SEvent a = Maybe a

class Clock p a where
  rate :: p -> A.Exp a
    
data Table a =
  Table !(A.Acc (A.Scalar Int)) 
        !(A.Acc (A.Array A.DIM1 a))
        !(A.Acc (A.Scalar Bool))

instance (A.Elt a) => Show (Table a) where
  show (Table sz a n) = "Table with " ++ show sz ++ " entries: " ++ show a

funToTable ::
     (A.Num a, A.Ord a, A.Fractional a, A.FromIntegral Int a)
  => (A.Exp a -> A.Exp a)
  -> A.Acc (A.Scalar Bool)
  -> A.Acc (A.Scalar Int)
  -> Table a
funToTable f normalize size =
  let xs =
        A.enumFromStepN
          ((A.index1 (A.the size)))
          0
          (A.recip $ A.fromIntegral (A.the size))
      ys = A.map f xs
      zs = A.acond (A.the normalize) (A.map (A./ (maxabs ys)) ys) ys
      maxabs = A.the Prelude.. A.maximum Prelude.. A.map A.abs
  in Table size zs normalize




data Backend
  = LLVM
  | CUDA

data AccSample (i :: Backend) (l :: Nat) a =
  AccSample (A.Acc (A.Array A.DIM1 a)) 
getLength ::
     forall i l a. (KnownNat l)
  => (AccSample i l a)
  -> Int
getLength _ = fromIntegral $ natVal (Proxy :: Proxy l) :: Int

fillLength ::
     (A.Shape (A.Z A.:. l), A.Elt e)
  => l
  -> A.Exp e
  -> A.Acc (A.Array (A.Z A.:. l) e)
fillLength l v =
    A.fill (A.constant (A.Z A.:. l)) v

type Audio a
   = ( A.Elt a
     , A.RealFrac a
     , Fractional (A.Exp a)
     , Num a
     , Fractional a
     , Floating (A.Exp a)
     , A.FromIntegral Int a)

instance forall i l a. (KnownNat l, Audio a) =>
         AudioSample (AccSample i l a) where
  zero =
    let s = fromIntegral $ natVal (Proxy :: Proxy l) :: Int
        z = fillLength s 0.0
    in AccSample z
  mix (AccSample a) (AccSample b) = (AccSample $ A.zipWith (A.+) a b)
  collapse (AccSample a) = [2.0]
  numChans _ = 1

instance forall i l a. (KnownNat l, A.Elt a, Fractional (A.Exp a)) =>
         AudioSample (AccSample i l a, AccSample i l a) where
  zero =
    let s = (fromIntegral $ natVal (Proxy :: Proxy l)) :: Int
        z = A.fill (A.constant (A.Z A.:. s)) 0.0
    in (AccSample z, AccSample z)
  mix (AccSample a, AccSample b) (AccSample c, AccSample d) =
    (AccSample $ A.zipWith (A.+) a c, AccSample $ A.zipWith (A.+) b d)
  collapse (AccSample a, AccSample b) = [2.0]
  numChans _ = 2

runSignal ::
     Interpreter i
  => Signal c b (AccSample i l a)
  -> b
  -> A.Acc (A.Array A.DIM1 a)
runSignal s i =
    let (AccSample r, SF _) = runSF (strip s) i
    in r

class Interpreter i where
  runToStorable ::
       (A.Arrays b, Num a, A.Elt a)
    => Signal c (A.Acc b) (AccSample i l a)
    -> b
    -> AIO.Vectors (AS.EltRepr a)
  evalToStorable ::
       (Num a, A.Elt a)
    => Signal c () (AccSample i l a)
    -> AIO.Vectors (AS.EltRepr a)


instance Interpreter CUDA where
  runToStorable sig n = AIO.toVectors $ AI.run1 (runSignal sig) n
  evalToStorable sig = AIO.toVectors $ AI.run (runSignal sig ()) 

instance Interpreter LLVM where
  runToStorable sig n = AIO.toVectors $ AL.run1 (runSignal sig) n
  evalToStorable sig = AIO.toVectors $ AL.run (runSignal sig ())


wrap
    :: (A.Ord n, Num (A.Exp n))
    =>A.Exp  n -> A.Exp n ->A.Exp  n
wrap val bound =
    A.ifThenElse (val A.> bound)
        (wrap val (val - bound))
        val
clip
    :: Ord (A.Exp n)
    => A.Exp  n ->A.Exp  n ->A.Exp  n ->A.Exp  n
clip val lower upper
  | val <= lower = lower
  | val >= upper = upper
  | otherwise = val



pow
    :: Floating (A.Exp a)
    => A.Exp a -> A.Exp a -> A.Exp a
pow a b = A.exp (A.log a A.* b)


readFromTable
    :: forall i l a.
       (KnownNat l, Audio a)
    => Table a -> A.Exp a -> (AccSample i l a)
readFromTable (Table sz array _) pos =
  let modu =
        A.the $ A.unit (fromIntegral $ natVal (Proxy :: Proxy l)) :: A.Exp Int
      start = A.truncate (A.fromIntegral (A.the sz) A.* pos)
      end = (start + modu)
      wrp =
        A.the $ A.zipWith (A.-) sz (A.zipWith (A.+) (A.unit start) (A.unit end))
      sl = A.drop (start A.+ end) array
  in AccSample $
     A.ifThenElse
       (end A.<= start)
       ((A.take end sl) A.++ (A.take wrp array))
       array

readFromTableA :: (Arrow a, KnownNat l, Audio aa) => Table aa -> a (A.Exp aa) (AccSample i l aa)
readFromTableA = arr Prelude.. readFromTable 

outA :: (Arrow a) => a b b
outA = arr Prelude.id


readFromTable' (Table _ array _) = AccSample array

type Phase a = A.Exp a
type Frequency a = A.Exp a

sineWave ::
     forall c i a l.
     ( Audio a
     , KnownNat l
     , ArrowApply (ArrowP SF c)
     )
  =>Signal c (Frequency a, Phase a) (AccSample i l a)
sineWave  = proc (freq, phs) -> do
             let nrm = A.unit ( A.lift False)
             let amp = A.constant $ 0.9999
             let tbl = funToTable (\x -> amp A.* A.sin (A.constant 2.0 * A.pi * x * freq) ) nrm (A.unit  (A.constant (fromIntegral $ natVal (Proxy :: Proxy l))))
             let rd = (readFromTableA tbl)
             rd -<< phs

-- This cannot be polymorphic in a to unify with the EltRepr a of runToStorable
generateSample :: (A.Arrays npts, Interpreter i) => npts -> Signal c (A.Acc (npts)) (AccSample i l Float) -> DVS.Vector Float
generateSample npts sig = runToStorable sig npts


arrange
 ::  (Storable a, Num a, Show a) => Int -> DVS.Vector a -> SV.Vector CFloat
arrange size evs =
  SVST.runSTVector
    (do v <- SVST.new (fromIntegral size) 0
        unsafeAddChunkToBuffer v evs
        return $ coerce v)


unsafeFrameToQueue ::
     ( KnownNat l, A.Arrays npt, Interpreter i
     )
                 =>Signal c (A.Acc npt) (AccSample i l Float) -> npt-> TBQueue (DVS.Vector Float) -> Int  -> IO ()
unsafeFrameToQueue fn npt queue ln = go 100 ln (generateSample npt fn)
  where
    go (c :: Int) l vc
      | c > 0 = do
        let (h, t) = (DVS.unsafeTake l vc, DVS.unsafeDrop l vc)
        atomically $ writeTBQueue queue h
        go (c - 1) l t
      | otherwise = go 100 ln (generateSample npt fn)
        
processAudio queu ln =
  unsafeFrameToQueue sineWave (A.constant 440.0, A.constant 0)

  
instance Clock AudRate Float where
    rate _ = A.constant 96000

unsafeAddChunkToBuffer :: (Storable a, Num a, Show a ) =>
   SVST.Vector s a -> DVS.Vector a -> ST s ()
unsafeAddChunkToBuffer v xs =
  let go i j =
        if j >= DVS.length xs
          then return ()
          else SVST.unsafeModify v i ((xs DVS.! j) +) >> go (i + 1) (j + 1)
  in go 0 0
