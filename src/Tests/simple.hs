module Main where
import qualified Sound.JACK                          as JACK
import qualified Sound.JACK.Audio                    as Audio
import Sound.JACK.Private (Client, )
import Sound.JACK (Direction, Input, Output, )

import qualified Sound.JACK.Exception as JackExc
import qualified Sound.JACK.FFI as JackFFI
import Sound.JACK.FFI (NFrames, nframesIndices, nframesBounds, )

import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class as Trans
import qualified Data.Array.Accelerate               as A
import qualified Data.Array.Accelerate.Array.Sugar   as A
import qualified Data.Array.Accelerate.IO            as A
import Foreign.ForeignPtr (newForeignPtr_, )
import Foreign.Ptr (Ptr, )
import Foreign.C.Error (Errno, )
import qualified Foreign.C.Types as C

import System.Environment (getProgName)
import Lib
import Data.Array.Storable (StorableArray, readArray, writeArray, )
import Data.Array.Unsafe (unsafeForeignPtrToStorableArray, )
import qualified Sound.JACK.Audio as Audio
import qualified Sound.JACK as JACK
import qualified Data.StorableVector                 as SV
import qualified Data.StorableVector.ST.Strict       as SVST
import qualified Data.StorableVector.Base            as SVB
import qualified Data.Vector.Storable                as DVS
import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class as Trans
import qualified Foreign.C.Error as E

import System.Environment (getProgName)

import Data.Array.Storable (writeArray, )

type Sample = C.CFloat

type Port = Priv.Port Sample

instance Clock AudRate Float where
    rate _ = A.constant 96000

gen ::  Signal (AudRate) (A.Acc (A.Array A.DIM1 Float)) (AccSample LLVM 96000 Float)
        -> DVS.Vector Float
gen sig =  runToStorable sig (A.fromList (A.Z A.:. 1) [0])

renderTone rt = coerce $ gen $ sineWave rt

        
check :: Monad m => Bool -> String -> m () -> m ()
check b msg act =
    if not b
        then trace msg $ return ()
        else act
             
arrange
 ::  (Storable a, Num a, Show a) => Size -> [(Int, DVS.Vector a)] -> SV.Vector CFloat
arrange size evs =
    let vec =
          SVST.runSTVector
          (do v <- SVST.new (fromIntegral size) 0
              mapM_ (uncurry $ unsafeAddChunkToBuffer v) evs
              return $ coerce v)
        _ = trace $ "vec: " ++ show vec
    in vec
    
unsafeAddChunkToBuffer :: (Storable a, Num a, Show a ) =>
   SVST.Vector s a -> Int -> DVS.Vector a -> ST s ()
unsafeAddChunkToBuffer v start xs =
    let go i j =
            if j >= DVS.length xs
                then return ()
                else SVST.unsafeModify v i ( (xs DVS.! j) +) >>
                     go (i + 1) (j + 1)
    in 
       check (start >= 0) ("start negative: " ++ show (start, DVS.length xs)
                          ) $
       check
           (start <= SVST.length v)
           ("start too late: " ++ show (start, DVS.length xs)
           ) $
       check
           (start + DVS.length xs <= SVST.length v)
           ("end too late: " ++ show (start, DVS.length xs, DVS.length xs)
           ) $
       go start 0


mainWait ::
    JackExc.ThrowsErrno e =>
    Jack.Client -> String -> Sync.ExceptionalT e IO ()
mainWait client name =
    Jack.withActivation client $ Trans.lift $ do
        putStrLn $ "started " ++ name ++ "..."
        Jack.waitForBreak
        
main :: IO ()
main = do
    name <- getProgName
    JACK.handleExceptions $
        JACK.withClientDefault name $ \client ->
        JACK.withPort client "output" $ \output ->
        JACK.withProcess client (process output) $
            mainWait client name

process ::
    Audio.Port JACK.Output -> JACK.NFrames -> Sync.ExceptionalT E.Errno IO ()
process output nframes = Trans.lift $ do
    outArr <- Audio.getBufferArray output nframes
    case JACK.nframesIndices nframes of
        [] -> return ()
        zero:rest -> do
            writeArray outArr zero 1
            mapM_ (\i -> writeArray outArr i 0) rest


