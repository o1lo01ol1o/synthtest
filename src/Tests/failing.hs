{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Main where
import qualified Control.Monad.Exception.Synchronous
                                               as Sync
import qualified Control.Monad.Trans.Class     as Trans
import qualified Control.Monad.Trans.State.Strict
                                               as MS
import qualified Data.Array.Accelerate         as A
import qualified Data.Array.Accelerate.Array.Sugar
                                               as A
import qualified Data.Array.Accelerate.IO.Data.Vector.Storable
                                               as A
import           Data.Coerce                    ( coerce )
import qualified Data.EventList.Absolute.TimeBody
                                               as EventList
import           Data.List                      ( intersperse )
import qualified Data.StorableVector.Base      as SVB
import qualified Data.Vector.Storable          as DVS
import qualified Foreign.C.Error               as E
import           Foreign.C.Types                ( CFloat(..) )
import           Foreign.Marshal.Array          ( copyArray )
import           GHC.TypeLits                   ( KnownNat )
import qualified Sound.JACK                    as JACK
import qualified Sound.JACK.Audio              as Audio
import qualified Sound.JACK.MIDI               as MIDI
import qualified Sound.MIDI.Message            as Msg
import qualified Sound.MIDI.Message.Channel    as ChannelMsg
import qualified Sound.MIDI.Message.Channel.Voice
                                               as VoiceMsg
import           System.Environment             ( getProgName )
import           System.Exit                    ( exitFailure )

import           Data.IORef                     ( IORef
                                                , newIORef
                                                , readIORef
                                                , writeIORef
                                                )

import           Sound.MIDI.Message.Channel.Voice
                                                ( Pitch )
import qualified Sound.MIDI.Message.Channel.Voice
                                               as VoiceMsg

import qualified Data.StorableVector           as SV
import qualified Data.StorableVector.ST.Strict as SVST

import           Control.Monad.ST.Strict       as ST
import           Foreign.Storable               ( Storable )

import qualified Data.Map                      as Map

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM  hiding ( check )
import           Control.Concurrent.STM.TBQueue
import           Control.Monad                  ( liftM )
import           Debug.Trace                    ( trace )
import           Lib
import           System.IO                      ( stderr )



data State a = State
  { _gphase :: Int
  , _ctrlRate :: Int
  , _accParams :: A.Acc (A.Array A.DIM1 a)
  }




mainWait client name = JACK.withActivation client $ Trans.lift $ do
        putStrLn $ "started " ++ name ++ "..."
        JACK.waitForBreak

initialState :: (A.Elt a, Num a) => (State a)
initialState = State 1 100 (A.use $ A.fromList (A.Z A.:. 1) [0])

main :: IO ()
main = do
        name       <- getProgName
        frameQueue <- newTBQueueIO 960
        asc        <- async $ unsafeFrameToQueue frameQueue 960
        JACK.handleExceptions $ JACK.withClientDefault name $ \client ->
                JACK.withPort client "output" $ \output ->
                        JACK.withProcess
                                        client
                                        (process client frameQueue asc output)
                                $ mainWait client name

intFromNFrames :: Integral i => JACK.NFrames -> i
intFromNFrames (JACK.NFrames n) = fromIntegral n

process
        :: JACK.Client
        -> TBQueue (DVS.Vector Float)
        -> Async ()
        -> Audio.Port JACK.Output
        -> JACK.NFrames
        -> Sync.ExceptionalT E.Errno IO ()
process client frameQueue asc output nframes = Trans.lift $ do
        rt     <- JACK.getSampleRate client
        outArr <- Audio.getBufferPtr output nframes
        dv     <- atomically $ readTBQueue frameQueue
        let state = coerce $ arrange (intFromNFrames nframes) dv
        SVB.withStartPtr state $ \src len -> copyArray outArr src len
           -- c <- poll asc
           -- case c of
           --   Just v -> putStrLn $ show v
           --   Nothing -> return ()
