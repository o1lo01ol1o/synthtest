{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module State where
import qualified Algebra.Graph.AdjacencyMap    as G
import qualified Algebra.Graph.Class as CG
import           Data.Complex                   ( Complex(..) )
import           Data.Proxy                     ( Proxy(..) )
import qualified Data.Dependent.Map            as DM
import qualified Data.Vector.Storable.Sized    as DVS
import           Data.Vector.Storable.Sized     ( (++) )
import qualified Data.Vector.Sized as DV
import qualified Data.Set                      as Set
import qualified Data.Map                      as Map
import           GHC.TypeLits
import           Type.Reflection                ( Typeable
                                                , TypeRep
                                                , someTypeRep
                                                )
import           Data.Bifunctor
import           Data.Coerce                    ( coerce )
import           Prelude                 hiding ( (++) )
-- import           Control.Lens.TH                ( makeLenses )

-- TODO: allow cycles by ordering graphs of cliques by clique order. where
-- computing the order the determines the fewest times the signal paths needs to
-- propagated to get one "feedback cycle"
data Connectable a
  = Input a
  | Output a
  | Control
  deriving (Functor, Show, Eq, Ord)
 
type EdgeLabelList o
   = Map.Map ((SomeModule o), (SomeModule o)) (Set.Set (Int, Int))

-- | Module topology 
data Topology' o = Topology 
  { _am :: G.AdjacencyMap (SomeModule o)
  , _el :: EdgeLabelList o
  } deriving (Eq, Show)

empty = Topology CG.empty Map.empty
vertex v = CG.vertex v
overlay (Topology ama ela) (Topology amb elb) =
  Topology (G.overlay ama amb) (Map.unionWith Set.union ela elb)

-- | connects two typologies with a label between all vertices.
connect :: (Topology' o) -> (Topology' o) -> (Int,Int) -> (Topology' o)
connect (Topology ama ela) (Topology amb elb) l = Topology (G.connect ama amb) els
  where
    uel = (Map.unionWith Set.union ela elb)
    insrt p = Map.insert p (Set.singleton l) uel
    els =
      Map.unionsWith Set.union $
      [ insrt (i, j)
      | i <- Set.toList $ G.vertexSet ama
      , j <- Set.toList $ G.vertexSet amb
      ]


mkTopology sm = Topology (G.vertex sm) Map.empty
-- | Wrapper to disambiguate multiple instancesof the same`SignalmModule`
newtype IDed (i :: Nat) a =
  IDed a
  deriving (Show, Eq, Ord, Functor)

newtype FloatParamIdx =
  FloatParamIdx [Int]
  deriving (Show, Eq, Ord)

newtype IntParamIdx =
  IntParamIdx [Int]
  deriving (Show, Eq, Ord)

newtype BoolParamIdx =
  BoolParamIdx [Int]
  deriving (Show, Eq, Ord)

newtype ComplexParamIdx =
  ComplexParamIdx [Int]
  deriving (Show, Eq, Ord)


data ParamIdxOffset = ParamIdxOffset
  { _piBool   :: !BoolParamIdx
  ,_piFloat   :: !FloatParamIdx
  ,_piInt     :: !IntParamIdx
  ,_piComplex :: !ComplexParamIdx
  }

data StateIdxOffset = StateIdxOffset
  {_siBool    :: !BoolParamIdx
  ,_siFloat   :: !FloatParamIdx
  ,_siInt     :: !IntParamIdx
  ,_siComplex :: !ComplexParamIdx
  }

-- | 'SignalModule a o's define the interface that lets up compose Accelerate functions from a 'Topology o' where 'o' is the type of the outputs of the module.
class SignalModule a o where
  type NumInputs a :: Nat
  -- ^ The number of inputs of type 'o' defined by the module
  type NumOutputs a :: Nat
  -- ^ The number of outputs of type 'o' produced by the module
  type BooleanParams a :: Nat
  -- ^ The number of boolean parameters needed by the module
  type IntParams a :: Nat
  -- ^ The number of integral parameters needed by the module
  type FloatParams a :: Nat
  -- ^ The number of float parameters needed by the module
  type ComplexParams a :: Nat
  -- ^ The number of complex parameters needed by the module
  initStates :: Proxy a -> Proxy o -> Maybe (ParamArrays r)
  initParams :: Proxy a -> Proxy o -> ParamVecs' a
  sigFun ::
       Proxy a
    -> ParamVecs' a -- ^ The (haskell) parameters for this function
    -> ParamArrays r -- ^ The (accelerate) program-wide parameters and states
    -> SigInputs a o -- ^ The (optional, acclerate) inputs of type `o`
    -> (Maybe ParamIdxOffset, Maybe StateIdxOffset) -- ^ The indicies of the needed parameters and states into `ParamVecs bl il fl cl`
    -> (SigOutputs a o, Maybe (ParamArrays r)) -- ^ The outputs (and optional states) of the signal function

type SigInputs a o = DV.Vector (NumInputs a) (Maybe o)
type SigOutputs a o = DV.Vector (NumOutputs a) (Maybe o)

type KnownLengths a
   = ( KnownNat (BooleanParams a)
     , KnownNat (IntParams a)
     , KnownNat (FloatParams a)
     , KnownNat (ComplexParams a))

type ParamVecs' a = ParamVecs (BooleanParams a) (IntParams a) (FloatParams a) (ComplexParams a)

-- | Wrapped 'SignalModule'
data SomeModule o where
  SomeModule
    :: forall a o.
       (SignalModule a o, Eq a, Ord a, Show a, Typeable a, KnownLengths a)
    => Int -> a
    -> ParamVecs' a
    -> SigInputs a o
    -> SomeModule o

instance (Show o) => Show (SomeModule o) where
  show (SomeModule i s _ _) =
    "SomeModule (" <> "Id: " <> show i <> ", Module: " <> show s <> ")"


plugIn (SomeModule iD a pv si) i v = (SomeModule iD a pv u)
        where u = DV.update si (DV.singleton (i, v))


-- FIXME: This is only propositional equality between a and b: (a ~ b)
instance Eq (SomeModule o) where
  (==) = heq
    where
      heq :: (SomeModule o) -> (SomeModule o) -> Bool
      heq (SomeModule i _ _ _) (SomeModule j _ _ _) = i == j


instance Ord (SomeModule o) where
  compare (SomeModule i _ _ _) (SomeModule j _ _ _) = i `compare` j



-- | The state of the module graph
data State a bl il fl cl = State
  { _topology :: !(Topology' a)
  , _paramVecs :: !(ParamVecs bl il fl cl)
  }

-- | Vectors containing (Haskell) parameters and states for 'SignalModule's
data ParamVecs bl il fl cl = ParamVecs
  { _pvBool :: !(DVS.Vector bl Bool)
  , _pvInt :: !(DVS.Vector il Int)
  , _pvFloat :: !(DVS.Vector fl Float)
  , _pvComplex :: !(DVS.Vector cl (Complex Float))
  } deriving (Show, Eq)

-- instance (Ord a) => Ord (Complex a) where
--   compare (ar :+ ai) (br :+ bi)
--     | ar == br && ai == bi = EQ
--     | ar < br = LT
--     | br < ar = GT
--     | ai < bi = LT
--     | otherwise = GT




data ParamOffSets = ParamOffSets
  { _poBool    :: !Int
  , _poInt     :: !Int
  , _poFloat   :: !Int
  , _poComplex :: !Int
  }


instance Semigroup ParamOffSets where
  (<>) (ParamOffSets a b c d) (ParamOffSets a' b' c' d') =
    ParamOffSets (a + a') (b + b') (c + c') (d + d')


instance Monoid ParamOffSets where
  mempty = ParamOffSets 0 0 0 0
  mappend = (<>)


concatParams
        :: ParamVecs bl il fl cl
        -> ParamVecs bl' il' fl' cl'
        -> ParamVecs (bl + bl') (il + il') (fl + fl') (cl + cl')
concatParams (ParamVecs a b c d) (ParamVecs a' b' c' d') = z
        where z = ParamVecs (a ++ a') (b ++ b') (c ++ c') (d ++ d')

-- |  Vectors containing (Accelerate) parameters and states for 'SignalModule's
data ParamArrays r = ParamArrays
  { _paBool    :: !(r Bool)
  , _paInt     :: !(r Int)
  , _paFloat   :: !(r Float)
  , _paComplex :: !(r (Complex Float))
  }


cycles :: Ord a => G.AdjacencyMap a -> [G.AdjacencyMap a]
cycles x = [ G.induce (`Set.member` c) x | c <- cs ]
        where cs = filter (\c -> Set.size c > 1) $ G.vertexList (G.scc x)

-- concat <$> traverse makeLenses [

--   ''ParamIdxOffset,
--   ''StateIdxOffset,
--   ''ParamVecs,
--   ''ParamArrays,
--   ''State
--                       ]

data SineOsc = SineOsc deriving (Show, Eq, Ord)

instance SignalModule SineOsc o where
  type NumInputs SineOsc = 0
  type NumOutputs SineOsc = 1
  type BooleanParams SineOsc = 0
  type IntParams SineOsc = 0
  type FloatParams SineOsc = 1
  type ComplexParams SineOsc = 0
  sigFun = undefined

data VCA = VCA deriving (Show, Eq, Ord)

instance SignalModule VCA o where
  type NumInputs VCA = 2
  type NumOutputs VCA = 1
  type BooleanParams VCA = 0
  type IntParams VCA = 0
  type FloatParams VCA = 0
  type ComplexParams VCA = 0
  sigFun = undefined

instance SignalModule NullOrigin o where
  type NumInputs NullOrigin = 0
  type NumOutputs NullOrigin = 0
  type BooleanParams NullOrigin = 0
  type IntParams NullOrigin = 0
  type FloatParams NullOrigin = 0
  type ComplexParams NullOrigin = 0
  sigFun = undefined

data NullOrigin = NullOrigin deriving (Show, Eq, Ord)

mkId :: a -> IDed 0 a
mkId = IDed

incrId :: IDed i a -> IDed (i + 1) a
incrId = coerce

constIncrId :: IDed i1 b -> IDed i2 a -> IDed (i2 + 1) b
constIncrId = const . coerce

orgNd :: Topology' o
orgNd =
        mkTopology (SomeModule (negate 1) (NullOrigin) emptyParams DV.empty)

emptyParams :: ParamVecs 0 0 0 0
emptyParams = ParamVecs @0 @0 @0 @0 DVS.empty DVS.empty DVS.empty DVS.empty


osc :: Topology' o
osc = mkTopology osc'

osc' :: (SomeModule o)
osc' = SomeModule 0 (SineOsc) params DV.empty
  where
    params =
      ParamVecs @0 @0 @1 @0 DVS.empty DVS.empty (DVS.singleton 440.0) DVS.empty
vca :: Topology' o
vca =
  mkTopology $
  SomeModule 1 VCA params (DV.singleton Nothing DV.++ DV.singleton Nothing)
  where
    params = emptyParams


g' :: Topology' a
g' = overlay a b
  where
    org2Osc = connect orgNd osc (0, 0)
    osc2Vca = connect osc vca (1, 1)
    org2vca = connect orgNd vca (0, 0)
    a = overlay org2Osc osc2Vca
    b = overlay osc2Vca org2vca



-- isValidTopology' :: Topology a -> Bool
-- isValidTopology' = isConnected
--     where
--         isConnected = \case
--                 (G.Connect (G.vertex x) (G.vertex y)) ->
--                         case (snd . unPlug $ x, snd . unPlug $ y) of
--                                 (Output _, Input _) -> True
--                                 (Control , _      ) -> True
--                                 _                   -> False
--                 (G.Connect a b) -> isConnected a && isConnected b
--                 (G.Overlay a b) -> isConnected a && isConnected b
--                 _               -> True

