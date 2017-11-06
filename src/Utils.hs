{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Utils (frac) where
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Smart as AS hiding (Exp)
import qualified Prelude as P

defaultProperFraction
    :: (A.ToFloating P.Int b, A.RealFrac b, A.Floating b)
    => A.Exp b
    -> (A.Exp P.Int, A.Exp b)
defaultProperFraction x =
  AS.untup2 P.$ A.cond (x A.== 0) (AS.tup2 (0, 0)) (AS.tup2 (n, f))
  where
    n = A.truncate x
    f = x A.- A.toFloating n
    
frac :: (A.RealFrac a, A.Floating a, A.ToFloating P.Int a) => A.Exp a -> A.Exp a
frac = P.snd P.. defaultProperFraction

