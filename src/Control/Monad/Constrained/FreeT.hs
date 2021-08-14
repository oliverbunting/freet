
{-|
Module      : Control.Monad.Constrained.FreeT
Description : FreeT
Copyright   : (c) Oliver Bunting, 2021
License     : MIT
Maintainer  : oliverbunting@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Control.Monad.Constrained.FreeT
(
    -- * Types
    FreeT(..),
)
where

import Data.Functor ( Functor(..) )
import Control.Applicative ( Applicative(..) )
import Control.Monad ( Monad(..) )
import Prelude ( ($), (.) )

-- | The free monad transformer, with constraint kinds
--
--  See
--
--     - http://okmij.org/ftp/Haskell/set-monad.html#set-cps
--     - https://doisinkidney.com/posts/2017-03-08-constrained-applicatives.html
--
data FreeT c m a where
       FreeT :: (forall r. c r => (a -> m r) ->  m r) ->  FreeT c m a


instance Functor (FreeT r m) where
    fmap f (FreeT m) = FreeT (\ c -> m (c . f))
    {-# INLINE fmap #-}


instance Applicative (FreeT r m) where
    pure x  = FreeT ($ x)
    {-# INLINE pure #-}
    (FreeT f) <*> (FreeT v) = FreeT (\ c -> f $ \ g -> v (c . g))
    {-# INLINE (<*>) #-}


instance Monad (FreeT c m) where
    (FreeT m) >>= k = FreeT (\ c -> m (\ x -> case k x of (FreeT c') -> c' c))
    {-# INLINE (>>=) #-}

