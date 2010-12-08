-----------------------------------------------------------------------------
-- |
-- Module      :  AFRPInternals
-- Copyright   :  (c) Yale University, 2003
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  antony@apocalypse.org
-- Stability   :  provisional
-- Portability :  non-portable (uses GHC extensions)
--
-- An interface giving access to some of the internal
-- details of the AFRP implementation.
--
-- This interface is indended to be used when the need arises to break
-- abstraction barriers, e.g. for interfacing AFRP to the real world, for
-- debugging purposes, or the like. Be aware that the internal details
-- may change. Relying on this interface means that your code is not
-- insulated against such changes.

module AFRPInternals (
    Event(..)		-- The event type, its constructors, and instances.
) where

import AFRPEvent


------------------------------------------------------------------------------
-- Extra Event instances
------------------------------------------------------------------------------

instance Show a => Show (Event a) where
    showsPrec d NoEvent   = showString "NoEvent"
    showsPrec d (Event a) = showParen (d >= 10)
				      (showString "Event " . showsPrec 10 a)


