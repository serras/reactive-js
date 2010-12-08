-----------------------------------------------------------------------------
-- |
-- Module      :  AFRPDiagnostics
-- Copyright   :  (c) Yale University, 2003
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  antony@apocalypse.org
-- Stability   :  provisional
-- Portability :  non-portable (uses GHC extensions)
--
-- Standardized error-reporting for AFRP 
--
module AFRPDiagnostics where

usrErr mn fn msg = error (mn ++ "." ++ fn ++ ": " ++ msg)

intErr mn fn msg = error ("[internal error] " ++ mn ++ "." ++ fn ++ ": "
                          ++ msg)
