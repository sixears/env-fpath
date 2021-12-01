{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Env.FPathError
  ( AsEnvPathError, EnvPathError( NotOnPath ), _EnvPathError, _NotOnPath
  , notOnPathError, throwNotOnPathError )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Data.Either        ( Either( Right ) )
import Data.Eq            ( Eq )
import Data.Function      ( id )
import Text.Show          ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- fpath --------------------------------

import FPath.AsFilePath  ( filepath )
import FPath.RelFile     ( RelFile )

-- lens --------------------------------

import Control.Lens.Prism   ( Prism', prism )
import Control.Lens.Review  ( (#) )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⫥) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

data EnvPathError = NotOnPath RelFile
  deriving Eq

class AsEnvPathError ε where
  _EnvPathError ∷ Prism' ε EnvPathError
  _NotOnPath    ∷ Prism' ε RelFile
  _NotOnPath    = _EnvPathError ∘ _NotOnPath

instance AsEnvPathError EnvPathError where
  _EnvPathError = id
  _NotOnPath = prism NotOnPath ( \ (NotOnPath fn) → Right fn )

instance Show EnvPathError where
  show (NotOnPath fn) = [fmt|not found on path: '%s'|] (fn ⫥ filepath)

instance Exception EnvPathError

notOnPathError ∷ AsEnvPathError ε ⇒ RelFile → ε
notOnPathError fn = _EnvPathError # NotOnPath fn

throwNotOnPathError ∷ (AsEnvPathError ε, MonadError ε μ) ⇒ RelFile → μ α
throwNotOnPathError = throwError ∘ notOnPathError

-- that's all, folks! ----------------------------------------------------------
