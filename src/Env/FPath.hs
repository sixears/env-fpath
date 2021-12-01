module Env.FPath
  ( envAbsFile, envRcAbsFile, getAbsPath, getPathD, getPathD_, getPath, getPath_
  , getPath', getEnvAbsPathD, getEnvPathD, pathExe, pathExe', resolveAbsPathD
  , resolvePathD

  , tests
  )
where

-- base --------------------------------

import Control.Monad  ( filterM, mapM, join, return, sequence )
import Data.Function  ( ($) )
import Data.Functor   ( fmap )
import Data.Either    ( partitionEithers, rights )
import Data.Maybe     ( fromMaybe, maybe )
import Data.Tuple     ( snd )
import System.Exit    ( ExitCode )
import System.IO      ( IO )

-- base-unicode-symbols ----------------

import Data.Function.Unicode    ( (∘) )
import Numeric.Natural.Unicode  ( ℕ )

-- data-textual ------------------------

import Data.Textual  ( toString )

-- env-plus ----------------------------

import Env         ( getEnv )
import Env.Reader  ( EnvReader, envLookupM )
import Env.Types   ( Env, EnvKey, fromList )

-- fpath -------------------------------

import FPath.AbsDir            ( AbsDir, parseAbsDirP )
import FPath.AbsFile           ( AbsFile, absfile )
import FPath.AppendableFPath   ( (⫻) )
import FPath.Error.FPathError  ( AsFPathError, FPathError, FPathIOError )
import FPath.Parseable         ( parse )
import FPath.RelFile           ( RelFile, relfile )

-- monaderror-io -----------------------

import MonadError           ( ѥ )
import MonadError.IO        ( eitherIOThrowT )
import MonadError.IO.Error  ( AsIOError )

-- monadio-plus ------------------------

import MonadIO        ( MonadIO )
import MonadIO.Cwd    ( getCwd' )
import MonadIO.File   ( AccessMode( ACCESS_X ), access )
import MonadIO.FPath  ( pResolveDir )
import MonadIO.User   ( homeDir, homePath )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool     ( pattern 𝕱 )
import Data.MoreUnicode.Either   ( 𝔼, pattern 𝕷, pattern 𝕽 )
import Data.MoreUnicode.Functor  ( (⊳), (⩺) )
import Data.MoreUnicode.Maybe    ( 𝕄, pattern 𝕵, pattern 𝕹 )
import Data.MoreUnicode.Monad    ( (≫) )
import Data.MoreUnicode.String   ( 𝕊 )
import Data.MoreUnicode.Text     ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )
import Control.Monad.Reader  ( runReaderT )

-- safe --------------------------------

import Safe  ( headMay )

-- split -------------------------------

import Data.List.Split  ( splitOn )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup, withResource )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( runTestsP, runTestsReplay, runTestTree )

--------------------------------------------------------------------------------

{- | Get some path from the environment as a list of absolute dirs,
     resolving relative to a given directory.  A missing env var. is treated as
     empty. -}
resolvePathD ∷ ∀ ε μ . (MonadIO μ, AsIOError ε, AsFPathError ε) ⇒
              𝕊 → AbsDir → μ ([ε], [AbsDir])
resolvePathD s d = do
  let ps = splitOn ":" s
  join $ return (partitionEithers ⊳ sequence ((ѥ ∘ pResolveDir d) ⊳ ps))

----------------------------------------

{- | Get some path from the environment as a list of absolute dirs, dropping all
     relative components. -}
resolveAbsPathD ∷ ∀ μ . MonadIO μ ⇒ 𝕊 → μ [AbsDir]
resolveAbsPathD s = return ∘ rights $ parseAbsDirP @FPathError ⊳ splitOn ":" s

----------------------------------------

{- | Get some path from the environment as a list of absolute dirs,
     resolving relative to a given directory.  A missing env var. is treated as
     empty. -}
getEnvPathD ∷ ∀ ε μ . (MonadIO μ, AsIOError ε, AsFPathError ε) ⇒
              EnvKey → AbsDir → μ ([ε], [AbsDir])
getEnvPathD e d = toString ⊳ fromMaybe "" ⊳ getEnv e ≫ \ s → resolvePathD s d

----------------------------------------

{- | Get some path from the environment as a list of absolute dirs, dropping all
     relative components. -}
getEnvAbsPathD ∷ ∀ μ . MonadIO μ ⇒ EnvKey → μ [AbsDir]
getEnvAbsPathD e = toString ⊳ fromMaybe "" ⊳ getEnv e ≫ resolveAbsPathD

----------------------------------------

{- | Get the PATH, dropping all non-absolute dirs. -}
getAbsPath ∷ ∀ μ . MonadIO μ ⇒ μ [AbsDir]
getAbsPath = do
  ps ← splitOn ":" ∘ toString ⊳ fromMaybe "" ⊳ getEnv "PATH"
  return ∘ rights $ parseAbsDirP @FPathError ⊳ ps

----------------------------------------

{- | Get the PATH as a list of absolute dirs, resolving relative to a given
     directory. -}
getPathD ∷ ∀ ε μ . (MonadIO μ, AsIOError ε, AsFPathError ε) ⇒
           AbsDir → μ ([ε], [AbsDir])
getPathD = getEnvPathD "PATH"

----------------------------------------

{- | Get the PATH as a list of absolute dirs, resolving relative to a given
     directory; dropping any parse errors. -}
getPathD_ ∷ MonadIO μ ⇒ AbsDir → μ [AbsDir]
getPathD_ = snd ⩺ getPathD @FPathIOError

----------------------------------------

{- | Get the PATH as a list of absolute dirs, resolving relative to the current
     working directory. -}
getPath ∷ ∀ ε ε' μ .
          (MonadIO μ,
           AsIOError ε , AsFPathError ε,
           AsIOError ε', AsFPathError ε',
           MonadError ε' μ) ⇒
          μ ([ε], [AbsDir])
getPath = getCwd' ≫ getPathD

--------------------

{- | Get the PATH as a list of absolute dirs, resolving relative to the current
     working directory; dropping any parse errors. -}
getPath_ ∷ ∀ ε μ . (MonadIO μ, AsIOError ε, AsFPathError ε, MonadError ε μ) ⇒
           μ [AbsDir]
getPath_ = snd ⊳ getPath @FPathIOError

----------------------------------------

{- | Get the PATH as a list of absolute dirs, resolving relative to the current
     working directory.  If looking up the cwd causes an error, that error is
     returned; but so is the path, ignoring all relative components.
-}
getPath' ∷ ∀ ε μ . (MonadIO μ, AsFPathError ε, AsIOError ε) ⇒ μ (𝕄 ε, [AbsDir])
getPath' = ѥ getCwd' ≫ \ case
             𝕷 e → (𝕵 e,) ⊳ getAbsPath
             𝕽 d → (𝕹,) ⊳ getPathD_ d

----------------------------------------

{- Find an executable on the path.  Any wacky error resolving the cwd is
   returned, and resolution continues ignoring relative components in the path
   (see `getPath'`). The file returned, if any, is executable by the current
   user (see `access`).
 -}
pathExe ∷ ∀ ε ε' μ .
          (MonadIO μ, AsIOError ε, AsFPathError ε, MonadError ε μ,
           AsIOError ε', AsFPathError ε') ⇒
          RelFile → μ (𝕄 ε', 𝕄 AbsFile)
pathExe r = do
  (e, path) ← getPath'
  fs        ← mapM (`pResolveDir` r) path
  fs'       ← filterM (fmap (fromMaybe 𝕱) ∘ access ACCESS_X) fs
  return (e,headMay fs')

{- | Like pathExe', but throws in case of cwd resolution error. -}
pathExe' ∷ ∀ ε μ .
          (MonadIO μ, AsIOError ε, AsFPathError ε, MonadError ε μ) ⇒
          RelFile → μ (𝕄 AbsFile)
pathExe' r =
  pathExe r ≫ \ case
    (𝕹, mf) → return mf
    (𝕵 e, _) → throwError e

----------------------------------------

{- | Get an absfile from the environment. -}
envAbsFile ∷ ∀ ε μ . (AsFPathError ε, MonadError ε μ, EnvReader μ) ⇒
             EnvKey → μ (𝕄 AbsFile)
envAbsFile = envLookupM parse

----------------------------------------

{- | Find an RC file, if possible.  @envKey@ is the name of the environment
     variable to look up (expected to be a full file name).  Failing the
     existence of that envvar, the fn returns the absolute name of @homeRel@
     relative to the user's home directory.
 -}
envRcAbsFile ∷ ∀ ε μ . (MonadIO μ, EnvReader μ,
                        AsFPathError ε, AsIOError ε, MonadError ε μ) ⇒
               EnvKey → RelFile → μ AbsFile
envRcAbsFile k r = envAbsFile k ≫ maybe (homePath r) return

envRcAbsFileTests ∷ TestTree
envRcAbsFileTests =
  withResource (eitherIOThrowT $ homeDir @FPathIOError) (\ _ → return ()) etests
  where
    etests ∷ IO AbsDir → TestTree
    etests d =
      let go ∷ Env → EnvKey → IO (𝔼 FPathIOError AbsFile)
          go env envKey    = ѥ $ runReaderT (envRcAbsFile envKey relFoo) env
          envFoo           = [absfile|/bar|]
          homeFoo hd       = hd ⫻ [relfile|foo|]
          relFoo           = [relfile|foo|]
          invalAbsBar      = e where 𝕷 e = parse @AbsFile ("bar" ∷ 𝕋)
          myEnv            = fromList [("HOME", "/my/home/"), ("FOO", "/bar")]
          myEnvNoHome      = fromList [("FOO", "/bar")]
          myEnvFooNotAbs   = fromList [("HOME", "/my/home/"), ("FOO", "bar")]
          myEnvHomeNotAbs  = fromList [("HOME", "my/home/"), ("FOO", "/bar")]
          myEnvNoHomeSlash = fromList [("HOME", "/my/home"), ("FOO", "/bar")]
      in testGroup "envRcAbsFileTests"
            [ testCase "envRcFile; fall back to $HOME" $
                d ≫ \ hd → go myEnv "NONSUCH"  ≫ (𝕽 (homeFoo hd) @=?)
            , testCase "envRcFile; success" $
                go myEnv "FOO" ≫ (𝕽 envFoo @=?)
            , testCase "envRcFile; no $HOME (but uses FOO)" $
                go myEnvNoHome "FOO" ≫ (𝕽 envFoo @=?)
            , testCase "envRcFile; FOO names invalid absolute file" $
                go myEnvFooNotAbs "FOO" ≫ (𝕷 invalAbsBar @=?)
            , testCase "envRcFile; unused FOO names invalid absolute file" $
                d ≫ \ hd → go myEnvFooNotAbs "BAR" ≫ (𝕽 (homeFoo hd) @=?)
            , testCase "envRcFile; unused HOME not absolute dir" $
                go myEnvHomeNotAbs "FOO" ≫ (𝕽 envFoo @=?)
            , testCase "envRcFile; fall back to $HOME; no $HOME slash" $
                d ≫ \ hd → go myEnvNoHomeSlash "NONSUCH" ≫ (𝕽 (homeFoo hd) @=?)
            , testCase "envRcFile; success; no $HOME slash" $
                go myEnvNoHomeSlash "FOO" ≫ (𝕽 envFoo @=?)
            ]

-- that's all, folks! ----------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Env.FPath" [ envRcAbsFileTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests
