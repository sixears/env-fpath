{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Fluffy.T.EnvReader
  ( tests )
where

import Prelude ( )

-- base --------------------------------

import Control.Monad  ( (>>=), return )
import Data.Either    ( Either( Left, Right ) )
import Data.Function  ( ($) )
import System.IO      ( IO )

-- mtl ---------------------------------

import Control.Monad.Reader  ( runReaderT )

-- path --------------------------------

import Path  ( (</>), absfile, relfile )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, defaultMain, testGroup, withResource )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@?=), testCase )

-- text --------------------------------

import Data.Text  ( Text )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Fluffy.Env.Types     ( EnvKey )
import Fluffy.EnvReader     ( Env( Env ), envRcAbsFile )
import Fluffy.MonadError    ( splitMError )
import Fluffy.Path          ( AbsDir, AbsFile, parseAbsFile )
import Fluffy.Path.Error    ( IOPathEnvError( IPE_PATH_ERROR ) )
import Fluffy.MonadIO.User  ( homeDir_ )

-------------------------------------------------------------------------------


_test :: IO ()
_test = defaultMain tests

tests :: TestTree
tests = testGroup "Fluffy.EnvReader" [ envRcFileTests ]

envRcFileTests :: TestTree
envRcFileTests = withResource homeDir_ (\ _ -> return ())  envRcFileTests'

envRcFileTests' :: IO AbsDir -> TestTree
envRcFileTests' homedir = do
  let homeFoo hd = hd </> [relfile|foo|] -- [absfile|/my/home/foo|]
      relFoo  = [relfile|foo|]
      envFoo  = [absfile|/bar|]
      invalAbsBar  = e
                     where Left e = parseAbsFile ("bar" :: Text)
      myEnv             = Env [( "HOME", "/my/home/" ), ( "FOO", "/bar" )]
      myEnvNoHome       = Env [( "FOO", "/bar" )]
      myEnvFooNotAbs    = Env [( "HOME", "/my/home/" ), ( "FOO", "bar" )]
      myEnvHomeNotAbs   = Env [( "HOME", "my/home/" ), ( "FOO", "/bar" )]
      myEnvNoHomeSlash  = Env [( "HOME", "/my/home" ), ( "FOO", "/bar" )]
      go :: Env -> EnvKey -> IO (Either IOPathEnvError AbsFile)
      go env envKey = splitMError $ runReaderT (envRcAbsFile envKey relFoo) env
  testGroup "envRcFile"
    [ testCase "envRcFile; fall back to $HOME" $
        homedir >>= \ hd -> go myEnv "NONSUCH"  >>= (@?= Right (homeFoo hd))
    , testCase "envRcFile; success" $
        go myEnv "FOO" >>= (@?= Right envFoo)
    , testCase "envRcFile; no $HOME (but uses FOO)" $
        go myEnvNoHome "FOO" >>= (@?= Right envFoo)
    , testCase "envRcFile; FOO names invalid absolute file" $
        go myEnvFooNotAbs "FOO" >>= (@?= Left (IPE_PATH_ERROR invalAbsBar))
    , testCase "envRcFile; unused FOO names invalid absolute file" $
        homedir >>= \ hd -> go myEnvFooNotAbs "BAR" >>= (@?= Right (homeFoo hd))
    , testCase "envRcFile; unused HOME not absolute dir" $
        go myEnvHomeNotAbs "FOO" >>= (@?= Right envFoo)
    , testCase "envRcFile; fall back to $HOME; no $HOME slash" $
        homedir >>= \ hd -> go myEnvNoHomeSlash "NONSUCH" >>= (@?= Right (homeFoo hd))
    , testCase "envRcFile; success; no $HOME slash" $
        go myEnvNoHomeSlash "FOO" >>= (@?= Right envFoo)

    ]
