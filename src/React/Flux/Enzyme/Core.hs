{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module React.Flux.Enzyme.Core
( Prop(..)
, EventType(..)
, EnzymeSelector(..)
) where

import Data.Aeson (encode, object, (.=), Value)
import Data.Aeson.Types (ToJSON, toJSON)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char (toLower)
import qualified Data.JSString as JSS

import Data.String.Conversions
import GHCJS.Marshal.Pure
import Language.Css.Pretty
-- TODO pack

import Language.Css.Syntax hiding (Prop)


-- | StringSelector can be a CSS class, tag, id, prop (e.g. "[foo=3]"),
--   component display name. (TODO should this be refined? Enhance? Be checked?)
--   PropertySelector specifies (some of) the element's props
--   ComponentSelector specifies the component constructor (how???)
data EnzymeSelector =
    StringSelector String
  | PropertySelector [Prop]
  | StyleSelector [Decl]
--  | ComponentSelector

-- | TODO: 'PropertySelector' should be translated to js array, then we could get rid of
-- 'js_exec_with_object', and 'execWithSelector' as a special case of 'execWith1Arg'.
instance PToJSVal EnzymeSelector where
  pToJSVal (StringSelector str) = pToJSVal str
  pToJSVal (PropertySelector p) = pToJSVal . JSS.pack . LBS.unpack $ encode p
  pToJSVal (StyleSelector dcls) = pToJSVal . JSS.pack . LBS.unpack $ encode (object ["style" .= declsToJSON dcls])

data EventType =
    MouseEnter
  | MouseLeave
  | Click
  deriving (Show)

-- | Turns the event into a string where the first character is lowercased. (Note: This is required for the ReactWrapper)
instance PToJSVal EventType where
  pToJSVal event = let (ch:chs) = show event in pToJSVal (toLower ch : chs)

data Prop where
  Prop :: forall a. (ToJSON a) => ST -> a -> Prop

-- FIXME: Introduce newtype Props = Props [Prop]
instance {-# OVERLAPPING #-} ToJSON [Prop] where
  toJSON = object . fmap (\(Prop k v) -> k .= v)

declsToJSON :: [Decl] -> Data.Aeson.Value
declsToJSON = object . fmap (\(Decl _mprio n a) -> (cs (prettyPrint n) .= prettyPrint a))
