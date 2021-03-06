{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Gen.Types.Data
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.Types.Data where

import           Control.Lens     hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Function    (on)
import           Data.Text        (Text)
import qualified Data.Text        as Text
import qualified Data.Text.Lazy   as LText
import           Gen.Types.Help
import           Gen.Types.Id
import           Gen.Types.Map

type Rendered = LText.Text

data Fun = Fun' Text Help Rendered Rendered
    deriving (Eq, Show)

instance ToJSON Fun where
    toJSON (Fun' n c s d) = object
        [ "type"          .= Text.pack "function"
        , "name"          .= n
        , "documentation" .= c
        , "signature"     .= s
        , "declaration"   .= d
        ]

data Prod = Prod'
    { _prodName   :: Text
    , _prodDoc    :: Maybe Help
    , _prodDecl   :: Rendered
    , _prodCtor   :: Fun
    , _prodLenses :: [Fun]
    } deriving (Eq, Show)

prodToJSON :: ToJSON a => Bool -> Bool -> Prod -> Map Text a -> [Pair]
prodToJSON s p Prod'{..} is =
    [ "type"          .= Text.pack "product"
    , "name"          .= _prodName
    , "constructor"   .= _prodCtor
    , "documentation" .= _prodDoc
    , "declaration"   .= _prodDecl
    , "lenses"        .= _prodLenses
    , "instances"     .= is
    , "shared"        .= s
    , "eq"            .= p
    ]

data Sum = Sum'
    { _sumName  :: Text
    , _sumDoc   :: Maybe Help
    , _sumDecl  :: Rendered
    , _sumCtors :: Map Text Text
    } deriving (Eq, Show)

sumToJSON :: Bool -> Sum -> [Text] -> [Pair]
sumToJSON s Sum'{..} is =
    [ "type"          .= Text.pack "sum"
    , "name"          .= _sumName
    , "constructors"  .= _sumCtors
    , "documentation" .= _sumDoc
    , "declaration"   .= _sumDecl
    , "instances"     .= is
    , "shared"        .= s
    , "eq"            .= True
    ]

data Gen = Gen'
    { _genName :: Text
    , _genDoc  :: Maybe Help
    , _genDecl :: Rendered
    } deriving (Eq, Show)

instance ToJSON Gen where
    toJSON Gen'{..} = object
        [ "type"          .= Text.pack "error"
        , "name"          .= _genName
        , "documentation" .= _genDoc
        , "declaration"   .= _genDecl
        ]

-- FIXME: Rather carry around Solved here.
data SData
    = Prod !Bool !Bool Prod (Map Text Rendered) -- ^ A product type (record).
    | Sum  !Bool Sum  [Text]                    -- ^ A nullary sum type.
    | Fun  Fun                                  -- ^ A function declaration.
      deriving (Eq, Show)

instance Ord SData where
    compare a b =
        case (a, b) of
            (Prod _ _ x _, Prod _ _ y _) -> on compare _prodName x y
            (Sum  _ x _,   Sum  _ y _)   -> on compare _sumName  x y
            (Fun  _,       Fun  _)       -> EQ
            (Prod {},      _)            -> GT
            (_,            Prod {})      -> LT
            (Sum  {},      _)            -> GT
            (_,            Sum  {})      -> LT

instance ToJSON SData where
    toJSON = \case
        Prod s p x is -> object (prodToJSON s p x is)
        Sum  s st  is -> object (sumToJSON  s st is)
        Fun  f        -> toJSON f

instance HasId SData where
    identifier = \case
        Prod _ _ p _       -> mkId (_prodName p)
        Sum  _ s _         -> mkId (_sumName  s)
        Fun (Fun' n _ _ _) -> mkId n

data WData = WData
    { _waitName   :: Text
    , _waitOpName :: Id
    , _waitCtor   :: Fun
    } deriving (Show)

makeLenses ''WData

instance ToJSON WData where
    toJSON (WData n _ c) = object
        [ "name"        .= n
        , "constructor" .= c
        ]
