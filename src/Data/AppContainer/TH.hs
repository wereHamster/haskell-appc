module Data.AppContainer.TH
  ( deriveJSON
  , deriveJSONOptions
  , FromJSON(..)
  , ToJSON(..)
  ) where


import Control.Monad

import Data.Char

import Data.Aeson
import Data.Aeson.TH


dropPrefix :: String -> String -> String
dropPrefix prefix x = toLower (head rest) : tail rest
  where rest = drop (length prefix) x


-- FIXME: Should dasherize and drop the prefix from the constructor as well!
deriveJSONOptions :: String -> Options
deriveJSONOptions prefix = defaultOptions
    { fieldLabelModifier     = dropPrefix prefix
    , constructorTagModifier = map toLower
    }
