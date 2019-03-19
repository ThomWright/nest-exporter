{-# LANGUAGE DeriveGeneric #-}

-- {-# LANGUAGE NamedFieldPuns #-}
module Web.Nest.HttpClient.Thermostat
  ( path
  , getThermostatData
  , ThermostatResponseBody
  ) where

import           Data.Aeson                      (FromJSON, parseJSON,
                                                  withObject, (.:))
import qualified Data.ByteString.Char8           as S8
import           Data.Text                       (Text, unpack)
import           GHC.Generics
import           Network.HTTP.Simple
import           Web.Nest.HttpClient.AccessToken (AccessToken (..))
import           Web.Nest.HttpClient.Base        (defaultSecureRequest)
import           Web.Nest.HttpClient.Request     (NestRequest)

path :: Text -> S8.ByteString
path thermostatId = S8.pack ("/devices/thermostats/" <> unpack thermostatId)

getThermostatData :: AccessToken -> Text -> NestRequest ThermostatResponseBody
getThermostatData (AccessToken token) thermostatId =
  setRequestPath (path thermostatId) $
  setRequestMethod "GET" $
  setRequestHeader
    "Authorization"
    [S8.pack $ "Bearer " <> unpack token]
    defaultSecureRequest

data ThermostatResponseBody = ThermostatResponseBody
  { device_id                   :: String
  , locale                      :: String
  , software_version            :: String
  , structure_id                :: String
  , name                        :: String
  , name_long                   :: String
  , last_connection             :: String
  , is_online                   :: Bool
  , can_cool                    :: Bool
  , can_heat                    :: Bool
  , is_using_emergency_heat     :: Bool
  , has_fan                     :: Bool
  , fan_timer_active            :: Bool
  , fan_timer_timeout           :: String
  , has_leaf                    :: Bool
  , temperature_scale           :: String
  , target_temperature_f        :: Float
  , target_temperature_c        :: Float
  , target_temperature_high_f   :: Float
  , target_temperature_high_c   :: Float
  , target_temperature_low_f    :: Float
  , target_temperature_low_c    :: Float
  , eco_temperature_high_f      :: Float
  , eco_temperature_high_c      :: Float
  , eco_temperature_low_f       :: Float
  , eco_temperature_low_c       :: Float
  , hvac_mode                   :: String
  , previous_hvac_mode          :: String
  , ambient_temperature_f       :: Float
  , ambient_temperature_c       :: Float
  , humidity                    :: Float
  , hvac_state                  :: String
  , where_id                    :: String
  , is_locked                   :: Bool
  , locked_temp_min_f           :: Float
  , locked_temp_max_f           :: Float
  , locked_temp_min_c           :: Float
  , locked_temp_max_c           :: Float
  , label                       :: String
  , where_name                  :: String
  , sunlight_correction_enabled :: Bool
  , sunlight_correction_active  :: Bool
  , fan_timer_duration          :: Int
  , time_to_target              :: String
  , time_to_target_training     :: String
  } deriving (Eq, Generic, Show)

instance FromJSON ThermostatResponseBody where
  parseJSON =
    withObject "ThermostatResponseBody" $ \v ->
      ThermostatResponseBody <$> v .: "device_id" <*> v .: "locale" <*>
      v .: "software_version" <*>
      v .: "structure_id" <*>
      v .: "name" <*>
      v .: "name_long" <*>
      v .: "last_connection" <*>
      v .: "is_online" <*>
      v .: "can_cool" <*>
      v .: "can_heat" <*>
      v .: "is_using_emergency_heat" <*>
      v .: "has_fan" <*>
      v .: "fan_timer_active" <*>
      v .: "fan_timer_timeout" <*>
      v .: "has_leaf" <*>
      v .: "temperature_scale" <*>
      v .: "target_temperature_f" <*>
      v .: "target_temperature_c" <*>
      v .: "target_temperature_high_f" <*>
      v .: "target_temperature_high_c" <*>
      v .: "target_temperature_low_f" <*>
      v .: "target_temperature_low_c" <*>
      v .: "eco_temperature_high_f" <*>
      v .: "eco_temperature_high_c" <*>
      v .: "eco_temperature_low_f" <*>
      v .: "eco_temperature_low_c" <*>
      v .: "hvac_mode" <*>
      v .: "previous_hvac_mode" <*>
      v .: "ambient_temperature_f" <*>
      v .: "ambient_temperature_c" <*>
      v .: "humidity" <*>
      v .: "hvac_state" <*>
      v .: "where_id" <*>
      v .: "is_locked" <*>
      v .: "locked_temp_min_f" <*>
      v .: "locked_temp_max_f" <*>
      v .: "locked_temp_min_c" <*>
      v .: "locked_temp_max_c" <*>
      v .: "label" <*>
      v .: "where_name" <*>
      v .: "sunlight_correction_enabled" <*>
      v .: "sunlight_correction_active" <*>
      v .: "fan_timer_duration" <*>
      v .: "time_to_target" <*>
      v .: "time_to_target_training"
