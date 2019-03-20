{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Data.Aeson                           as J hiding (decode)
import qualified Data.ByteString.Char8                as BL8
import qualified Data.ByteString.Lazy                 as BL
import           Data.Csv
import           Data.List.Utils
import           Data.Maybe
import           Data.Scientific
import qualified Data.Vector                          as V
import           GHC.Generics
import           Prelude                              hiding (lines)
import           Text.Parsec                          as P
import           Text.ParserCombinators.Parsec.Number


data Point = Point {
    latitude   :: Float
  , longtitude :: Float
  } deriving (Show, Generic)

newtype Points = Points [Point]
  deriving (Show)

instance FromField Points where
  parseField v = return
                   $ either (error . show) id
                   $ P.runParser pointsParser () "points" (BL8.unpack v)
    where
      pointsParser :: P.Parsec String () Points
      pointsParser = Points <$> between (string "POLYGON((") (string "))")
                                        (point `sepBy` (char ','))
      point :: Parsec String () Point
      point = Point <$> floating2 False <*> (spaces *> floating2 False)


data InputDistrict = InputDistrict {
    inDistName     :: String
  , inDistPolygons :: Points
  } deriving (Show)

instance FromRecord InputDistrict where
  parseRecord v = InputDistrict <$> v .! 0 <*> v .! 2

data Entry = Entry {
    e_number                               :: String
  , e_geometry                             :: String
  , e_timestamp                            :: String
  , e_neighbourhood                        :: String
  , e_district                             :: String
  , e_polygon_buffer_area_w_radius_50m     :: String
  , e_img_url                              :: String
  , e_tree                                 :: Float
  , e_tree_species                         :: String
  , e_trash_container                      :: String
  , e_trash_container_type                 :: String
  , e_park                                 :: Float
  , e_park_surface_m2                      :: String
  , e_park_name                            :: String
  , e_traffic_light                        :: String
  , e_traffic_light_type                   :: String
  , e_tram_stop                            :: String
  , e_metro_stop                           :: String
  , e_public_toilet                        :: String
  , e_metro_line                           :: String
  , e_tram_line                            :: String
  , e_building_monument                    :: String
  , e_building_use_residential             :: String
  , e_building_rental                      :: String
  , e_building_use_activities_meeting      :: Float
  , e_building_use_care                    :: Float
  , e_building_use_education               :: Float
  , e_building_use_enterprises             :: Float
  , e_building_use_going_out_tourism       :: Float
  , e_building_use_hotels_bars_restaurants :: Float
  , e_building_use_offices                 :: Float
  , e_building_use_parking                 :: Float
  , e_building_use_public_transport        :: Float
  , e_building_use_religion                :: Float
  , e_building_use_retail                  :: Float
  , e_building_use_sports                  :: Float
  , e_building                             :: Float
  , e_building_avg_value                   :: Maybe Float
  , e_building_median_age                  :: Maybe Float
  } deriving (Show, Generic)


data Properties = Properties {
    propName                             :: String
  , tree                                 :: Float
  , tree_species                         :: [String]
  , park                                 :: Float
  , building_use_activities_meeting      :: Float
  , building_use_care                    :: Float
  , building_use_education               :: Float
  , building_use_enterprises             :: Float
  , building_use_going_out_tourism       :: Float
  , building_use_hotels_bars_restaurants :: Float
  , building_use_offices                 :: Float
  , building_use_parking                 :: Float
  , building_use_public_transport        :: Float
  , building_use_religion                :: Float
  , building_use_retail                  :: Float
  , building_use_sports                  :: Float
  , building                             :: Float
  , building_avg_value_min               :: Float
  , building_avg_value_max               :: Float
  , building_median_age_min              :: Float
  , building_median_age_max              :: Float
  } deriving (Show, Generic)

data District = District {
    districtGeometry   :: Points
  , districtType       :: String
  , districtProperties :: Properties
  } deriving (Show, Generic)

data DistrictData = DistrictData {
    districtDataType :: String
  , features         :: [District]
  } deriving (Show, Generic)

data Geojson = Geojson {
    jsonType :: String
  , datas    :: DistrictData
  } deriving (Show, Generic)

instance ToJSON Points where
  toJSON (Points pts) = ar (map (\Point{..} -> ar [num latitude, num longtitude]) pts)
    where ar = J.Array . V.fromList
          num = J.Number . fromFloatDigits

instance ToJSON Properties
instance ToJSON District
instance ToJSON DistrictData
instance ToJSON Geojson

instance FromRecord Entry

loadDistricts :: FilePath -> IO [InputDistrict]
loadDistricts path = do
  csvData <- BL.readFile path
  let Right decoded = decode HasHeader csvData
  return $ V.toList decoded

loadEntries :: FilePath -> IO [Entry]
loadEntries path = do
  csvData <- BL.readFile path
  let decoded = either (error.show) id $ decode HasHeader csvData
  return $ V.toList decoded

toGeojson :: [InputDistrict] -> [Entry] -> Geojson
toGeojson districts entries = Geojson "geojson" $ DistrictData "FeatureCollection" dists
  where
    dists = for districts (\InputDistrict{..} ->
                             District inDistPolygons "Feature"
                               $ makeProperties
                                   (filter ((==inDistName) . e_district) entries)
                                   inDistName)
    for = flip map

makeProperties :: [Entry] -> String -> Properties
makeProperties dist name = Properties {
    propName                             = name
  , tree                                 = sum e_tree
  , tree_species                         = uniq $ concat species
  , park                                 = sum e_park
  , building_use_activities_meeting      = sum e_building_use_activities_meeting
  , building_use_care                    = sum e_building_use_care
  , building_use_education               = sum e_building_use_education
  , building_use_enterprises             = sum e_building_use_enterprises
  , building_use_going_out_tourism       = sum e_building_use_going_out_tourism
  , building_use_hotels_bars_restaurants = sum e_building_use_hotels_bars_restaurants
  , building_use_offices                 = sum e_building_use_offices
  , building_use_parking                 = sum e_building_use_parking
  , building_use_public_transport        = sum e_building_use_public_transport
  , building_use_religion                = sum e_building_use_religion
  , building_use_retail                  = sum e_building_use_retail
  , building_use_sports                  = sum e_building_use_sports
  , building                             = sum e_building
  , building_avg_value_min               = min e_building_avg_value
  , building_avg_value_max               = max e_building_avg_value
  , building_median_age_min              = min e_building_median_age
  , building_median_age_max              = max e_building_median_age
  }
  where
    op :: (a -> b -> b) -> b -> (Entry -> a) -> b
    op o def f  = foldr o def (map f dist)
    sum :: Num a => (Entry -> a) -> a
    sum = op (+) 0
    min :: (Entry -> Maybe Float) -> Float
    min = op (\a b -> maybe b (Prelude.min b) a) 0
    max :: (Entry -> Maybe Float) -> Float
    max = op (\a b -> maybe b (Prelude.max b) a) 0
    species = map (\p -> either (error.show) id
                         $ P.runParser speciesParser () "species"
                         $ e_tree_species p) dist

speciesParser :: Parsec String () [String]
speciesParser = between (char '[') (char ']') (nan <|> species)
  where
    nan = string "nan" *> (return [])
    species = specie `sepBy` (char ' ')
    specie = between (char '\'') (char '\'') (many (letter <|> (char '-') <|> (char ' ')))

main :: IO ()
main = do
  dists <- loadDistricts "districts.csv"
  entries <- loadEntries "test.csv"
  BL.writeFile "out.json" (J.encode $ toGeojson dists entries)
