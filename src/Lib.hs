{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib  where

import           Data.Aeson
import           GHC.Generics
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data BigType = VOne String Int String String Int Int String String Int Int
               deriving (Show, Generic, Eq)

instance ToJSON BigType where
  toJSON (VOne a b c d e f g h i j) =
    object [ "a" .= a
           , "b" .= b
           , "c" .= c
           , "d" .= d
           , "e" .= e
           , "f" .= f
           , "g" .= g
           , "h" .= g
           , "i" .= i
           , "j" .= j
                    ]
instance FromJSON BigType

genString :: Gen String
genString = Gen.string (Range.constant 0 1000) Gen.alphaNum

genInt :: Gen Int
genInt = Gen.int (Range.constant 0 1000)

genBigType :: Gen BigType
genBigType =
  VOne
     <$> genString
     <*> genInt
     <*> genString
     <*> genString
     <*> genInt
     <*> genInt
     <*> genString
     <*> genString
     <*> genInt
     <*> genInt

trippingWrapper :: (MonadTest m, ToJSON a, FromJSON a, Show a, Eq a) => a -> m ()
trippingWrapper a = tripping a encode decode

eachOf :: (Show a) => TestLimit -> Gen a -> (a -> PropertyT IO ()) -> Property
eachOf testLimit things hasProperty =
  withTests testLimit . property $ forAll things >>= hasProperty

prop_roundTrip_no_wrapper :: Property
prop_roundTrip_no_wrapper =
  property $ do
    xs <- forAll genBigType
    tripping xs encode decode

prop_roundTrip1_wrapper :: Property
prop_roundTrip1_wrapper =
  property $ do
    xs <- forAll genBigType
    trippingWrapper xs

prop_roundTrip2_wrapper_withTests :: Property
prop_roundTrip2_wrapper_withTests = withTests 100 .
  property $ do
    xs <- forAll genBigType
    trippingWrapper xs


-- | This should fail similarly to `roundTripStaticConfig_with_eachOf` but it does not!
-- It fails with the diff we want vs `roundTripStaticConfig_with_eachOf` which only
-- returns the value we failed on
prop_roundTrip3_with_eachOf :: Property
prop_roundTrip3_with_eachOf = eachOf 100 genBigType trippingWrapper

tests :: IO Bool
tests =
  checkParallel $$(discover)