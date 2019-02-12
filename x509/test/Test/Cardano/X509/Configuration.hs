{-# LANGUAGE TemplateHaskell #-}
module Test.Cardano.X509.Configuration (tests) where

import           Cardano.X509.Configuration
import qualified Data.List.NonEmpty
import           Hedgehog (Gen, Property)
import qualified Hedgehog as H
import qualified Hedgehog.Gen
import           Hedgehog.Range
import           Test.Pos.Util.Tripping
import           Universum

roundTripTLSConfiguration :: Property
roundTripTLSConfiguration = roundTripsAesonYamlShow 100 genTlsConfig

genTlsConfig :: Gen TLSConfiguration
genTlsConfig = TLSConfiguration <$> genCertConfig <*> genServerConfig <*> genClients

genCertConfig :: Gen CertConfiguration
genCertConfig = CertConfiguration <$> genString <*> genString <*> genInt

genServerConfig :: Gen ServerConfiguration
genServerConfig = ServerConfiguration <$> genCertConfig <*> genNonEmptyString

genClients :: Gen [CertConfiguration]
genClients = Hedgehog.Gen.list (linearFrom 0 0 10) genCertConfig

genString :: Gen String
genString = Hedgehog.Gen.string (linearFrom 0 0 10) Hedgehog.Gen.ascii

genNonEmptyString :: Gen (NonEmpty String)
genNonEmptyString = pure $ Data.List.NonEmpty.fromList [ "foo.com" ] -- TODO

genInt :: Gen Int
genInt = Hedgehog.Gen.integral (linearFrom 10 10 20)

tests :: IO Bool
tests = H.checkParallel $$discoverRoundTrip
