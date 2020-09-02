module HedgehogExtra (checkInformative, Report(..), Result(..), wasSuccess) where

import Pre

import Hedgehog (Property)
import Hedgehog.Internal.Runner (checkNamed)
import Hedgehog.Internal.Region (displayRegion)
import Hedgehog.Internal.Report (Report(..), Result(..))

checkInformative :: MonadIO m => Property -> m (Report Result)
checkInformative prop =
  liftIO . displayRegion $ \region ->
    checkNamed region Nothing Nothing prop

wasSuccess :: Report Result -> Bool
wasSuccess Report{reportStatus = OK} = True
wasSuccess _ = False
