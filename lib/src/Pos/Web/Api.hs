{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Web API exposed by node.

module Pos.Web.Api
       ( NodeApi
       , nodeApi

       , HealthCheckApi
       , healthCheckApi
       ) where

import           Universum

import           Pos.Util.Servant (Tag, TagDescription (..))
import           Servant.API ((:<|>), (:>), Get, JSON, PlainText, Summary)

import           Pos.Chain.Txp (Utxo)
import           Pos.Web.Types (CConfirmedProposalState)

----------------------------------------------------------------------------
-- Base
----------------------------------------------------------------------------

-- | Servant API which provides access to full node internals.
--
-- Implementations of these methods are in
-- 'Pos.Web.Server.nodeServantHandlers'.
type NodeApi =
    Tag "Legacy API" ('TagDescription
    "Why is this here? How long will it remain?\
    \ ") :>
    ( "utxo"
        :> Summary "Utxo"
        :> Get '[JSON] Utxo
    :<|>

    "confirmed_proposals"
        :> Summary "Confirmed proposals"
        :> Get '[JSON] [CConfirmedProposalState]
    )

-- | Helper Proxy.
nodeApi :: Proxy NodeApi
nodeApi = Proxy

----------------------------------------------------------------------------
-- HealthCheck
----------------------------------------------------------------------------

-- | Helper Proxy.
healthCheckApi :: Proxy HealthCheckApi
healthCheckApi = Proxy

type HealthCheckApi =
    "healthcheck" :> "route53" :> Get '[PlainText] String
