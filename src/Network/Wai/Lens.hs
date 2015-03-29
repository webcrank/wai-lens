{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Network.Wai.Lens where

import Control.Lens
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Monoid
import Data.Text (Text)
import Data.Tuple
import Data.Vault.Lazy (Vault)
import Network.Socket
import Network.HTTP.Types
import qualified Network.Wai as W

class HasMethod s a | s -> a where
  method :: Lens' s a

instance HasMethod W.Request Method where
  method = lens W.requestMethod $ \rq m -> rq { W.requestMethod = m }
  {-# INLINE method #-}

class HasHttpVersion s a | s -> a where
  httpVersion :: Lens' s a

instance HasHttpVersion W.Request HttpVersion where
  httpVersion = lens W.httpVersion $ \rq v -> rq { W.httpVersion = v }
  {-# INLINE httpVersion #-}

class HasRawPathInfo s a | s -> a where
  rawPathInfo :: Lens' s a

instance HasRawPathInfo W.Request ByteString where
  rawPathInfo = lens W.rawPathInfo $ \rq p -> rq { W.rawPathInfo = p }
  {-# INLINE rawPathInfo #-}

class HasRawQueryString s a | s -> a where
  rawQueryString :: Lens' s a

instance HasRawQueryString W.Request ByteString where
  rawQueryString = lens W.rawQueryString $ \rq q -> rq { W.rawQueryString = q }
  {-# INLINE rawQueryString #-}

class HasHeaders s a | s -> a where
  headers :: Lens' s a

instance HasHeaders W.Request RequestHeaders where
  headers = lens W.requestHeaders $ \rq h -> rq { W.requestHeaders = h }
  {-# INLINE headers #-}

class HasRemoteHost s a | s -> a where
  remoteHost :: Lens' s a

instance HasRemoteHost W.Request SockAddr where
  remoteHost = lens W.remoteHost $ \rq h -> rq { W.remoteHost = h }
  {-# INLINE remoteHost #-}

class HasPathInfo s a | s -> a where
  pathInfo :: Lens' s a

instance HasPathInfo W.Request [Text] where
  pathInfo = lens W.pathInfo $ \rq p -> rq { W.pathInfo = p }
  {-# INLINE pathInfo #-}

class HasQueryString s a | s -> a where
  queryString :: Lens' s a

instance HasQueryString W.Request Query where
  queryString = lens W.queryString $ \rq q -> rq { W.queryString = q }
  {-# INLINE queryString #-}

class HasRequestBody s a | s -> a where
  requestBody :: Lens' s a

instance HasRequestBody W.Request (IO ByteString) where
  requestBody = lens W.requestBody $ \rq b -> rq { W.requestBody = b }
  {-# INLINE requestBody #-}

class HasVault s a | s -> a where
  vault :: Lens' s a

instance HasVault W.Request Vault where
  vault  = lens W.vault $ \rq v -> rq { W.vault = v }
  {-# INLINE vault #-}

class HasRequestBodyLength s a | s -> a where
  requestBodyLength :: Lens' s a

instance HasRequestBodyLength W.Request W.RequestBodyLength where
  requestBodyLength = lens W.requestBodyLength $ \rq l -> rq { W.requestBodyLength = l }
  {-# INLINE requestBodyLength #-}

class HasStatus s a | s -> a where
  status :: Lens' s a

-- | Useful for looking up query string or header values.
--
-- @
-- req ^. headers . value "Content-Type"
-- @
value
  :: (Eq a, Foldable f)
  => a
  -> (b -> Const (First b) b)
  -> f (a, b)
  -> Const (First b) (f (a, b))
value n = folded . to swap . aside (only n) . _1
{-# INLINE value #-}

