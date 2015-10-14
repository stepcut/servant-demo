{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module SHSP where

import Data.Typeable (Typeable)
import HSP.Monad
import HSP.XML
import HSP.HTML4
import HSP.XMLGenerator
import Data.Text.Lazy.Encoding as TL
import Control.Monad.Identity
import qualified Network.HTTP.Media as M
import Servant

data HTML deriving Typeable

class ToHtml a where
    toHtml :: a -> XMLGenT (HSPT XML Identity) XML

instance Accept HTML where
    contentType _ = "text" M.// "html" M./: ("charset", "utf-8")

instance (ToHtml c) => MimeRender HTML c where
     mimeRender _ = TL.encodeUtf8 . renderAsHTML . runIdentity . unHSPT . unXMLGenT . toHtml

instance MimeRender HTML (XMLGenT (HSPT XML Identity) XML) where
    mimeRender _ = TL.encodeUtf8 . renderAsHTML . runIdentity . unHSPT . unXMLGenT

