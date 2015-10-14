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
module Person where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (foldMap)
import Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding as TL
import GHC.Generics
import SHSP
import HSP.XMLGenerator
import HSP.XML
import Network.Wai
import Network.HTTP.Types
import Servant
import Servant.Docs
import Servant.Docs.Pandoc
import Text.Pandoc
import Language.Haskell.HSX.QQ (hsx)

-- | Define a record to hold a 'Person'
data Person = Person
  { firstName :: String
  , lastName  :: String
  , age       :: Int
  } deriving Generic -- for the JSON instance

-- | automatically derive a JSON instance for 'Person'
instance ToJSON Person

-- | instance to show a single 'Person' as HTML
instance ToHtml Person where
    toHtml Person{..} =
        [hsx| <tr>
               <td><% firstName %></td>
               <td><% lastName %></td>
               <td><% show age %></td>
              </tr>
        |]

-- | instance to show a list of 'Person' as HTML
instance ToHtml [Person] where
    toHtml people =
        [hsx| <table>
               <tr>
                 <th>First Name</th>
                 <th>Last Name</th>
                 <th>Age</th>
               </tr>
               <% mapM toHtml people %>
              </table>
        |]

-- | define a single route /persons which lists all the people.
-- Depending on the value of the Accept header, this can return JSON
-- or HTML.
type PersonAPI = "persons" :> Get '[JSON, HTML] [Person]


instance ToSample [Person] [Person] where
    toSamples _ = [("a person", [Person "Isaac"  "Newton"   372])]

-- | automatically generate docs for the API
apiDocs :: API
apiDocs = docs personAPI

-- | the people database (a pure value for now)
persons :: [Person]
persons =
  [ Person "Isaac"  "Newton"   372
  , Person "Albert" "Einstein" 136
  ]


personAPI :: Proxy PersonAPI
personAPI = Proxy

type ServerAPI = PersonAPI :<|> Raw

api :: Proxy ServerAPI
api = Proxy

server :: Server ServerAPI
server = return persons
     :<|> serveDocs
         where
           serveDocs _ respond =
               respond $ responseLBS status200 [html] docsHtml
           plain = ("Content-Type", "text/plain")
           html = ("Content-Type", "text/html")
           docsBS :: ByteString
           docsBS = (TL.encodeUtf8 . TL.pack . markdown) apiDocs
           docsHtml :: ByteString
           docsHtml = (TL.encodeUtf8 . TL.pack . writeHtmlString def . pandoc) apiDocs

-- | glue to create an 'Application'
app :: Application
app = serve api server
