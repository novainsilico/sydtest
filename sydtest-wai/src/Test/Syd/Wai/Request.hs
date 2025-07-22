{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Syd.Wai.Request where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State as State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Time
import GHC.Stack (HasCallStack)
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.Internal (httpRaw)
import Network.HTTP.Types as HTTP
import Test.Syd
import Test.Syd.Wai.Client
import Test.Syd.Wai.Matcher
import Control.Exception (SomeException, try, throwIO)

-- | Perform a @GET@ request to the application under test.
get :: HasCallStack => ByteString -> WaiSession st (HTTP.Response LB.ByteString)
get path = request methodGet path [] ""

-- | Perform a @POST@ request to the application under test.
post :: HasCallStack => ByteString -> LB.ByteString -> WaiSession st (HTTP.Response LB.ByteString)
post path = request methodPost path []

-- | Perform a @PUT@ request to the application under test.
put :: HasCallStack => ByteString -> LB.ByteString -> WaiSession st (HTTP.Response LB.ByteString)
put path = request methodPut path []

-- | Perform a @PATCH@ request to the application under test.
patch :: HasCallStack => ByteString -> LB.ByteString -> WaiSession st (HTTP.Response LB.ByteString)
patch path = request methodPatch path []

-- | Perform an @OPTIONS@ request to the application under test.
options :: HasCallStack => ByteString -> WaiSession st (HTTP.Response LB.ByteString)
options path = request methodOptions path [] ""

-- | Perform a @DELETE@ request to the application under test.
delete :: HasCallStack => ByteString -> WaiSession st (HTTP.Response LB.ByteString)
delete path = request methodDelete path [] ""

-- | Perform a request to the application under test, with specified HTTP
-- method, request path, headers and body.
request :: HasCallStack => Method -> ByteString -> [Header] -> LB.ByteString -> WaiSession st (HTTP.Response LB.ByteString)
request method path headers body = do
  port <- asks waiClientPort
  let req =
        defaultRequest
          { host = "localhost",
            port = fromIntegral port, -- Safe because it is PortNumber -> INt
            method = method,
            path = path,
            requestHeaders = headers,
            requestBody = RequestBodyLBS body
          }
  now <- liftIO getCurrentTime
  cj <- State.gets waiClientStateCookies
  let (req', cj') = insertCookiesIntoRequest req cj now
  State.modify' (\s -> s {waiClientStateCookies = cj'})

  performRequest req'

-- | Perform a bare 'HTTP.Request'.
--
-- You can use this to make a request to an application other than the one
-- under test.  This function does __not__ set the host and port of the request
-- like 'request' does, but it does share a 'CookieJar'.
performRequest :: HasCallStack => HTTP.Request -> WaiSession st (HTTP.Response LB.ByteString)
performRequest req = do
  man <- asks waiClientManager
  respM <- liftIO $ try $ httpRaw req man >>= traverse (fmap LB.fromChunks . brConsume)
  -- This is a naive way to restore a callstack from an exception which does not have one
  resp <- case respM :: Either SomeException (Response LB.ByteString) of
    Right resp -> pure resp
    Left e -> liftIO $ throwIO e
  cj <- State.gets waiClientStateCookies
  now <- liftIO getCurrentTime
  let (cj', _) = updateCookieJar resp req now cj
  State.modify'
    ( \s ->
        s
          { waiClientStateLast = Just (req, resp),
            waiClientStateCookies = cj'
          }
    )
  pure resp

-- | Make a test assertion using a 'ResponseMatcher' on the 'HTTP.Response' produced by the given action
--
-- This function is provided for backward compatibility with wai-test but this approach has been made obsolete by the way sydtest does things.
-- You should use 'shouldBe' based on the responses that you get from functions like 'get' and 'post' instead.
shouldRespondWith :: (HasCallStack) => WaiSession st (HTTP.Response LB.ByteString) -> ResponseMatcher -> WaiExpectation st
shouldRespondWith action ResponseMatcher {..} = do
  response <- action
  liftIO $
    context (ppShow response) $ do
      HTTP.statusCode (responseStatus response) `shouldBe` matchStatus
      forM_ matchHeaders $ \(MatchHeader matchHeaderFunc) ->
        mapM_ expectationFailure $ matchHeaderFunc (responseHeaders response) (responseBody response)
      let (MatchBody matchBodyFunc) = matchBody
      mapM_ expectationFailure $ matchBodyFunc (responseHeaders response) (responseBody response)
