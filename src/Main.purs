module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
--import Control.Monad.Eff.Console
import Control.Monad.Free (liftFI)

import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Foreign.Class (readProp)
import Data.Functor (($>))
import Data.Maybe (Maybe(..))

import Halogen (HalogenEffects(), Component(), ComponentHTML(), ComponentDSL(), Natural(), runUI, modify, liftAff', component)
import Halogen.Util (appendToBody, onLoad)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B

import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Exception (error, message)
import Network.HTTP.Affjax -- (AJAX(), URL(), affjax, get, post, put)
import Network.HTTP.Affjax.Request
import Network.HTTP.Affjax.Response
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.Method -- (Method(..))
import Network.HTTP.MimeType.Common (applicationJSON)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Printer (printJson)
import Data.Argonaut.Decode (decodeJson, DecodeJson)
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Either (Either(..), either)
import Data.JSON
import Data.Tuple
import qualified Data.Map as M

import WebSocket

-- | CSS
m_navwrapper :: H.ClassName
m_navwrapper = H.className "nav-wrapper"

m_right :: H.ClassName
m_right = H.className "right"
  
m_brandlogo :: H.ClassName
m_brandlogo = H.className "brand-logo"

m_homed :: H.ClassName
m_homed = H.className "hide-on-med-and-down"

m_navbarfixed :: H.ClassName
m_navbarfixed = H.className "navbar-fixed"

m_waveseffect :: H.ClassName
m_waveseffect = H.className "waves-effect"

m_waveslight :: H.ClassName
m_waveslight = H.className "waves-light"

m_btn :: H.ClassName
m_btn = H.className "btn"

-- | The state of the component.
type State = { busy :: Boolean,
               user :: String,
               payload :: String,
               result :: Maybe String }

initialState :: State
initialState = { busy: false,
                 payload: (userRequest "Jack"),
                 user: "Jack",
                 result: Nothing }

userRequest :: String -> String
userRequest name =
  (encode jsonData)
  where
  jsonData = Tuple "useridkey" name

-- | The component query algebra.
data Query a
  = SetPayload String a
  | SetUser String a
  | MakeRequest String a

-- | The effects used in the app.
--type WsEffects eff = HalogenEffects (avar :: AVAR | eff)
type AppEffects eff = HalogenEffects (ajax :: AJAX | eff)
--type WsEffects eff = HalogenEffects ()
--type HalogenEffects eff = (avar :: AVAR, err :: EXCEPTION, dom :: DOM | eff)

-- | The effect type for WS requests made with WS.
--foreign import data AJAX :: !


-- | The definition for the app's main UI component.
ui :: forall eff. Component State Query (Aff (AppEffects eff))
ui = component render eval
  where

  render :: State -> ComponentHTML Query
  render st =
    --H.div [ P.classes [m_navbarfixed]] $
    H.div_ $
      [ H.nav_
        [ H.div
          [ P.classes [m_navwrapper]]
          [ H.a
            [ P.href "#!"
            , P.classes [m_brandlogo]
            ]
            [ H.text "Arduino" ]
          , H.ul
            [ P.classes [m_right, m_homed]]
--          [ H.li_ [ linkTo (Sessions </> New) "Log a session" ]
            [ H.li_
              [ H.a
                [ P.href "#" ]
                [ H.text "One"]
              ]
            , H.li_
              [ H.a
                [ P.href "#" ]
                [ H.text "Two"]
              ]
            ]
          ]
        ]
      , H.div
        [ P.classes [B.container]]
        [ H.h2_
          [ H.text "Halogen Ajax" ]
        , H.input 
            [ P.id_ "msgdata"
            --, P.classes [ B.formControl ]
            --, P.inputType inpType
            , P.value st.user
            , E.onValueInput (E.input SetUser)
            ]
        , H.p_
            [ H.text st.user ]
    {-      , H.p_
            [ H.textarea
                [ P.value st.payload
                , E.onValueInput (E.input SetPayload)]] -}
        , H.p_
            [ H.button
                [ P.disabled st.busy
                , P.classes [m_waveseffect,m_waveslight,m_btn]
                , E.onClick (E.input_ (MakeRequest st.payload))
                ]
                [ H.text "Skicka" ]
            ]
        , H.p_
            [ H.text (if st.busy then "Working..." else "") ]
        ]
      ]
      ++ flip foldMap st.result \js ->
          [ H.div_
              [ H.h3_
                  [ H.text "Svar:" ]
              , H.pre_
                  [ H.code_ [ H.text js ] ]]]

  eval :: Natural Query (ComponentDSL State Query (Aff (AppEffects eff)))
  eval (SetPayload payload next) = modify (_ { payload = payload, result = Nothing :: Maybe String }) $> next
  eval (SetUser user next) = modify (_ { user = user }) $> next
  eval (MakeRequest payload next) = do
    modify (_ { busy = true })
    result <- liftAff' (fetchJS payload)
    --result <- liftAff' (wstest payload)
    modify (_ { busy = false, result = Just result })
    pure next

--runRequest :: forall eff. String -> Aff (ajax :: AJAX | eff) String
--runRequest input = liftEff' (mkWebSocket "ws://localhost:3000/chat")

-- wstest :: forall eff. String -> Aff (WsEffects) String
-- wstest payload = do
--   liftEff' (mkWebSocket "ws://localhost:3000/chat")
--   let response = "ok" :: String
--   return response

--type URL = String 
apiUrl :: URL
apiUrl = "http://94.245.59.238:3000/"

requestURL :: URL -> URL
requestURL url = apiUrl ++ url

-- data Endpoint a b c = Endpoint {
--   method :: Method,
--   serverUrl :: String,
--   mkClientUrl :: a -> URL
-- }

-- parseOrThrow :: forall eff a. (DecodeJson a) => String -> Aff eff a
-- parseOrThrow a = 
--   either throwStr (\json -> either throwStr return $ decodeJson json) (jsonParser a)
--     where throwStr str = throwError $ error str

-- execEndpoint :: forall eff a b c. (EncodeJson b, DecodeJson b, EncodeJson c, DecodeJson c) =>
--                   Endpoint a b c -> a -> b -> Aff (ajax :: AJAX | eff) c

-- execEndpoint (Endpoint {method: method, mkClientUrl: f}) a b = 
--   affjax opts >>= _.response >>> parseOrThrow
--     where opts = { method: method
--                  , url: f a
--                  , headers: [ContentType applicationJSON]
--                  , content: (Just $ printJson $ encodeJson b) :: Maybe String
--                  , username: Nothing
--                  , password: Nothing}

-- data Post = Post { title :: String
--                  , body  :: String
--                  }

-- getPhotobooths :: forall a. Endpoint a Unit (Array Post)
-- getPhotobooths = Endpoint { method: GET, serverUrl: "/posts"
--                           , mkClientUrl: const "/posts"} 

-- headers :: [RequestHeader]
-- headers =
-- 	[ RequestHeader "Authorization" "bla"
-- 	, Accept Network.HTTP.MimeType.Common.applicationJSON
-- 	, ContentType Network.HTTP.MimeType.Common.applicationJSON
-- 	]

-- getWithHeaders :: forall e a b. (Respondable b) => [RequestHeader] -> URL -> Affjax e b
-- getWithHeaders headers url =
-- 	affjax $ defaultRequest
-- 		{ method = GET
-- 		, url = url
-- 		, headers = headers
-- 		}

defRq :: AffjaxRequest Unit
defRq = defaultRequest { headers = [ContentType applicationJSON, Accept applicationJSON] }

    -- return if res.status == StatusCode 201
    --         then Right res.response
    --         else Error.throwJS res.status

body :: String
body = "hej"
--    body = show $ LoginRequestBody { user: username, pass: password }

-- | Post some PureScript code to the trypurescript API and fetch the JS result.
fetchJS :: forall eff. String -> Aff (ajax :: AJAX | eff) String
fetchJS payload = do
  --result <- post (requestURL "posts") payload
  --result <- getPhotobooths
  --result <- get (requestURL "posts")

  result <- affjax defRq
    { method = GET
    , url = requestURL "posts"
    --, content = Just body
    }

  --getChannelsRequest = getWithHeaders headers $ requestURL "posts"
  let response = result.response
  return response
  --return case readProp "js" response <|> readProp "error" response of
  --  Right js -> js
  --  Left _ -> "Invalid response"

-- | Run the app.
main :: Eff (AppEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui initialState
  onLoad $ appendToBody app.node
  
  -- --onMessage ws log
  -- onOpen ws $ do
  --   send ws "Anders"
  -- --onClose ws $ do
  --   --log "disconnected"
