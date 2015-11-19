module Router where

import BigPrelude

import Data.Functor.Coproduct (Coproduct(..), left)
import Control.Monad.Aff (Aff(), forkAff)
import qualified Control.Monad.Aff as AF
import Control.Monad.Eff.Exception
import Control.Monad.Aff.AVar
import DOM
import Control.Monad.Free (liftFI)

import Data.String (toLower)

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Properties.Indexed as P
import Halogen.Component.ChildPath (ChildPath(), cpR, cpL)
import Halogen.Themes.Bootstrap3 as B

import Routing
import Routing.Match
import Routing.Match.Class

import qualified Component.Profile as Profile
import qualified Component.Sessions as Sessions
import qualified Component.Courses as Courses

data Input a 
  = Goto Routes a

data CRUD
  = Index
  | Show Number

data Routes
  = Courses
  | Sessions CRUD
  | Home
  | Profile

init :: State
init = { currentPage: "Home" }

routing :: Match Routes
routing = courses
      <|> sessions
      <|> profile
      <|> home
  where
    profile = Profile <$ route "profile"
    courses = Courses <$ route "courses"
    home = Home <$ lit ""
    sessions = Sessions <$> (route "sessions" *> parseCRUD)
    route str = lit "" *> lit str
    parseCRUD = Show <$> num <|> pure Index

type State = { currentPage :: String }
--type ChildQuery = Coproduct Courses.Input Sessions.Input Profile.Input
type ChildQuery = Coproduct Courses.Input Sessions.Input
type ChildState = Either Courses.State Sessions.State 
type ChildSlot = Either Courses.Slot Sessions.Slot

-- type ChildState = either1of3 Courses.State Sessions.State Profile.State
-- type ChildSlot = either1of3 Courses.Slot Sessions.Slot Sessions.Slot

-- pathToProfile :: ChildPath Profile.State ChildState Profile.Input ChildQuery Profile.Slot ChildSlot
-- pathToProfile = cpL

pathToCourses :: ChildPath Courses.State ChildState Courses.Input ChildQuery Courses.Slot ChildSlot
pathToCourses = cpL

pathToSessions :: ChildPath Sessions.State ChildState Sessions.Input ChildQuery Sessions.Slot ChildSlot
pathToSessions = cpR

type StateP g
  = InstalledState State ChildState Input ChildQuery g ChildSlot

type QueryP
  = Coproduct Input (ChildF ChildSlot ChildQuery)

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


ui :: forall g. (Plus g) 
   => Component (StateP g) QueryP g
ui = parentComponent render eval
  where
    render st =
    --H.div [ P.classes [m_navbarfixed]] $
    H.div_ -- $
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
               (map link ["Sessions", "Courses", "Home", "Profile"])
          ]
        ]
         , H.div
           [ P.classes [B.container]]
           [ viewPage st.currentPage
           ]
      ]
    link s = H.li_ [ H.a [ P.href ("#/" ++ toLower s) ] [ H.text s ] ]

    viewPage :: String -> HTML (SlotConstructor ChildState ChildQuery g ChildSlot) Input
    viewPage "Sessions" =
      H.slot' pathToSessions Sessions.Slot \_ -> { component: Sessions.ui, initialState: unit }
    -- viewPage "Profile" =
    --   H.slot' pathToProfile Profile.Slot \_ -> { component: Profile.ui, initialState: unit }
    viewPage "Courses" =
      H.slot' pathToCourses Courses.Slot \_ -> { component: Courses.ui, initialState: Courses.initialState }
    viewPage _ =
      H.h2_ [ H.text "Hem" ]

    eval :: EvalParent Input State ChildState Input ChildQuery g ChildSlot
    eval (Goto Courses next) = do
      modify (_ { currentPage = "Courses" })
      pure next
    eval (Goto (Sessions view) next) = do
      modify case view of
                  Index -> (_ { currentPage = "Sessions" })
                  Show n -> (_ { currentPage = "Session " ++ show n })
      pure next
    eval (Goto Profile next) = do
      modify (_ { currentPage = "Profile" })
      pure next
    eval (Goto Home next) = do
      modify (_ { currentPage = "Home" })
      pure next

type Effects e = (dom :: DOM, avar :: AVAR, err :: EXCEPTION | e)

routeSignal :: forall eff. Driver QueryP eff
            -> Aff (Effects eff) Unit
routeSignal driver = do
  Tuple old new <- matchesAff routing
  redirects driver old new

redirects :: forall eff. Driver QueryP eff
          -> Maybe Routes
          -> Routes
          -> Aff (Effects eff) Unit
redirects driver _ =
  driver <<< left <<< action <<< Goto
-- redirects driver _ Home = 
--   driver (left (action (Goto Home))))
-- redirects driver _ Profile =
--   driver (left (action (Goto Profile))))
-- redirects driver _ (Sessions view) =
--   driver (left (action (Goto (Sessions view)))))
