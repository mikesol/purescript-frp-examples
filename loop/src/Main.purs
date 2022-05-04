module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, deferCofree)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Control.Parallel (parTraverse)
import Control.Plus (empty)
import Data.Array (cons, null)
import Data.Filterable (filter)
import Data.Foldable (oneOf, oneOfMap)
import Data.Homogeneous.Record (fromHomogeneous, homogeneous)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Newtype (unwrap)
import Data.Number (ceil)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Deku.Attribute ((:=))
import Deku.Control (blank, plant, text, text_)
import Deku.DOM as D
import Deku.Listeners (click, click_, slider)
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Behavior (sampleBy, step, unfold)
import FRP.Event (Event, bang, mapAccum, memoize)
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Event.Time (delay)
import FRP.Event.VBus (V, vbus)
import Type.Proxy (Proxy(..))
import WAGS.Clock (withACTime)
import WAGS.Control (gain_, playBuf)
import WAGS.Core (Channel(..), Node)
import WAGS.Interpret (close, context, decodeAudioDataFromUri)
import WAGS.Math (calcSlope)
import WAGS.Properties as P
import WAGS.Run (run2e)
import WAGS.WebAPI (AudioContext, BrowserAudioBuffer)

-- | This is our event bus.
-- | It holds all of the possible events we'll receive in our application.
-- | `onOff` will turn the audio on and off, and everything else is for a slider.
type Bus = V
  ( notes ::
      V
        ( n1 :: Unit
        , n2 :: Unit
        , n3 :: Unit
        , n4 :: Unit
        , n5 :: Unit
        , n6 :: Unit
        , n7 :: Unit
        , n8 :: Unit
        , n9 :: Unit
        , n10 :: Unit
        , n11 :: Unit
        )
  , onOff :: Maybe (Effect Unit)
  , slider :: Number
  )

dc1 :: forall i. i -> Cofree Identity i -> Cofree Identity i
dc1 a b = deferCofree \_ -> a /\ (Identity b)

dc2 :: forall i. i -> (Unit -> Cofree Identity i) -> Cofree Identity i
dc2 a b = deferCofree (map (\b' -> a /\ (Identity b')) b)

infixr 6 dc1 as <:
infixr 6 dc2 as <<:

score = (_.d1 /\ _.n1)
  <: (_.f1 /\ _.n2)
  <: (_.e1 /\ _.n3)
  <: (_.d1 /\ _.n4)
  <: (_.e1 /\ _.n5)
  <: (_.d1 /\ _.n6)
  <: (_.f1 /\ _.n7)
  <: (_.a1 /\ _.n8)
  <: (_.d1 /\ _.n9)
  <: (_.f1 /\ _.n10)
  <: (_.e1 /\ _.n11)
  <<: \_ -> score

main :: Effect Unit
main = launchAff_ do
  ctx' <- context
  sounds <- map fromHomogeneous $ parTraverse (decodeAudioDataFromUri ctx') $ homogeneous
    { d1: "https://freesound.org/data/previews/560/560682_12581356-lq.mp3"
    , e1: "https://freesound.org/data/previews/560/560688_12581356-lq.mp3"
    , f1: "https://freesound.org/data/previews/560/560690_12581356-lq.mp3"
    , g1: "https://freesound.org/data/previews/560/560678_12581356-lq.mp3"
    , a1: "https://freesound.org/data/previews/560/560668_12581356-lq.mp3"
    , b1: "https://freesound.org/data/previews/560/560674_12581356-lq.mp3"
    , c1: "https://freesound.org/data/previews/560/560675_12581356-lq.mp3"
    , d2: "https://freesound.org/data/previews/560/560689_12581356-lq.mp3"
    , e2: "https://freesound.org/data/previews/560/560687_12581356-lq.mp3"
    }
  close ctx'
  liftEffect $ runInBody1
    ( vbus (Proxy :: Proxy Bus) \push event -> do
        let
          onOff = event.onOff <|> bang Nothing
          checkedB = map fromHomogeneous
            $ traverse (flip (unfold (const not)) true)
            $ homogeneous event.notes
          tempoB = calcSlope 0.0 0.05 100.0 0.5 <$> step 50.0 event.slider
          makeCheckbox (t /\ p) = D.div_
            [ text_ t
            , D.input
                ( oneOf
                    [ bang $ D.Xtype := "checkbox"
                    , filter isNothing event.onOff $> D.Checked := "false"
                    , click_ (bang p)
                    ]
                )
                blank
            ]

          animate :: AudioContext -> Event (Array { checked :: _, time :: Number, buffer :: (_ -> BrowserAudioBuffer), onOff :: (_ -> Boolean) })
          animate ctx = mapAccum
            ( \{ behaviors: { tempo, checked }, acTime } { loop, writeAdj, prevACTime, prevAdjTime } -> do
                let prevAC = fromMaybe 0.0 prevACTime
                let prevAJ = fromMaybe 0.0 prevAdjTime
                let gap = acTime - prevAC
                let adjGap = gap / tempo
                let adjTime = adjGap + prevAJ
                let lookAhead = 0.3
                let
                  f wa cf =
                    if wa < adjTime + lookAhead then
                      ( let
                          q /\ r /\ s = f (wa + 1.0) (unwrap (unwrapCofree cf))
                        in
                          q
                            /\
                              ( cons
                                  ( let
                                      a /\ b = extract cf
                                    in
                                      { checked
                                      , buffer: a
                                      , onOff: b
                                      , time: calcSlope prevAJ prevAC adjTime acTime wa
                                      }
                                  )
                                  r
                              )
                            /\ s
                      )
                    else wa /\ [] /\ cf
                let w /\ a /\ l = f writeAdj loop
                { writeAdj: w, prevACTime: Just acTime, prevAdjTime: Just adjTime, loop: l } /\ a
            )
            ( sampleBy { behaviors: _, acTime: _ }
                ({ tempo: _, checked: _ } <$> tempoB <*> checkedB)
                (withACTime ctx animationFrame <#> _.acTime)
            )
            { loop: score, writeAdj: 0.0, prevACTime: Nothing, prevAdjTime: Nothing }

          graph :: forall lock. _ -> Array (Node _ lock _)
          graph e =
            [ gain_ 1.0
                ( oneOfMap
                    ( \x ->
                        if x.onOff x.checked then
                          ( ( bang $ Sound
                                (playBuf (x.buffer sounds) (bang (P.onOff x.time)))
                            ) <|> (delay 2000 (bang Silence))
                          )
                        else empty
                    ) <$> e
                )
            ]
        plant $ D.div (bang $ D.Class := "w-screen h-screen flex flex-col")
          [ D.div (bang $ D.Class := "flex-grow") blank
          , D.div (bang $ D.Class := "flex flex-row")
              [ D.div (bang $ D.Class := "flex-grow") blank
              , D.div (bang $ D.Class := "")
                  ( [ D.div_ (text_ "Check the check box to turn off a note") ]
                      <>
                        ( makeCheckbox <$>
                            [ "Note 1" /\ push.notes.n1
                            , "Note 2" /\ push.notes.n2
                            , "Note 3" /\ push.notes.n3
                            , "Note 4" /\ push.notes.n4
                            , "Note 5" /\ push.notes.n5
                            , "Note 6" /\ push.notes.n6
                            , "Note 7" /\ push.notes.n7
                            , "Note 8" /\ push.notes.n8
                            , "Note 9" /\ push.notes.n9
                            , "Note 10" /\ push.notes.n10
                            , "Note 11" /\ push.notes.n11
                            ]
                        )
                      <>
                        [ D.div_ [ text_ "Faster <> Slower", D.input (slider (bang push.slider)) blank ]
                        , D.div_
                            [ D.button
                                ( (bang $ D.Class := "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded") <|> click
                                    ( onOff <#> case _ of
                                        Nothing -> do
                                          ctx <- context
                                          run2e ctx
                                            (memoize (animate ctx) graph) <#> (_ *> close ctx) <#> Just >>= push.onOff
                                        Just off -> off *> push.onOff Nothing
                                    )
                                )
                                (text $ (isJust >>> if _ then "Turn off" else "Turn on") <$> onOff)
                            ]
                        ]
                  )
              , D.div (bang $ D.Class := "flex-grow") blank
              ]
          , D.div (bang $ D.Class := "flex-grow") blank
          ]
    )
