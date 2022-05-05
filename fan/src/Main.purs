module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, deferCofree)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Array (cons)
import Data.Filterable (filter, filterDefault, filterMap)
import Data.Foldable (oneOf, oneOfMap)
import Data.Homogeneous.Record (fromHomogeneous, homogeneous)
import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Newtype (unwrap)
import Data.Number (pow)
import Data.Traversable (traverse)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Deku.Attribute ((:=))
import Deku.Control (blank, plant, text, text_)
import Deku.DOM as D
import Deku.Listeners (click, click_, slider)
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import FRP.Behavior (sampleBy, step, unfold)
import FRP.Event (Event, bang, keepLatest, mapAccum, memoize)
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Event.Time (delay)
import FRP.Event.VBus (V, vbus)
import Type.Proxy (Proxy(..))
import WAGS.Clock (withACTime)
import WAGS.Control (bandpass_, gain, gain_, globalFan1, bandpass_, lowpass_, sawtoothOsc, squareOsc)
import WAGS.Core (Audible, AudioEnvelope(..), Channel(..), Node, mix)
import WAGS.Interpret (close, context)
import WAGS.Math (calcSlope)
import WAGS.Properties as P
import WAGS.Run (run2e)
import WAGS.WebAPI (AudioContext)

switcher
  :: forall i o lock payload
   . (i -> Node o lock payload)
  -> Event i
  -> Audible o lock payload
switcher f event = mix $ keepLatest
  $ memoize (counter event) \cenv -> map
      ( \(p /\ n) -> bang (Sound $ f p) <|>
          ((const Silence) <$> filter (eq (n + 1) <<< snd) cenv)
      )
      cenv
  where
  counter :: forall a. Event a → Event (a /\ Int)
  counter ev = mapAccum fn ev 0
    where
    fn a b = (b + 1) /\ (a /\ b)

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
  , filter :: V (none :: Unit, bandpass :: Unit, lowpass :: Unit)
  , onOff :: Maybe (Effect Unit)
  , slider :: Number
  )

dc1 :: forall i. i -> Cofree Identity i -> Cofree Identity i
dc1 a b = deferCofree \_ -> a /\ (Identity b)

dc2 :: forall i. i -> (Unit -> Cofree Identity i) -> Cofree Identity i
dc2 a b = deferCofree (map (\b' -> a /\ (Identity b')) b)

infixr 6 dc1 as <:
infixr 6 dc2 as <<:

score = (60 /\ _.n1)
  <: (64 /\ _.n2)
  <: (62 /\ _.n3)
  <: (66 /\ _.n4)
  <: (64 /\ _.n5)
  <: (68 /\ _.n6)
  <: (66 /\ _.n7)
  <: (70 /\ _.n8)
  <: (68 /\ _.n9)
  <: (72 /\ _.n10)
  <: (70 /\ _.n11)
  <<: \_ -> score

midi2cps :: Int -> Number
midi2cps i = 440.0 * (2.0 `pow` ((69.0 - (toNumber i)) / 12.0))

data Filter = None | Bandpass | Lowpass

derive instance Eq Filter

main :: Effect Unit
main = runInBody1
  ( vbus (Proxy :: Proxy Bus) \push event -> do
      let
        onOff = event.onOff <|> bang Nothing
        checkedB = map fromHomogeneous
          $ traverse (flip (unfold (const not)) true)
          $ homogeneous event.notes
        filterB = step None ((event.filter.none $> None) <|> (event.filter.bandpass $> Bandpass) <|> (event.filter.lowpass $> Lowpass))
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

        animate
          :: AudioContext
          -> Event { prevFilter :: Filter, filter :: Filter, checked :: _, arr :: Array { time :: Number, pitch :: Int, sawSq :: (_ -> Boolean) } }
        animate ctx = mapAccum
          ( \{ behaviors: { tempo, checked, filter }, acTime } { prevFilter, loop, writeAdj, prevACTime, prevAdjTime } -> do
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
                                    { pitch: a
                                    , sawSq: b
                                    , time: calcSlope prevAJ prevAC adjTime acTime wa
                                    }
                                )
                                r
                            )
                          /\ s
                    )
                  else wa /\ [] /\ cf
              let w /\ a /\ l = f writeAdj loop
              { prevFilter: filter, writeAdj: w, prevACTime: Just acTime, prevAdjTime: Just adjTime, loop: l } /\ { prevFilter, filter, checked, arr: a }
          )
          ( sampleBy { behaviors: _, acTime: _ }
              ({ tempo: _, checked: _, filter: _ } <$> tempoB <*> checkedB <*> filterB)
              (withACTime ctx animationFrame <#> _.acTime)
          )
          { loop: score, prevFilter: None, writeAdj: 0.0, prevACTime: Nothing, prevAdjTime: Nothing }

        graph :: forall lock. _ -> Array (Node _ lock _)
        graph e =
          [ globalFan1
              ( gain_ 1.0
                  ( ( \{ checked, arr } -> oneOfMap
                        ( \x ->
                            ( bang $ Sound
                                ( gain 0.0 (bang $ P.gain (AudioEnvelope { d: 0.5, o: x.time + 0.02, p: [ 0.0, 0.1, 0.5, 0.2, 0.05, 0.01, 0.0 ] }))
                                    ( if x.sawSq checked then squareOsc (midi2cps x.pitch) (bang (P.onOff x.time))
                                      else sawtoothOsc (midi2cps x.pitch) (bang (P.onOff x.time))
                                    )
                                )
                            ) <|> (delay 2000 (bang Silence))

                        )
                        arr
                    ) <$> e
                  )
              )
              \n -> mix
                ( switcher
                    ( case _ of
                        None -> gain_ 1.0 n
                        Lowpass -> lowpass_ 200.0 n
                        Bandpass -> bandpass_ { frequency: 3000.0, q: 10.0 } n
                    )
                    (bang None <|> filterMap (\i -> if i.filter == i.prevFilter then Nothing else Just i.filter) e)
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
                          [ "Sawtooth / square 1" /\ push.notes.n1
                          , "Sawtooth / square 2" /\ push.notes.n2
                          , "Sawtooth / square 3" /\ push.notes.n3
                          , "Sawtooth / square 4" /\ push.notes.n4
                          , "Sawtooth / square 5" /\ push.notes.n5
                          , "Sawtooth / square 6" /\ push.notes.n6
                          , "Sawtooth / square 7" /\ push.notes.n7
                          , "Sawtooth / square 8" /\ push.notes.n8
                          , "Sawtooth / square 9" /\ push.notes.n9
                          , "Sawtooth / square 10" /\ push.notes.n10
                          , "Sawtooth / square 11" /\ push.notes.n11
                          ]
                      )
                    <>
                      [ D.div_ [ text_ "Faster <> Slower", D.input (slider (bang push.slider)) blank ]
                      , D.div_
                          ( map
                              ( \(a /\ b /\ c) -> D.div_
                                  [ D.input
                                      (oneOf [ bang $ D.Xtype := "radio", bang $ D.Id := b, bang $ D.Name := "filter", bang $ D.Value := b, click_ (bang c) ])
                                      blank
                                  , D.label (oneOfMap bang [ D.For := b ]) (text_ a)
                                  ]
                              )
                              [ "No filter" /\ "no_filter" /\ push.filter.none
                              , "Bandpass" /\ "bandpass" /\ push.filter.bandpass
                              , "Lowpass" /\ "lowpass" /\ push.filter.lowpass
                              ]
                          )
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
