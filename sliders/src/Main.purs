module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), isJust)
import Data.Number (pow)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (D1)
import Deku.Attribute ((:=))
import Deku.Control (blank, plant, text, text_)
import Deku.DOM as D
import Deku.Listeners (click, slider)
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import FRP.Event (Event, bang)
import FRP.Event.VBus (V, vbus)
import Type.Proxy (Proxy(..))
import WAGS.Control (constant, gain, gain_, sinOsc, waveShaper)
import WAGS.Core (class ToAudioParameter, Node, _none, bangOn)
import WAGS.Interpret (makeFloatArray)
import WAGS.Math (calcSlope)
import WAGS.Properties as P
import WAGS.Run (run2_)

-- | This is our event bus.
-- | It holds all of the possible events we'll receive in our application.
-- | `onOff` will turn the audio on and off, and everything else is for a slider.
type Bus = V
  ( gainWidth :: Number
  , gainRateWidth :: Number
  , gainRateRate :: Number
  , gainRateBase :: Number
  , gainBase :: Number
  , freqWidth :: Number
  , freqRate :: Number
  , freqBase :: Number
  , onOff :: Maybe (Effect Unit)
  )

main :: Effect Unit
main = do
  let range = 0 .. 127
  -- | We make a waveshaper that clamps all negative values to 0.0
  -- | for our gain. This will allow for silence.
  let clamp1 = makeFloatArray ((range $> 0.0) <> (toNumber >>> (_ / 127.0) <$> range))
  -- | We make a waveshaper that scales between 0.1 and 1.0, favoring low frequencies
  let clamp2 = makeFloatArray (toNumber >>> calcSlope 0.0 0.0 127.0 1.0 >>> (_ `pow` 3.0) >>> calcSlope 0.0 0.1 1.0 10.0 <$> range)
  runInBody1
    ( vbus (Proxy :: Proxy Bus) \push event -> do
        let
          onOff = event.onOff <|> bang Nothing
          -- | We create an lfo with a width, a rate, and a base control
          -- | width * sine (rate) + base
          -- | As well as initial values for all of these
          lfo
            :: forall width rate base lock p
             . ToAudioParameter width lock p
            => ToAudioParameter rate lock p
            => ToAudioParameter base lock p
            => Number
            -> Event width
            -> Number
            -> Event rate
            -> Number
            -> Event base
            -> Node D1 lock p
          lfo a width c rate e base = gain_ 1.0
            [ gain a
                -- | `P.gain`, `P.frequency` and `P.offset` all produce
                -- | values that can be consumed by units likt `gain`,
                -- | `sinOsc` and `constant`.
                (P.gain <$> width)
                -- | bangOn is needed to turn a generator on
                -- | generators are off by default
                -- | The tie fighter is used to compose events together
                -- | Here, we want to listen for turning on/off _and_
                -- | changes to frequency
                (sinOsc c (bangOn <|> P.frequency <$> rate))
            , constant e (bangOn <|> P.offset <$> base)
            ]

          graph :: forall lock. Array (Node _ lock _)
          graph =
            [ gain 0.0
                -- | Our gain node takes a single event
                -- | that sets up an a-rate control.
                -- | This control itself accepts events, as
                -- | we see in the `lfo` function above.
                -- | This is a powerful mechanism to create nested
                -- | event logics.
                ( bang $ P.gain
                    -- | Our first waveshaper gates our gain
                    ( waveShaper
                        { curve: clamp1
                        , oversample: _none
                        }
                        ( lfo 0.1
                            -- | calcSlope x0 y0 x1 y1 x
                            (calcSlope 0.0 0.03 100.0 0.6 <$> event.gainWidth)
                            5.0
                            -- | Our second waveshaper gates our rate
                            ( bang
                                ( waveShaper
                                    { curve: clamp2
                                    , oversample: _none
                                    }
                                    ( lfo 3.0
                                        (calcSlope 0.0 0.03 100.0 6.0 <$> event.gainRateWidth)
                                        2.0
                                        (calcSlope 0.0 0.1 100.0 4.0 <$> event.gainRateRate)
                                        5.0
                                        (calcSlope 0.0 0.1 100.0 10.0 <$> event.gainRateBase)
                                    )
                                )
                            )
                            0.0
                            (calcSlope 0.0 (-0.2) 100.0 0.2 <$> event.gainBase)
                        )
                    )
                )
                [ sinOsc 440.0
                    ( bangOn <|>
                        ( bang
                            ( P.frequency
                                ( lfo 0.1
                                    (calcSlope 0.0 1.0 100.0 130.0 <$> event.freqWidth)
                                    40.0
                                    (calcSlope 0.0 0.1 100.0 10.0 <$> event.freqRate)
                                    440.0
                                    (calcSlope 0.0 220.0 100.0 670.0 <$> event.freqBase)
                                )
                            )
                        )
                    )

                ]
            ]
        plant $ D.div (bang $ D.Class := "w-screen h-screen flex flex-col")
          [ D.div (bang $ D.Class := "flex-grow") blank
          , D.div (bang $ D.Class := "flex flex-row")
              [ D.div (bang $ D.Class := "flex-grow") blank
              , D.div (bang $ D.Class := "")
                  ( ( makeSlider <$>
                        [ "Gain width" /\ push.gainWidth
                        , "Gain rate width" /\ push.gainRateWidth
                        , "Gain rate rate" /\ push.gainRateRate
                        , "Gain rate base" /\ push.gainRateBase
                        , "Gain base" /\ push.gainBase
                        , "Freq width" /\ push.freqWidth
                        , "Freq rate" /\ push.freqRate
                        , "Freq base" /\ push.freqBase
                        ]
                    ) <>
                      [ D.div_
                          [ D.button
                              ( (bang $ D.Class := "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded") <|> click
                                  ( onOff <#> case _ of
                                      Nothing ->
                                        run2_ graph <#> Just >>= push.onOff
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
  where
  makeSlider (t /\ p) = D.div_ [ text_ t, D.input (slider (bang p)) blank ]