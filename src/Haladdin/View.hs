{-# LANGUAGE OverloadedStrings          #-}
module Haladdin.View where

import           Control.Lens hiding (Level, view)
import           Data.Ext
import           Data.Geometry.Box
import           Data.Geometry.Point
import           Data.Geometry.Vector
import           Haladdin.Action
import           Haladdin.Model
import           Miso
import           Miso.String (ms)
import           Miso.Svg hiding (height_, id_, style_, width_)

--------------------------------------------------------------------------------

view   :: Model -> View Action
view m = defSvg_ [ style_ $ "border-style" =: "solid" ] [render m]


defSvg_          :: [Attribute action] -> [View action] -> View action
defSvg_ ats body = svg_ ([ width_     "800"
                         , height_    "600"
                         , transform_ "scale(1,-1)"
                         ] <> ats
                        )
                        body

menuSvg_      :: View action -> View action
menuSvg_ body = defSvg_ [] [body]

--defaultSVGWithVi


class Renderable t where
  render :: t -> View Action

instance Renderable t => Renderable [t] where
  render = g_ [] . map render


instance Renderable GameMode where
  render = \case
      Playing gs -> render gs
      Paused gs  -> render gs -- defSvg_  [] []
      GameOver s -> g_ []
                       [ text_ [] ["Game Over: "]
                       , render s
                       ]
      Finished s -> g_ []
                       [ text_ [] ["Finished!"]
                       , render s
                       ]

instance Renderable GameState where
  render gs = g_ []
                 [ defSvg_  [ viewBox_ vb
                            ] [body]
                 , renderStatusBar $ gs^.player
                 ]
    where
      ViewPort c (Vector2 w h) = gs^.viewPort

      body = g_ []
                [ render $ gs^.world
                , render $ gs^.player
                ]
      -- tf = ms . concat $ [ "translate("
      --                    , show $ -(w/2)
      --                    , ", "
      --                    , show $ -(h/2)
      --                    , ")"
      --                    ]

      vb = ms . unwords . map show $ [ c^.xCoord - (w / 2)
                                     , c^.yCoord - (h / 2)
                                     , w
                                     , h
                                     ]

renderStatusBar   :: Player -> View Action
renderStatusBar p = g_ []
                 [ rect_ [ x_      $ ms (0   :: Int)
                         , y_      $ ms (600 - statusBarHeight)
                         , height_ $ ms statusBarHeight
                         , width_   "100%"
                         , style_  $ "fill" =: "green"
                         ] []
                 ]
  where
    statusBarHeight = 40 :: Int


instance Renderable World where
  render (World _ l _) = render l

instance Renderable Level where
  render (Level cs os es t) = g_ [] [ render cs
                                    , render os
                                    , render es
                                    ]

instance Renderable Collectable where
  render = undefined

instance Renderable Item where
  render (Item bx k) = renderRectWith [ style_ $ "fill" =: "burlywood"
                                      ] bx


renderRectWith        :: [Attribute action] -> Rectangle p R -> View action
renderRectWith ats bx = rect_ ([ x_     . ms $ bx^.minP.core.cwMin.xCoord
                               , y_     . ms $ bx^.minP.core.cwMin.yCoord
                               , width_  $ ms (width bx)
                               , height_ $ ms (height bx)
                               ] <> ats ) []



instance Renderable Enemy where
  render = undefined


instance Renderable Player where
  render p = render $ p^.aladdin

instance Renderable Aladdin where
  render (Aladdin p d st _ s _) = g_ []
                                     [ rect_ [ x_      . ms $ p^.xCoord
                                             , y_      . ms $ p^.yCoord
                                             , width_  $ ms (100 :: Int)
                                             , height_ $ ms (40  :: Int)
                                             , id_       "aladdin"
                                             ] []
                                     ]

instance Renderable Score where
  render (Score s) = text_ [] [text $ ms s]
