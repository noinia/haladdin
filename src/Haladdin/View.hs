{-# LANGUAGE OverloadedStrings          #-}
module Haladdin.View where

import           Control.Lens hiding (Level, view)
import           Data.Ext
import           Data.Geometry.Box
import           Data.Geometry.Point
import           Data.Geometry.Transformation
import           Data.Geometry.Vector
import qualified Data.List as List
import           Haladdin.Action
import           Haladdin.Model
import           Miso
import           Miso.String (ms)
import           Miso.Svg hiding (height_, id_, style_, width_)

--------------------------------------------------------------------------------

view   :: Model -> View Action
view m = div_ []
              [ defSvg_ [ style_ $ "border-style" =: "solid" ] [render m]
              , div_ [] [text . ms . show $ m]
              ]

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
                 , text_ [ x_ $ ms (20  :: Int)
                         , y_ $ ms (580 :: Int)
                         ]
                         [ "Health: " ]
                 ]
  where
    statusBarHeight = 40 :: Int


instance Renderable World where
  render (World _ l _) = render l

instance Renderable Level where
  render (Level cs os es t _) = g_ [] [ render cs
                                      , render os
                                      , render es
                                      , render t
                                      ]

instance Renderable Collectable where
  render = undefined

instance Renderable Item where
  render (Item bx k) = renderRectWith [ style_ $ renderStyle k
                                      ] bx

renderStyle = \case
  Wall      -> "fill" =: "burlywood"
  Water     -> "fill" =: "blue"
  Coal      -> "fill" =: "red"
  Ladder    -> "fill" =: "brown"

instance Renderable TargetArea where
  render (TargetArea r) = renderRectWith [style_ $ "fill" =: "green"] r


renderRectWith        :: [Attribute action] -> Rectangle p R -> View action
renderRectWith ats bx = rect_ ([ x_     . ms $ bx^.minP.core.cwMin.xCoord
                               , y_     . ms $ bx^.minP.core.cwMin.yCoord
                               , width_  $ ms (width bx)
                               , height_ $ ms (height bx)
                               ] <> ats ) []



instance Renderable Enemy where
  render (Enemy p _ k st _) = undefined


instance Renderable Player where
  render p = render $ p^.aladdin

instance Renderable Aladdin where
  render (Aladdin p v st _ s _) = g_ []
                                     [ polygon_ [ points_ $ mkMS pts
                                                , id_       "aladdin"
                                                ] []
                                     , renderSword (List.last pts)  s
                                     ]
    where
      pts = [p, p .+^ Vector2 0 h, p .+^ Vector2 w (h/2)]

      -- aladdin's height
      h = case st of
            Crouching -> 50
            _         -> 100

      w = 50 * case v^.xComponent > 0 of
                 True  -> 1
                 False -> (-1)

      mkMS = ms . unwords . map (\(Point2 x y) -> show x <> "," <> show y)



renderSword p = \case
      Shielded -> g_ [] []
      Extended -> renderRectWith [] $ swordExtended p

swordExtended p = translateBy (toVec p) $ swordExtendedInital

swordExtendedInital = box (ext $ origin) (ext $ Point2 20 5)

instance Renderable Score where
  render (Score s) = text_ [] [text $ ms s]
