{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Text.XML.Vast.Coerced.Lens (
  module Text.XML.Vast.Internal.Lens,
  HasURL (..),
  HasAdSystem (..),
  HasError (..),
  HasImpression (..),
  HasCreatives (..),
  HasExtensions (..),
  vast,
  nobanner,
  ad,
  inline,
  wrapper,
  adTitle,
  description,
  advertiser,
  pricing,
  survey,
  creative,
  creativeExtensions,
  creativeExtension,
  linear,
  adParameters,
  duration,
  mediaFiles,
  mediaFile,
  trackingEvents,
  tracking,
  videoClicks,
  clickThrough,
  clickTracking,
  customClick,
  icons,
  icon,
  iconClicks,
  iconClickThrough,
  iconClickTracking,
  iconViewTracking,
  nonLinear,
  companionAds,
  extension,
  vastAdTagURI,
) where

import Control.Lens
import Data.Coerce (Coercible)
import Data.Text (Text)
import Text.XML.Vast.Internal.Lens

import Text.XML.Vast.Coerced.Types

-- ** Basic Lenses

type struct >> field = Traversal' struct field

-- | Representational equality
type a ~~ b = Coercible a b

class a ~~ Element => HasURL a where
  url :: a >> Text
  url = text
  {-# INLINE url #-}

instance HasURL Error
instance HasURL Impression
instance HasURL MediaFile
instance HasURL Tracking
instance HasURL ClickTracking
instance HasURL VastAdTagURI

vast :: VastDocument >> Vast
vast = root @_ @Vast . named "VAST"
{-# INLINE vast #-}

nobanner :: VastDocument >> Nobanner
nobanner = root @_ @Vast . named "nobanner"

ad :: Vast >> Ad
ad = named "Ad"
{-# INLINE ad #-}

inline :: Ad >> InLine
inline = named "InLine"
{-# INLINE inline #-}

wrapper :: Ad >> Wrapper
wrapper = named "Wrapper"
{-# INLINE wrapper #-}

class a ~~ Element => HasAdSystem a where
  adSystem :: a >> AdSystem
  adSystem = named "AdSystem"
  {-# INLINE adSystem #-}

instance HasAdSystem InLine
instance HasAdSystem Wrapper

adTitle :: InLine >> AdTitle
adTitle = named "AdTitle"
{-# INLINE adTitle #-}

description :: InLine >> Description
description = named "Description"
{-# INLINE description #-}

advertiser :: InLine >> Advertiser
advertiser = named "Advertiser"
{-# INLINE advertiser #-}

pricing :: InLine >> Pricing
pricing = named "Pricing"
{-# INLINE pricing #-}

survey :: InLine >> Survey
survey = named "Survey"
{-# INLINE survey #-}

class a ~~ Element => HasError a where
  error' :: a >> Error
  error' = named "Error"
  {-# INLINE error' #-}

instance HasError Vast
instance HasError InLine
instance HasError Wrapper

class a ~~ Element => HasImpression a where
  impression :: a >> Impression
  impression = named "Impression"
  {-# INLINE impression #-}

instance HasImpression InLine
instance HasImpression Wrapper

class a ~~ Element => HasCreatives a where
  creatives :: a >> Creatives
  creatives = named "Creatives"
  {-# INLINE creatives #-}

instance HasCreatives InLine
instance HasCreatives Wrapper

creative :: Creatives >> Creative
creative = named "Creative"
{-# INLINE creative #-}

creativeExtensions :: Creative >> CreativeExtensions
creativeExtensions = named "CreativeExtensions"
{-# INLINE creativeExtensions #-}

creativeExtension :: CreativeExtensions >> CreativeExtension
creativeExtension = named "CreativeExtension"
{-# INLINE creativeExtension #-}

linear :: Creative >> Linear
linear = named "Linear"
{-# INLINE linear #-}

adParameters :: Linear >> AdParameters
adParameters = named "AdParameters"
{-# INLINE adParameters #-}

duration :: Linear >> Duration
duration = named "Duration"
{-# INLINE duration #-}

mediaFiles :: Linear >> MediaFiles
mediaFiles = named "MediaFiles"
{-# INLINE mediaFiles #-}

mediaFile :: MediaFiles >> MediaFile
mediaFile = named "MediaFile"
{-# INLINE mediaFile #-}

trackingEvents :: Linear >> TrackingEvents
trackingEvents = named "TrackingEvents"
{-# INLINE trackingEvents #-}

tracking :: TrackingEvents >> Tracking
tracking = named "Tracking"
{-# INLINE tracking #-}

videoClicks :: Linear >> VideoClicks
videoClicks = named "VideoClicks"
{-# INLINE videoClicks #-}

clickThrough :: VideoClicks >> ClickThrough
clickThrough = named "ClickThrough"
{-# INLINE clickThrough #-}

clickTracking :: VideoClicks >> ClickTracking
clickTracking = named "ClickTracking"
{-# INLINE clickTracking #-}

customClick :: VideoClicks >> CustomClick
customClick = named "CustomClick"
{-# INLINE customClick #-}

icons :: Linear >> Icons
icons = named "Icons"
{-# INLINE icons #-}

icon :: Icons >> Icon
icon = named "Icon"
{-# INLINE icon #-}

iconClicks :: Icon >> IconClicks
iconClicks = named "IconClicks"
{-# INLINE iconClicks #-}

iconClickThrough :: IconClicks >> IconClickThrough
iconClickThrough = named "IconClickThrough"
{-# INLINE iconClickThrough #-}

iconClickTracking :: IconClicks >> IconClickTracking
iconClickTracking = named "IconClickTracking"
{-# INLINE iconClickTracking #-}

iconViewTracking :: Icon >> IconViewTracking
iconViewTracking = named "IconViewTracking"
{-# INLINE iconViewTracking #-}

nonLinear :: Creative >> NonLinear
nonLinear = named "NonLinear"
{-# INLINE nonLinear #-}

companionAds :: Creative >> CompanionAds
companionAds = named "CompanionAds"
{-# INLINE companionAds #-}

class a ~~ Element => HasExtensions a where
  extensions :: a >> Extensions
  extensions = named "Extensions"
  {-# INLINE extensions #-}

instance HasExtensions InLine
instance HasExtensions Wrapper

extension :: Extensions >> Extension
extension = named "Extension"
{-# INLINE extension #-}

vastAdTagURI :: Wrapper >> VastAdTagURI
vastAdTagURI = named "VASTAdTagURI"
{-# INLINE vastAdTagURI #-}

-- ** Coercible xml-conduit lenses

-- These lenses are just coerced versions of those defined at 'Text.XML.Lens'.
