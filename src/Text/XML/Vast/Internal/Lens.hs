{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Text.XML.Vast.Internal.Lens (
  -- * Lenses, traversals for 'Element'
  Element (..),
  (...),

  -- ** Names
  name,
  localName,
  el,
  ell,
  named,

  -- ** Attributes
  attributeIs,
  attributeSatisfies,
  attributeSatisfies',
  withoutAttribute,
  attr,
  attribute,
  attrs,

  -- ** Contents
  text,
  comment,

  -- ** Children
  nodes,

  -- * Prisms for 'Node'
  Node (..),
  _Element,
  _Content,
  AsInstruction (..),
  AsComment (..),

  -- * Lenses for 'Document'
  Document (..),
  root,
  prologue,
  epilogue,
  doctype,

  -- * Lenses for 'Name'
  Name (..),
  _nameLocalName,
  _nameNamespace,
  _namePrefix,

  -- * Lenses for 'Instruction'
  Instruction (..),
  _instructionTarget,
  _instructionData,
) where

import Control.Lens
import qualified Data.CaseInsensitive as CI
import Data.Coerce
import Data.Map (Map)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Text.XML
import Text.XML.Vast.Internal.Other (el, ell)
import qualified Text.XML.Vast.Internal.Other as XML

-- | Representational equality
type a ~~ b = Coercible a b

-- ** Coercible xml-conduit lenses

-- These lenses are just coerced versions of those defined at 'Text.XML.Lens'.

-- | The root element of the document.
root :: (a ~~ Document, b ~~ Element) => Lens' a b
root = coerced . XML.root . coerced
{-# INLINE root #-}

prologue :: a ~~ Document => Lens' a Prologue
prologue = coerced . XML.prologue
{-# INLINE prologue #-}

epilogue :: a ~~ Document => Lens' a [Miscellaneous]
epilogue = coerced . XML.epilogue
{-# INLINE epilogue #-}

doctype :: Lens' Prologue (Maybe Doctype)
doctype = XML.doctype
{-# INLINE doctype #-}

class AsInstruction t where
  _Instruction :: Prism' t Instruction

_instructionTarget :: Lens' Instruction Text
_instructionTarget f (Instruction t d) = f t <&> \t' -> Instruction t' d
{-# INLINE _instructionTarget #-}

_instructionData :: Lens' Instruction Text
_instructionData f (Instruction t d) = f d <&> \d' -> Instruction t d'
{-# INLINE _instructionData #-}

instance AsInstruction Node where
  _Instruction = prism' NodeInstruction $ \case
    NodeInstruction e -> Just e
    _ -> Nothing
  {-# INLINE _Instruction #-}

instance AsInstruction Miscellaneous where
  _Instruction = prism' MiscInstruction $ \case
    MiscInstruction e -> Just e
    _ -> Nothing
  {-# INLINE _Instruction #-}

class AsComment t where
  _Comment :: Prism' t Text

instance AsComment Node where
  _Comment = prism' NodeComment $ \case
    NodeComment e -> Just e
    _ -> Nothing
  {-# INLINE _Comment #-}

instance AsComment Miscellaneous where
  _Comment = prism' MiscComment $ \case
    MiscComment e -> Just e
    _ -> Nothing
  {-# INLINE _Comment #-}

_nameLocalName :: Lens' Name Text
_nameLocalName f n = f (nameLocalName n) <&> \x -> n{nameLocalName = x}
{-# INLINE _nameLocalName #-}

_nameNamespace :: Lens' Name (Maybe Text)
_nameNamespace f n = f (nameNamespace n) <&> \x -> n{nameNamespace = x}
{-# INLINE _nameNamespace #-}

_namePrefix :: Lens' Name (Maybe Text)
_namePrefix f n = f (namePrefix n) <&> \x -> n{namePrefix = x}
{-# INLINE _namePrefix #-}

_Element :: a ~~ Element => Prism' Node a
_Element = XML._Element . coerced
{-# INLINE _Element #-}

_Content :: Prism' Node Text
_Content = prism' NodeContent $ \case
  NodeContent e -> Just e
  _ -> Nothing
{-# INLINE _Content #-}

name :: a ~~ Element => Lens' a Name
name = coerced . XML.name
{-# INLINE name #-}

localName :: a ~~ Element => Lens' a Text
localName = coerced . XML.localName
{-# INLINE localName #-}

attrs :: a ~~ Element => Lens' a (Map Name Text)
attrs = coerced . XML.attrs
{-# INLINE attrs #-}

nodes :: a ~~ Element => Lens' a [Node]
nodes = coerced . XML.nodes
{-# INLINE nodes #-}

attr :: a ~~ Element => Name -> Traversal' a Text
attr n = attrs . ix n
{-# INLINE attr #-}

attribute :: a ~~ Element => Name -> Lens' a (Maybe Text)
attribute n = attrs . at n
{-# INLINE attribute #-}

-- | Traverse elements which has the specified *local* name (case-insensitive).
named :: (a ~~ Element, b ~~ Element) => CI.CI Text -> Traversal' a b
named n = coerced . XML.named n . coerced
{-# INLINE named #-}

attributeSatisfies ::
  (a ~~ Element) =>
  Name ->
  (Text -> Bool) ->
  Traversal' a a
attributeSatisfies n p = attributeSatisfies' n (maybe False p)
{-# INLINE attributeSatisfies #-}

attributeSatisfies' ::
  (a ~~ Element) =>
  Name ->
  (Maybe Text -> Bool) ->
  Traversal' a a
attributeSatisfies' n p = coerced . XML.attributeSatisfies' n p . coerced
{-# INLINE attributeSatisfies' #-}

withoutAttribute ::
  (a ~~ Element) =>
  Name ->
  Traversal' a a
withoutAttribute n = attributeSatisfies' n isNothing
{-# INLINE withoutAttribute #-}

attributeIs ::
  (a ~~ Element) =>
  Name ->
  Text ->
  Traversal' a a
attributeIs n v = attributeSatisfies n (== v)
{-# INLINE attributeIs #-}

-- | Traverse all contents of the element.
text :: a ~~ Element => Traversal' a Text
text = coerced . XML.text
{-# INLINE text #-}

-- | Traverse all comments of the element.
comment :: a ~~ Element => Traversal' a Text
comment = nodes . traverse . _Comment
{-# INLINE comment #-}

-- | 'plate' traverses over its sub-elements.
instance a ~~ Element => Plated a where
  plate = nodes . traverse . _Element
  {-# INLINE plate #-}
