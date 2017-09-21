{-# LANGUAGE GADTs, TypeFamilies #-}
module Type.Checkerz.Parser.Predicate where

class Language l where
  type Expr l
  type Pred l

