-- file: ch06/BrokenClass.hs
{-# LANGUAGE TypeSynonymInstances #-} --型パラメータを具体化したデータ型を型クラスのinstanceにするのを許す言語拡張
{-# LANGUAGE FlexibleInstances #-}
module BrokenClass where
import SimpleJSON 
import JSONClass


instance (JSON a) => JSON [a] where
  toJValue = undefined
  fromJValue =undefined
  
instance (JSON a) => JSON [(String, a)] where
  toJValue = undefined
  fromJValue =undefined
  