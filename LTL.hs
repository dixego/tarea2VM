module LTL where

data LTL           -- Representación de fórmulas LTL en forma positiva
  = TLit String    -- Literal
  | TNeg String    -- Negación de literal
  | TOr  LTL LTL   -- Disyunción
  | TAnd LTL LTL   -- Conjunción
  | TU   LTL LTL   -- Until
  | TR   LTL LTL   -- Release
  | TX   LTL       -- NeXt
  deriving (Eq, Ord, Show)

ttrue  = TLit "p" `TOr` TNeg "p"
tfalse = TLit "p" `TAnd` TNeg "p"
