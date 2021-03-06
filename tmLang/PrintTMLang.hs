{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module PrintTMLang where

-- pretty-printer generated by the BNF converter

import AbsTMLang
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)



instance Print TMIdent where
  prt _ (TMIdent i) = doc (showString ( i))



instance Print State where
  prt i e = case e of
    State tmident -> prPrec i 0 (concatD [prt 0 tmident])

instance Print NextCommand where
  prt i e = case e of
    NContinue state -> prPrec i 0 (concatD [prt 0 state])
    NAccept -> prPrec i 0 (concatD [doc (showString "accept")])
    NReject -> prPrec i 0 (concatD [doc (showString "reject")])

instance Print Symbol where
  prt i e = case e of
    SWild -> prPrec i 0 (concatD [doc (showString "*")])
    SBlank -> prPrec i 0 (concatD [doc (showString "_")])
    STrue -> prPrec i 0 (concatD [doc (showString "1")])
    SFalse -> prPrec i 0 (concatD [doc (showString "0")])

instance Print Direction where
  prt i e = case e of
    DStay -> prPrec i 0 (concatD [doc (showString "*")])
    DLeft -> prPrec i 0 (concatD [doc (showString "l")])
    DRight -> prPrec i 0 (concatD [doc (showString "r")])

instance Print Instruction where
  prt i e = case e of
    Instruction state symbol1 symbol2 direction nextcommand -> prPrec i 0 (concatD [prt 0 state, prt 0 symbol1, prt 0 symbol2, prt 0 direction, prt 0 nextcommand])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print Program where
  prt i e = case e of
    Program instructions -> prPrec i 0 (concatD [prt 0 instructions])


