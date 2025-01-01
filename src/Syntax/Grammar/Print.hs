-- File generated by the BNF Converter (bnfc 2.9.5).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for Syntax.

module Syntax.Grammar.Print where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified Syntax.Grammar.Abs

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t, null spc, null rest) of
      (True , _   , True ) -> []             -- remove trailing space
      (False, _   , True ) -> t              -- remove trailing space
      (False, True, False) -> t ++ ' ' : s   -- add space if none
      _                    -> t ++ s
    where
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Syntax.Grammar.Abs.Ident where
  prt _ (Syntax.Grammar.Abs.Ident i) = doc $ showString i
instance Print Syntax.Grammar.Abs.Exp where
  prt i = \case
    Syntax.Grammar.Abs.Var id_ -> prPrec i 15 (concatD [prt 0 id_])
    Syntax.Grammar.Abs.BTrue -> prPrec i 15 (concatD [doc (showString "true")])
    Syntax.Grammar.Abs.BFalse -> prPrec i 15 (concatD [doc (showString "false")])
    Syntax.Grammar.Abs.DVal d -> prPrec i 15 (concatD [prt 0 d])
    Syntax.Grammar.Abs.Rand -> prPrec i 15 (concatD [doc (showString "rand()")])
    Syntax.Grammar.Abs.App exp1 exp2 -> prPrec i 14 (concatD [prt 14 exp1, prt 15 exp2])
    Syntax.Grammar.Abs.InL exp -> prPrec i 13 (concatD [doc (showString "left"), prt 14 exp])
    Syntax.Grammar.Abs.InR exp -> prPrec i 13 (concatD [doc (showString "right"), prt 14 exp])
    Syntax.Grammar.Abs.Fst exp -> prPrec i 13 (concatD [doc (showString "fst"), prt 14 exp])
    Syntax.Grammar.Abs.Snd exp -> prPrec i 13 (concatD [doc (showString "snd"), prt 14 exp])
    Syntax.Grammar.Abs.Min exp -> prPrec i 12 (concatD [doc (showString "-"), prt 11 exp])
    Syntax.Grammar.Abs.Sqrt exp -> prPrec i 12 (concatD [doc (showString "sqrt"), doc (showString "("), prt 13 exp, doc (showString ")")])
    Syntax.Grammar.Abs.Sin exp -> prPrec i 12 (concatD [doc (showString "sin"), doc (showString "("), prt 13 exp, doc (showString ")")])
    Syntax.Grammar.Abs.Cos exp -> prPrec i 12 (concatD [doc (showString "cos"), doc (showString "("), prt 13 exp, doc (showString ")")])
    Syntax.Grammar.Abs.EPow exp -> prPrec i 12 (concatD [doc (showString "exp"), doc (showString "("), prt 13 exp, doc (showString ")")])
    Syntax.Grammar.Abs.Mul exp1 exp2 -> prPrec i 10 (concatD [prt 10 exp1, doc (showString "*"), prt 11 exp2])
    Syntax.Grammar.Abs.Div exp1 exp2 -> prPrec i 10 (concatD [prt 10 exp1, doc (showString "/"), prt 11 exp2])
    Syntax.Grammar.Abs.Mod exp1 exp2 -> prPrec i 10 (concatD [prt 10 exp1, doc (showString "%"), prt 11 exp2])
    Syntax.Grammar.Abs.Add exp1 exp2 -> prPrec i 9 (concatD [prt 9 exp1, doc (showString "+"), prt 10 exp2])
    Syntax.Grammar.Abs.Sub exp1 exp2 -> prPrec i 9 (concatD [prt 9 exp1, doc (showString "-"), prt 10 exp2])
    Syntax.Grammar.Abs.Eq exp1 exp2 -> prPrec i 8 (concatD [prt 8 exp1, doc (showString "=="), prt 9 exp2])
    Syntax.Grammar.Abs.Lt exp1 exp2 -> prPrec i 8 (concatD [prt 8 exp1, doc (showString "<"), prt 9 exp2])
    Syntax.Grammar.Abs.Gt exp1 exp2 -> prPrec i 8 (concatD [prt 8 exp1, doc (showString ">"), prt 9 exp2])
    Syntax.Grammar.Abs.Neq exp1 exp2 -> prPrec i 8 (concatD [prt 8 exp1, doc (showString "!="), prt 9 exp2])
    Syntax.Grammar.Abs.Leq exp1 exp2 -> prPrec i 8 (concatD [prt 8 exp1, doc (showString "<="), prt 9 exp2])
    Syntax.Grammar.Abs.Geq exp1 exp2 -> prPrec i 8 (concatD [prt 8 exp1, doc (showString ">="), prt 9 exp2])
    Syntax.Grammar.Abs.Not exp -> prPrec i 7 (concatD [doc (showString "!"), prt 8 exp])
    Syntax.Grammar.Abs.And exp1 exp2 -> prPrec i 6 (concatD [prt 6 exp1, doc (showString "and"), prt 7 exp2])
    Syntax.Grammar.Abs.Or exp1 exp2 -> prPrec i 6 (concatD [prt 6 exp1, doc (showString "or"), prt 7 exp2])
    Syntax.Grammar.Abs.Ite exp1 exp2 exp3 -> prPrec i 5 (concatD [doc (showString "if"), prt 5 exp1, doc (showString "then"), prt 6 exp2, doc (showString "else"), prt 6 exp3])
    Syntax.Grammar.Abs.Match exp1 id_1 exp2 id_2 exp3 -> prPrec i 4 (concatD [doc (showString "match"), prt 4 exp1, doc (showString "{"), doc (showString "L"), prt 0 id_1, doc (showString "->"), prt 5 exp2, doc (showString ";"), doc (showString "R"), prt 0 id_2, doc (showString "->"), prt 5 exp3, doc (showString "}")])
    Syntax.Grammar.Abs.Tup exp1 exp2 -> prPrec i 3 (concatD [doc (showString "("), prt 3 exp1, doc (showString ","), prt 3 exp2, doc (showString ")")])
    Syntax.Grammar.Abs.Abstr id_ exp -> prPrec i 0 (concatD [doc (showString "lam"), prt 0 id_, doc (showString "->"), prt 1 exp])

instance Print Syntax.Grammar.Abs.Type where
  prt i = \case
    Syntax.Grammar.Abs.TDouble -> prPrec i 3 (concatD [doc (showString "Double")])
    Syntax.Grammar.Abs.TBool -> prPrec i 3 (concatD [doc (showString "Bool")])
    Syntax.Grammar.Abs.TProd type_1 type_2 -> prPrec i 2 (concatD [doc (showString "("), prt 2 type_1, doc (showString ","), prt 2 type_2, doc (showString ")")])
    Syntax.Grammar.Abs.TFun type_1 type_2 -> prPrec i 1 (concatD [prt 1 type_1, doc (showString "->"), prt 2 type_2])
    Syntax.Grammar.Abs.TCoprod type_1 type_2 -> prPrec i 0 (concatD [prt 0 type_1, doc (showString "+"), prt 1 type_2])
