module Pretty where

import Syntax

import Data.List.NonEmpty (NonEmpty(..))
import Text.PrettyPrint

class Pretty a where
    pretty :: a -> Doc
    pretty = undefined -- TODO

instance Pretty (Ident a) where
    pretty (Ident _ s) = text s

instance Pretty (Block a) where
    pretty (Block _ ss mr) =
            vcat' (map pretty ss)
        $+$ maybe empty pretty mr

instance Pretty (Statement a) where
    pretty (EmptyStmt _) = ";"
    pretty (Assign _ (v:|vs) (e:|es)) =
            sepBy ", " (v:vs)
        <+> "="
        <+> sepBy ", " (e:es)
    pretty (FunCall _ f) = pretty f
    pretty (Label _ i) = "::" <> pretty i <> "::"
    pretty (Break _) = "break"
    pretty (Goto _ i) = "goto" <+> pretty i
    pretty (Do _ b) =
            "do"
        $+$ nest 4 (pretty b)
        $+$ "end"
    pretty (While _ e b) =
            "while" <+> pretty e <+> "do"
        $+$ nest 4 (pretty b)
        $+$ "end"
    pretty (Repeat _ b e) =
            "repeat"
        $+$ nest 4 (pretty b)
        $+$ "until" <+> pretty e
    pretty (If _ ((e,b):|es) mb'') =
            "if" <+> pretty e <+> "then"
        $+$ nest 4 (pretty b)
        $+$ vcat' (map (\(e',b') -> "elseif" <+> pretty e' <+> "then"
                                    $+$ nest 4 (pretty b')) es)
        $+$ maybe empty (\b'' -> "else"
                             $+$ nest 4 (pretty b'')) mb''
        $+$ "end"
    pretty (For _ i e1 e2 me3 b) =
            "for" <+> pretty i <+> "=" <+> pretty e1 <> "," <+> pretty e2 <> maybe empty (("," <+>) . pretty) me3 <+> "do"
        $+$ nest 4 (pretty b)
        $+$ "end"
    pretty (ForIn _ (i:|is) (e:|es) b) =
            "for" <+> sepBy ", " (i:is) <+> "in" <+> sepBy ", " (e:es) <+> "do"
        $+$ nest 4 (pretty b)
        $+$ "end"
    pretty (FunAssign _ (i:|is) mi b) =
            "function" <+> sepBy "." (i:is) <> maybe empty ((":" <>) . pretty) mi <> pretty b
    pretty (LocalFunAssign _ i b) =
            "local function" <+> pretty i <> pretty b
    pretty (LocalAssign _ (i:|is) es) =
            "local" <+> sepBy ", " (i:is) <+> case es of
                                                  [] -> empty
                                                  _  -> "=" <+> sepBy ", " es

instance Pretty (ReturnStatement a) where
    pretty (ReturnStatement _ es) = "return" <+> sepBy ", " es

instance Pretty (Variable a) where
    pretty (VarIdent _ i) = pretty i
    pretty (VarField _ e1 e2) = pretty e1 <> "[" <> pretty e2 <> "]"
    pretty (VarFieldName _ e i) = pretty e <> "." <> pretty i

instance Pretty (Expression a) where
    pretty (Nil _) = "nil"
    pretty (Bool _ True) = "true"
    pretty (Bool _ _) = "false"
    pretty (Integer _ s) = text s
    pretty (Float _ s) = text s
    pretty (String _ s) = "\"" <> text s <> "\""
    pretty (Vararg _) = "..."
    pretty (FunDef _ b) = "function" <> pretty b
    pretty (PrefixExp _ e) = pretty e
    pretty (TableCtor _ t) = pretty t
    pretty (Binop _ op e1 e2) = pretty e1 <+> pretty op <+> pretty e2
    pretty (Unop _ op e) = pretty op <> pretty e

instance Pretty (PrefixExpression a) where
    pretty (PrefixVar _ v) = pretty v
    pretty (PrefixFunCall _ c) = pretty c
    pretty (Parens _ e) = "(" <> pretty e <> ")"

instance Pretty (FunctionCall a) where
    pretty (FunctionCall _ e a) = pretty e <> pretty a
    pretty (MethodCall _ e i a) = pretty e <> ":" <> pretty i <> pretty a

instance Pretty (FunctionArgs a) where
    pretty (Args _ es) = "(" <> sepBy ", " es <> ")"
    pretty (ArgsTable _ t) = pretty t
    pretty (ArgsString _ s) = "\"" <> text s <> "\""

instance Pretty (FunctionBody a) where
    pretty (FunctionBody _ (i:|is) va b) =
            "(" <> sepBy ", " (i:is) <> if va then ", ...)" else ")"
        $+$ nest 4 (pretty b)
        $+$ "end"
    pretty (FunctionBodyVararg _ b) =
            "(...)"
        $+$ nest 4 (pretty b)
        $+$ "end"

instance Pretty (TableConstructor a) where
    pretty (TableConstructor _ []) = "{ }"
    pretty (TableConstructor _ [f]) = "{" <+> pretty f <+> "}"
    pretty (TableConstructor _ fs) =
            "{"
        $+$ nest 4 (vcat' (map pretty fs))
        $+$ "}"

instance Pretty (Field a) where
    pretty (FieldExp _ e1 e2) = "[" <> pretty e1 <> "]" <+> "=" <+> pretty e2
    pretty (FieldIdent _ i e) = pretty i <+> "=" <+> pretty e
    pretty (Field _ e) = pretty e

instance Pretty (Binop a) where
    pretty (Plus _)       = "+"
    pretty (Minus _)      = "-"
    pretty (Mult _)       = "*"
    pretty (FloatDiv _)   = "/"
    pretty (FloorDiv _)   = "//"
    pretty (Exponent _)   = "^"
    pretty (Modulo _)     = "%"
    pretty (BitwiseAnd _) = "&"
    pretty (BitwiseXor _) = "~"
    pretty (BitwiseOr _)  = "|"
    pretty (Rshift _)     = ">>"
    pretty (Lshift _)     = "<<"
    pretty (Concat _)     = ".."
    pretty (Lt _)         = "<"
    pretty (Leq _)        = "<="
    pretty (Gt _)         = ">"
    pretty (Geq _)        = ">="
    pretty (Eq _)         = "=="
    pretty (Neq _)        = "~="
    pretty (And _)        = "and"
    pretty (Or _)         = "or"

instance Pretty (Unop a) where
    pretty (Negate _)     = "-"
    pretty (Not _)        = "not"
    pretty (Length _)     = "#"
    pretty (BitwiseNot _) = "~"

sepBy :: Pretty a => Doc -> [a] -> Doc
sepBy d = hcat . punctuate d . map pretty

vcat' :: [Doc] -> Doc
vcat' = foldr ($+$) empty
