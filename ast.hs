
type Var   = String
data UnOp  = Not | Shl1 | Shr1 | Shr4 | Shr16 
data BinOp = And | Or   | Xor  | Plus
data Exp   = Zero | One | Var Var | IfZ Exp Exp Exp | UOp UnOp Exp | BOp BinOp Exp Exp
           | Fold Exp Exp Var Var Exp
data Prog  = Lam Var Exp

pprint :: Prog -> String
pprint (Lam x e) = "(lambda (" ++ x ++ ") " ++ ppexp e ++ ")"

ppexp Zero = "0"
ppexp One  = "1"
ppexp (Var x) = x
ppexp (IfZ e1 e2 e3) = "(if0 " ++ ppexp e1 ++ " " ++ ppexp e2 ++ " " ++ ppexp e3 ++ ")"
ppexp (UOp o e) = "(" ++ ppuop o ++ " " ++ ppexp e ++ ")"
ppexp (BOp o e1 e2) = "(" ++ ppbop o ++ " " ++ ppexp e1 ++ " " ++ ppexp e2 ++ ")"

ppuop Not   = "not"
ppuop Shl1  = "shl1"
ppuop Shr1  = "shr1"
ppuop Shr4  = "shr4"
ppuop Shr16 = "shr16"

ppbop Plus = "plus"
ppbop And  = "and"
ppbop Or   = "or"
ppbop Xor  = "xor"
