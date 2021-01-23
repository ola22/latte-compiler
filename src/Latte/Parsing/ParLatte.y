-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Latte.Parsing.ParLatte where
import Latte.Parsing.AbsLatte
import Latte.Parsing.LexLatte
import Latte.Parsing.ErrM

}

-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%name pProgram_internal Program
%token
  '!' { PT _ (TS _ 1) }
  '!=' { PT _ (TS _ 2) }
  '%' { PT _ (TS _ 3) }
  '&&' { PT _ (TS _ 4) }
  '(' { PT _ (TS _ 5) }
  ')' { PT _ (TS _ 6) }
  '*' { PT _ (TS _ 7) }
  '+' { PT _ (TS _ 8) }
  '++' { PT _ (TS _ 9) }
  ',' { PT _ (TS _ 10) }
  '-' { PT _ (TS _ 11) }
  '--' { PT _ (TS _ 12) }
  '.' { PT _ (TS _ 13) }
  '/' { PT _ (TS _ 14) }
  ':' { PT _ (TS _ 15) }
  ';' { PT _ (TS _ 16) }
  '<' { PT _ (TS _ 17) }
  '<=' { PT _ (TS _ 18) }
  '=' { PT _ (TS _ 19) }
  '==' { PT _ (TS _ 20) }
  '>' { PT _ (TS _ 21) }
  '>=' { PT _ (TS _ 22) }
  '[' { PT _ (TS _ 23) }
  '[]' { PT _ (TS _ 24) }
  ']' { PT _ (TS _ 25) }
  'boolean' { PT _ (TS _ 26) }
  'class' { PT _ (TS _ 27) }
  'else' { PT _ (TS _ 28) }
  'false' { PT _ (TS _ 29) }
  'for' { PT _ (TS _ 30) }
  'if' { PT _ (TS _ 31) }
  'int' { PT _ (TS _ 32) }
  'new' { PT _ (TS _ 33) }
  'null' { PT _ (TS _ 34) }
  'return' { PT _ (TS _ 35) }
  'string' { PT _ (TS _ 36) }
  'true' { PT _ (TS _ 37) }
  'void' { PT _ (TS _ 38) }
  'while' { PT _ (TS _ 39) }
  '{' { PT _ (TS _ 40) }
  '||' { PT _ (TS _ 41) }
  '}' { PT _ (TS _ 42) }

  L_ident {PT _ (TV _)}
  L_integ {PT _ (TI _)}
  L_quoted {PT _ (TL _)}

%%

Ident :: {
  (Maybe (Int, Int), Ident)
}
: L_ident {
  (Just (tokenLineCol $1), Ident (prToken $1)) 
}

Integer :: {
  (Maybe (Int, Int), Integer)
}
: L_integ {
  (Just (tokenLineCol $1), read (prToken $1)) 
}

String :: {
  (Maybe (Int, Int), String)
}
: L_quoted {
  (Just (tokenLineCol $1), prToken $1)
}

Program :: {
  (Maybe (Int, Int), Program (Maybe (Int, Int)))
}
: ListTopDef {
  (fst $1, Latte.Parsing.AbsLatte.Program (fst $1)(snd $1)) 
}

TopDef :: {
  (Maybe (Int, Int), TopDef (Maybe (Int, Int)))
}
: Type Ident '(' ListArg ')' Block {
  (fst $1, Latte.Parsing.AbsLatte.FnDef (fst $1)(snd $1)(snd $2)(snd $4)(snd $6)) 
}
| 'class' Ident '{' ListStructBody '}' {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.StructDef (Just (tokenLineCol $1)) (snd $2)(snd $4)) 
}

StructBody :: {
  (Maybe (Int, Int), StructBody (Maybe (Int, Int)))
}
: Type Ident ';' {
  (fst $1, Latte.Parsing.AbsLatte.Attr (fst $1)(snd $1)(snd $2)) 
}

ListTopDef :: {
  (Maybe (Int, Int), [TopDef (Maybe (Int, Int))]) 
}
: TopDef {
  (fst $1, (:[]) (snd $1)) 
}
| TopDef ListTopDef {
  (fst $1, (:) (snd $1)(snd $2)) 
}

ListStructBody :: {
  (Maybe (Int, Int), [StructBody (Maybe (Int, Int))]) 
}
: StructBody {
  (fst $1, (:[]) (snd $1)) 
}
| StructBody ListStructBody {
  (fst $1, (:) (snd $1)(snd $2)) 
}

Arg :: {
  (Maybe (Int, Int), Arg (Maybe (Int, Int)))
}
: Type Ident {
  (fst $1, Latte.Parsing.AbsLatte.Arg (fst $1)(snd $1)(snd $2)) 
}

ListArg :: {
  (Maybe (Int, Int), [Arg (Maybe (Int, Int))]) 
}
: {
  (Nothing, [])
}
| Arg {
  (fst $1, (:[]) (snd $1)) 
}
| Arg ',' ListArg {
  (fst $1, (:) (snd $1)(snd $3)) 
}

Block :: {
  (Maybe (Int, Int), Block (Maybe (Int, Int)))
}
: '{' ListStmt '}' {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.Block (Just (tokenLineCol $1)) (reverse (snd $2)))
}

ListStmt :: {
  (Maybe (Int, Int), [Stmt (Maybe (Int, Int))]) 
}
: {
  (Nothing, [])
}
| ListStmt Stmt {
  (fst $1, flip (:) (snd $1)(snd $2)) 
}

Stmt :: {
  (Maybe (Int, Int), Stmt (Maybe (Int, Int)))
}
: ';' {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.Empty (Just (tokenLineCol $1)))
}
| Block {
  (fst $1, Latte.Parsing.AbsLatte.BStmt (fst $1)(snd $1)) 
}
| Type ListItem ';' {
  (fst $1, Latte.Parsing.AbsLatte.Decl (fst $1)(snd $1)(snd $2)) 
}
| Expr '=' Expr ';' {
  (fst $1, Latte.Parsing.AbsLatte.Ass (fst $1)(snd $1)(snd $3)) 
}
| Ident '++' ';' {
  (fst $1, Latte.Parsing.AbsLatte.Incr (fst $1)(snd $1)) 
}
| Ident '--' ';' {
  (fst $1, Latte.Parsing.AbsLatte.Decr (fst $1)(snd $1)) 
}
| 'return' Expr ';' {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.Ret (Just (tokenLineCol $1)) (snd $2)) 
}
| 'return' ';' {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.VRet (Just (tokenLineCol $1)))
}
| 'if' '(' Expr ')' Stmt {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.Cond (Just (tokenLineCol $1)) (snd $3)(snd $5)) 
}
| 'if' '(' Expr ')' Stmt 'else' Stmt {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.CondElse (Just (tokenLineCol $1)) (snd $3)(snd $5)(snd $7)) 
}
| 'while' '(' Expr ')' Stmt {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.While (Just (tokenLineCol $1)) (snd $3)(snd $5)) 
}
| Expr ';' {
  (fst $1, Latte.Parsing.AbsLatte.SExp (fst $1)(snd $1)) 
}
| 'for' '(' Type Ident ':' Expr ')' Stmt {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.For (Just (tokenLineCol $1)) (snd $3)(snd $4)(snd $6)(snd $8)) 
}

Item :: {
  (Maybe (Int, Int), Item (Maybe (Int, Int)))
}
: Ident {
  (fst $1, Latte.Parsing.AbsLatte.NoInit (fst $1)(snd $1)) 
}
| Ident '=' Expr {
  (fst $1, Latte.Parsing.AbsLatte.Init (fst $1)(snd $1)(snd $3)) 
}

ListItem :: {
  (Maybe (Int, Int), [Item (Maybe (Int, Int))]) 
}
: Item {
  (fst $1, (:[]) (snd $1)) 
}
| Item ',' ListItem {
  (fst $1, (:) (snd $1)(snd $3)) 
}

BuiltinType :: {
  (Maybe (Int, Int), BuiltinType (Maybe (Int, Int)))
}
: 'int' {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.Int (Just (tokenLineCol $1)))
}
| 'string' {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.Str (Just (tokenLineCol $1)))
}
| 'boolean' {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.Bool (Just (tokenLineCol $1)))
}
| 'void' {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.Void (Just (tokenLineCol $1)))
}

ArrayType :: {
  (Maybe (Int, Int), ArrayType (Maybe (Int, Int)))
}
: BuiltinType '[]' {
  (fst $1, Latte.Parsing.AbsLatte.BuiltinArr (fst $1)(snd $1)) 
}
| Ident '[]' {
  (fst $1, Latte.Parsing.AbsLatte.UserArr (fst $1)(snd $1)) 
}

Type :: {
  (Maybe (Int, Int), Type (Maybe (Int, Int)))
}
: BuiltinType {
  (fst $1, Latte.Parsing.AbsLatte.BltinType (fst $1)(snd $1)) 
}
| ArrayType {
  (fst $1, Latte.Parsing.AbsLatte.ArrType (fst $1)(snd $1)) 
}
| Ident {
  (fst $1, Latte.Parsing.AbsLatte.UserType (fst $1)(snd $1)) 
}

ListType :: {
  (Maybe (Int, Int), [Type (Maybe (Int, Int))]) 
}
: {
  (Nothing, [])
}
| Type {
  (fst $1, (:[]) (snd $1)) 
}
| Type ',' ListType {
  (fst $1, (:) (snd $1)(snd $3)) 
}

Expr4 :: {
  (Maybe (Int, Int), Expr (Maybe (Int, Int)))
}
: 'new' Type '[' Expr ']' {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.ENewArr (Just (tokenLineCol $1)) (snd $2)(snd $4)) 
}
| 'new' Type {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.ENewStruct (Just (tokenLineCol $1)) (snd $2)) 
}
| Expr4 MulOp Expr5 {
  (fst $1, Latte.Parsing.AbsLatte.EMul (fst $1)(snd $1)(snd $2)(snd $3)) 
}
| Expr5 {
  (fst $1, snd $1)
}

Expr6 :: {
  (Maybe (Int, Int), Expr (Maybe (Int, Int)))
}
: Ident {
  (fst $1, Latte.Parsing.AbsLatte.EVar (fst $1)(snd $1)) 
}
| Expr6 '.' Ident {
  (fst $1, Latte.Parsing.AbsLatte.EStructField (fst $1)(snd $1)(snd $3)) 
}
| Expr6 '[' Expr ']' {
  (fst $1, Latte.Parsing.AbsLatte.EArrAt (fst $1)(snd $1)(snd $3)) 
}
| Expr7 {
  (fst $1, snd $1)
}

Expr7 :: {
  (Maybe (Int, Int), Expr (Maybe (Int, Int)))
}
: Integer {
  (fst $1, Latte.Parsing.AbsLatte.ELitInt (fst $1)(snd $1)) 
}
| 'true' {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.ELitTrue (Just (tokenLineCol $1)))
}
| 'false' {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.ELitFalse (Just (tokenLineCol $1)))
}
| String {
  (fst $1, Latte.Parsing.AbsLatte.EString (fst $1)(snd $1)) 
}
| '(' Expr ')' 'null' {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.EStructCoerce (Just (tokenLineCol $1)) (snd $2)) 
}
| '(' ArrayType ')' 'null' {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.EStructArrCoerce (Just (tokenLineCol $1)) (snd $2)) 
}
| Ident '(' ListExpr ')' {
  (fst $1, Latte.Parsing.AbsLatte.EApp (fst $1)(snd $1)(snd $3)) 
}
| '(' Expr ')' {
  (Just (tokenLineCol $1), snd $2)
}

Expr5 :: {
  (Maybe (Int, Int), Expr (Maybe (Int, Int)))
}
: '-' Expr6 {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.Neg (Just (tokenLineCol $1)) (snd $2)) 
}
| '!' Expr6 {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.Not (Just (tokenLineCol $1)) (snd $2)) 
}
| Expr6 {
  (fst $1, snd $1)
}

Expr3 :: {
  (Maybe (Int, Int), Expr (Maybe (Int, Int)))
}
: Expr3 AddOp Expr4 {
  (fst $1, Latte.Parsing.AbsLatte.EAdd (fst $1)(snd $1)(snd $2)(snd $3)) 
}
| Expr4 {
  (fst $1, snd $1)
}

Expr2 :: {
  (Maybe (Int, Int), Expr (Maybe (Int, Int)))
}
: Expr2 RelOp Expr3 {
  (fst $1, Latte.Parsing.AbsLatte.ERel (fst $1)(snd $1)(snd $2)(snd $3)) 
}
| Expr3 {
  (fst $1, snd $1)
}

Expr1 :: {
  (Maybe (Int, Int), Expr (Maybe (Int, Int)))
}
: Expr2 '&&' Expr1 {
  (fst $1, Latte.Parsing.AbsLatte.EAnd (fst $1)(snd $1)(snd $3)) 
}
| Expr2 {
  (fst $1, snd $1)
}

Expr :: {
  (Maybe (Int, Int), Expr (Maybe (Int, Int)))
}
: Expr1 '||' Expr {
  (fst $1, Latte.Parsing.AbsLatte.EOr (fst $1)(snd $1)(snd $3)) 
}
| Expr1 {
  (fst $1, snd $1)
}

ListExpr :: {
  (Maybe (Int, Int), [Expr (Maybe (Int, Int))]) 
}
: {
  (Nothing, [])
}
| Expr {
  (fst $1, (:[]) (snd $1)) 
}
| Expr ',' ListExpr {
  (fst $1, (:) (snd $1)(snd $3)) 
}

AddOp :: {
  (Maybe (Int, Int), AddOp (Maybe (Int, Int)))
}
: '+' {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.Plus (Just (tokenLineCol $1)))
}
| '-' {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.Minus (Just (tokenLineCol $1)))
}

MulOp :: {
  (Maybe (Int, Int), MulOp (Maybe (Int, Int)))
}
: '*' {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.Times (Just (tokenLineCol $1)))
}
| '/' {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.Div (Just (tokenLineCol $1)))
}
| '%' {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.Mod (Just (tokenLineCol $1)))
}

RelOp :: {
  (Maybe (Int, Int), RelOp (Maybe (Int, Int)))
}
: '<' {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.LTH (Just (tokenLineCol $1)))
}
| '<=' {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.LE (Just (tokenLineCol $1)))
}
| '>' {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.GTH (Just (tokenLineCol $1)))
}
| '>=' {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.GE (Just (tokenLineCol $1)))
}
| '==' {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.EQU (Just (tokenLineCol $1)))
}
| '!=' {
  (Just (tokenLineCol $1), Latte.Parsing.AbsLatte.NE (Just (tokenLineCol $1)))
}

{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    t:_ -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens

pProgram = (>>= return . snd) . pProgram_internal
}

