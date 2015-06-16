/*  
 *  Yacc grammar for the parser.  The files parser.mli and parser.ml
 *  are generated automatically from parser.mly.
 */

%{
open Support.Error
open Support.Pervasive
open Syntax
%}

/* ---------------------------------------------------------------------- */
/* Preliminaries */

/* We first list all the tokens mentioned in the parsing rules
   below.  The names of the tokens are common to the parser and the
   generated lexical analyzer.  Each token is annotated with the type
   of data that it carries; normally, this is just file information
   (which is used by the parser to annotate the abstract syntax trees
   that it constructs), but sometimes -- in the case of identifiers and
   constant values -- more information is provided.
 */

/* Keyword tokens */
%token <Support.Error.info> TYPE
%token <Support.Error.info> INERT
%token <Support.Error.info> LAMBDA
%token <Support.Error.info> TTOP
%token <Support.Error.info> IF
%token <Support.Error.info> THEN
%token <Support.Error.info> ELSE
%token <Support.Error.info> TRUE
%token <Support.Error.info> FALSE
%token <Support.Error.info> BOOL
%token <Support.Error.info> TBOT
%token <Support.Error.info> LET
%token <Support.Error.info> IN
%token <Support.Error.info> FIX
%token <Support.Error.info> LETREC
%token <Support.Error.info> CASE
%token <Support.Error.info> OF
%token <Support.Error.info> AS
%token <Support.Error.info> USTRING
%token <Support.Error.info> UNIT
%token <Support.Error.info> REF
%token <Support.Error.info> UUNIT
%token <Support.Error.info> RREF
%token <Support.Error.info> TIMESFLOAT
%token <Support.Error.info> SUCC
%token <Support.Error.info> PRED
%token <Support.Error.info> ISZERO
%token <Support.Error.info> UFLOAT
%token <Support.Error.info> SSOURCE
%token <Support.Error.info> SSINK
%token <Support.Error.info> NAT
%token <Support.Error.info> WHILE

/* Identifier and constant value tokens */
%token <string Support.Error.withinfo> UCID  /* uppercase-initial */
%token <string Support.Error.withinfo> LCID  /* lowercase/symbolic-initial */
%token <int Support.Error.withinfo> INTV
%token <float Support.Error.withinfo> FLOATV
%token <string Support.Error.withinfo> STRINGV

/* Symbolic tokens */
%token <Support.Error.info> APOSTROPHE
%token <Support.Error.info> DQUOTE
%token <Support.Error.info> ARROW
%token <Support.Error.info> BANG
%token <Support.Error.info> BARGT
%token <Support.Error.info> BARRCURLY
%token <Support.Error.info> BARRSQUARE
%token <Support.Error.info> COLON
%token <Support.Error.info> COLONCOLON
%token <Support.Error.info> COLONEQ
%token <Support.Error.info> COLONHASH
%token <Support.Error.info> COMMA
%token <Support.Error.info> DARROW
%token <Support.Error.info> DDARROW
%token <Support.Error.info> DOT
%token <Support.Error.info> EOF
%token <Support.Error.info> EQ
%token <Support.Error.info> EQEQ
%token <Support.Error.info> EXISTS
%token <Support.Error.info> GT
%token <Support.Error.info> HASH
%token <Support.Error.info> LCURLY
%token <Support.Error.info> LCURLYBAR
%token <Support.Error.info> LEFTARROW
%token <Support.Error.info> LPAREN
%token <Support.Error.info> LSQUARE
%token <Support.Error.info> LSQUAREBAR
%token <Support.Error.info> LT
%token <Support.Error.info> RCURLY
%token <Support.Error.info> RPAREN
%token <Support.Error.info> RSQUARE
%token <Support.Error.info> SEMI
%token <Support.Error.info> SLASH
%token <Support.Error.info> STAR
%token <Support.Error.info> TRIANGLE
%token <Support.Error.info> USCORE
%token <Support.Error.info> VBAR

/* ---------------------------------------------------------------------- */
/* The starting production of the generated parser is the syntactic class
   toplevel.  The type that is returned when a toplevel is recognized is
     Syntax.context -> (Syntax.command list * Syntax.context) 
   that is, the parser returns to the user program a function that,
   when given a naming context, returns a fully parsed list of
   Syntax.commands and the new naming context that results when
   all the names bound in these commands are defined.

   All of the syntactic productions in the parser follow the same pattern:
   they take a context as argument and return a fully parsed abstract
   syntax tree (and, if they involve any constructs that bind variables
   in some following phrase, a new context).
   
*/

%start toplevel
%type < Syntax.context -> (Syntax.command list * Syntax.context) > toplevel
%%

/* ---------------------------------------------------------------------- */
/* Main body of the parser definition */

/* The top level of a file is a sequence of commands, each terminated
   by a semicolon. */
toplevel :
    EOF
      { fun ctx -> [],ctx }
  | Command toplevel
      { fun ctx ->
          let cmd,ctx = $1 ctx in
          let cmds,ctx = $2 ctx in
          cmd::cmds,ctx }

/* A top-level command */
Command :
  | LCID EQ Expr SEMI
      { fun ctx -> (Assign($1.i, $1.v, $3 ctx), ctx) }
  | WHILE LPAREN Expr RPAREN LCURLY Commands RCURLY
      { fun ctx -> (While(dummyinfo, $3 ctx, $6 ctx), ctx) }
  | IF LPAREN Expr RPAREN LCURLY Commands RCURLY ELSE LCURLY Commands RCURLY
      { fun ctx ->
            let cmd = $6 ctx in 
            let cmd2 = $10 ctx in
            (If(dummyinfo, $3 ctx, cmd, cmd2), ctx) }
  | LCID COLON VarBind SEMI
      { fun ctx -> (Bind($1.i, $1.v, VarBind($3 ctx)),ctx) }
  | LCID COLON OpBinds SEMI
      { fun ctx -> (Bind($1.i, $1.v, ArrBind($3 ctx)),ctx) }

VarBind : INTV { fun ctx -> $1.v }

OpBinds :
  | ABind { fun ctx -> ($1 ctx)::[] }
  | ABind COMMA OpBinds { fun ctx -> ($1 ctx)::($3 ctx) }
  
ABind :
  | Tier { fun ctx -> ($1 ctx)::[] }
  | Tier ARROW ABind { fun ctx -> ($1 ctx)::($3 ctx) }
  
Tier : LPAREN INTV COMMA INTV RPAREN { fun ctx -> ($2.v,$4.v) }

Commands :
  | Command { fun ctx -> let cmd,ctx = $1 ctx in CmdList(dummyinfo,cmd::[]) }
  | Command Commands
      { fun ctx -> let cmd,ctx = $1 ctx in
                   let cmd2 = $2 ctx in
                   match cmd2 with
                         CmdList(_,cmds) -> CmdList(dummyinfo,cmd::cmds)
                       | _ -> CmdList(dummyinfo,[]) }

Expr :
  | LCID
      { fun ctx -> VarExpr($1.v) }
  | INTV
      { fun ctx -> NatExpr($1.v) }
  | LCID LPAREN ExprList RPAREN
      { fun ctx -> OpExpr($1.v, $3 ctx) }

ExprList :
  | Expr
      { fun ctx -> ($1 ctx)::[] }
  | Expr COMMA ExprList
      { fun ctx -> ($1 ctx)::($3 ctx) }
      
