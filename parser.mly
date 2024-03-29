/* Ocamlyacc parser for MicroC */

%{
open Ast

let fst (a,_,_) = a;;
let snd (_,b,_) = b;;
let trd (_,_,c) = c;;

%}

%token PERIOD LPAREN RPAREN LBRACE RBRACE PLUS MINUS TIMES DIV ASSIGN
%token MAKEA MAKE USING NAMED HAS BE DOES DO CALLED FUNC NUM OUTPUT STRING 
%token EQ NEQ LT AND OR LTE GTE GT 
%token IF ELSE WHILE INT BOOL FLOAT
%token CHUNK COLON
/* return, COMMA token */
%token RETURN COMMA
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID
%token <string> SLIT
%token <string> FLIT
%token EOF

%start program
%type <Ast.program> program

/* Why have this in right? */
%right MAKEA /*friendly*/
%right MAKE  /*friendly*/

%left OR
%left AND
%left EQ NEQ 
%left LT GT LTE GTE
%left PLUS MINUS
%left TIMES DIV
%left COLON
%%

/* add function declarations*/
program:
  decls EOF { $1}

decls:
        /* nothing */ { ([], [], [])               }
 | vdecl PERIOD decls { (($1 :: fst $3), snd $3, trd $3) }
 | fdecl decls { (fst $2, ($1 :: snd $2), trd $2) }
 | cdecl decls { (fst $2, snd $2, ($1 :: trd $2)) }

vdecl_list:
  /*nothing*/ { [] }
  | vdecl PERIOD vdecl_list  {  $1 :: $3 }

/* friendly vdecl covered by */
/* make_a number called x. */


vdecl:
  MAKEA typbind { $2 }

typbind:
  typ CALLED ID { ($1, $3)}

/* need another rule for assign */
/* make x be <expr> */




typ:
    NUM   { Int   } /*changed INT to NUM for friendly*/
  | INT   { Int }
  | FLOAT { Float }
  | BOOL  { Bool  }
  | STRING { String }
  | CHUNK ID { Chunk($2) }


/* fdecl  ADDED RETURN TYPE*/
fdecl:
  MAKEA FUNC CALLED ID OUTPUT typ USING LPAREN formals_opt RPAREN DOES LBRACE vdecl_list stmt_list RBRACE
  /*vdecl LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE */
  {
    {
      rtyp=$6;
      fname= $4;
      formals=$9;
      locals=$13;
      body=$14
    }
  }


cdecl:
  MAKEA CHUNK CALLED ID LBRACE vdecl_list RBRACE 
  {
    {
      cname = $4;
      cfields = $6;

    }
  }

/* formals_opt */
formals_opt:
  /*nothing*/ { [] }
  | formals_list { $1 }

formals_list:
  typbind { [$1] }
  | typbind COMMA formals_list { $1::$3 }

stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list  { $1::$2 }

stmt:
    expr PERIOD                               { Expr $1      }
  | LBRACE stmt_list RBRACE                 { Block $2 }
  /* if (condition) { block1} else {block2} */
  /* if (condition) stmt else stmt */
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
  /* return */
  | RETURN expr PERIOD                      { Return $2      }

expr:
    LITERAL          { Literal($1)            }
  | BLIT             { BoolLit($1)            }
  | ID               { Id($1)                 }
  | FLIT             { FLiteral($1)           }
  | SLIT             { StringWord($1)         }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Mul,   $3)   }
  | expr DIV    expr { Binop($1, Div,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq, $3)     }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | expr LTE    expr { Binop($1, LessEqual,    $3)   }
  | expr GT     expr { Binop($1, Greater,    $3)   }
  | expr GTE    expr { Binop($1, GreaterEqual,    $3)   }
  /*| ID ASSIGN expr   { Assign($1, $3)         }*/
  | MAKE expr BE  expr { Assign($2, $4)         }
  | LPAREN expr RPAREN { $2                   }
  /* call */
  | DO ID LPAREN args_opt RPAREN { Call ($2, $4)  }
  /* access */
  | expr COLON ID    { Colon ($1, $3)         }

/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }


