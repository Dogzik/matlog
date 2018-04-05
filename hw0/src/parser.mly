%{
open Grammar;;
%}
%token <string> VAR
%token IMPL AND OR NOT
%token OPEN CLOSE
%token EOF
%right IMPL
%left OR
%left AND
%nonassoc NOT
%start main
%type <Grammar.expression> main
%%
main:
        exp EOF          { $1 }
exp:
        VAR              { Var ($1) }            
	|OPEN exp CLOSE  { $2 }     
        |NOT exp         { Not ($2) }  
        |exp IMPL exp    { Bin (Impl, $1, $3) }
        |exp AND exp     { Bin (And, $1, $3) }
        |exp OR exp      { Bin (Or, $1, $3) }
        
