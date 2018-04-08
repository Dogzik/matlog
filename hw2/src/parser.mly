%{
open Grammar;;
%}
%token <string> VAR
%token IMPL AND OR NOT
%token OPEN CLOSE
%token DEQ COMMA
%token EOF
%right IMPL
%left DEQ
%left COMMA
%left OR
%left AND
%nonassoc NOT
%start main
%start assump
%type <Grammar.expression> main
%type <(Grammar.expression list) * Grammar.expression> assump
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

assump:
		exps DEQ exp EOF { ($1, $3) }
		|DEQ exp EOF	 { ([], $2) }
exps:
		exp				 { [$1] }
		|exp COMMA exps	 { $1::$3 }
        
