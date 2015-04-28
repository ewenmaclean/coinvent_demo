



%token SPEC
%token THEN
%token CHORD
%token OP
%token HASABSNOTE
%token ROOT
%token HASRELNOTE
%token END
%token PRIORITY
%token OPEN
%token CLOSE
%token EOF
%token COLON
%token PERCENT
%token <string> IDENT
%token EQUALS
%token <int> INT
%token COMMA
%token COMMENT
%token EOL
%token DOT
%token NOT
%token SUC
%token SORT
%token SORTS
%token LOGIC
%token OPS
%token PRED
%token PREDS
%token GENERATED
%token FREE
%start chord1
%type <string> chord1


%%

chord:
| SPEC IDENT EQUALS chord {$2}
| OP IDENT COLON CHORD chord {""}
| CHORD chord {""} 
| THEN chord {""}
| DOT HASRELNOTE OPEN IDENT COMMA INT CLOSE chord {""}
| DOT HASABSNOTE OPEN IDENT COMMA INT CLOSE chord {""}
| DOT ROOT OPEN IDENT CLOSE EQUALS INT chord {""}
| DOT NOT INT EQUALS INT chord {""}
| DOT SUC OPEN INT CLOSE EQUALS INT chord {""}
| PERCENT OPEN IDENT CLOSE PERCENT chord {""}
| END {""}
| IDENT chord {""}
| EOF {""};

chord1:
| chord { $1 };
