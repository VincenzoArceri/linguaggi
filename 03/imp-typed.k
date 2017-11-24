module IMP-TYPED-SYNTAX

	syntax Ids ::= List{Id, ","}

	syntax AExp ::= Int 
				|	Id
				|	Id "++"
				|	"++" Id
				|	"read" "(" ")"
				|	AExp "*" AExp [seqstrict]
				|	AExp "/" AExp [seqstrict]
				>	AExp "+" AExp [seqstrict]
				|	AExp "-" AExp [seqstrict]
				>	Id "=" AExp  [strict(2)]
				|	"(" AExp ")"  [bracket]

	syntax BExp ::= Bool
				|	AExp ">" AExp  [seqstrict]
				|	AExp "<" AExp  [seqstrict]
				|	BExp "&&" BExp [seqstrict]
				|	BExp "||" BExp [seqstrict]
				|	"!" BExp	   [strict]

	syntax Block ::= "{" "}"
				  |	 "{" Stmt "}"

	syntax Stmt ::= AExp ";" 								[strict]
				|	"int" Ids "=" AExp ";" 					[strict(2)]
				|	"int" Ids ";"
				|	"print" "(" AExp ")" ";" 				[strict]
				|	Block
				|	"if" "(" BExp ")" Block "else" Block 	[strict]
				|	"while" "(" BExp")" Block 				[seqstrict]
				| 	"for" "(" Stmt BExp ";" Stmt ")" Block 	[seqstrict]
				|	Stmt Stmt

endmodule


module IMP-TYPED 
	imports IMP-TYPED-SYNTAX

	configuration
	<T>
		<k> $PGM:Stmt </k>
		<env> .Map </env>
		<store> .Map </store>
		<nextLoc> 0:Int </nextLoc>
		<in stream="stdin"> .List </in>
		<out stream="stdout"> .List </out> 
	</T>

	syntax IntType ::= "int"
	syntax BoolType ::= "bool"
	syntax UnitType ::= "unit"

	syntax Type ::= IntType | BoolType | UnitType

	syntax AExp ::= IntType
	syntax BExp ::= BoolType
	syntax Block ::= UnitType
	syntax Stmt ::= UnitType

	syntax KResult ::= Type


	rule _:Int => int
	rule _:Bool => bool

	rule int ; => unit

	rule int + int => int
	rule int * int => int
	rule int - int => int
	rule int / int => int

	rule int > int => bool
	rule int < int => bool

	rule bool || bool => bool
	rule bool && bool => bool

	rule ! bool => bool

	rule <k> int ( X:Id , Xs:Ids => Xs ) = int ; ... </k>
		 <env> Env:Map => Env[X <- L]</env>
		 <store> Store:Map => Store[L <- int] </store> 
		 <nextLoc> L:Int => L +Int 1 </nextLoc>

	rule int .Ids = int ; => unit

	rule <k> ( X:Id  = int  => int)    ... </k> 
		 <env> ... X |-> L ... </env>
		 <store> ... L |-> int ... </store>

	rule <k> ( X:Id => int ) ... </k>
		 <env> ... X |-> L ... </env>
		 <store> ... L |-> int ... </store>

	rule S1:Stmt S2:Stmt => S1 ~> S2 [structural]
	rule unit ~> S2:Stmt => S2 

	rule { } => unit

	rule <k> { S:Stmt } => S ~> Env ...</k>
		 <env> Env:Map </env>

	rule <k> unit ~> Env:Map => unit ... </k>
		 <env> _ => Env </env>

	rule <k> X:Id ++ => int ... </k>
		 <env> ... X |-> L ... </env>
		 <store> ... L |-> int ... </store>

	rule <k> ++ X:Id => int ... </k>
		 <env> ... X |-> L ... </env>
		 <store> ... L |-> int ... </store>

	rule if (bool) unit else unit => unit
	rule if (bool) unit else unit => unit

	rule <k> read() => int ... </k>
		 <in> ListItem(I:Int) => .List ... </in>

	rule <k> print(int) ; => unit ... </k>
		 <out> ... .List => ListItem(int) </out> 

	rule while (bool) unit => unit

	rule for (unit bool ; unit ) unit => unit

	rule int Xs:Ids ; => int Xs = 0 ; [structural]

endmodule