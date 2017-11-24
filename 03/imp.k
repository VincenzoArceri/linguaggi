module IMP-SYNTAX

	syntax Ids ::= List{Id, ","}

	syntax AExp ::= Int 
				|	Id
				|	"++" Id
				|	Id "++"
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
				|	BExp "&&" BExp [strict(1)]
				|	BExp "||" BExp [strict(1)]
				|	"!" BExp	   [strict]

	syntax Block ::= "{" "}"
				  |	 "{" Stmt "}"

	syntax Stmt ::= AExp ";"	[strict]
				|	"int" Ids "=" AExp ";" [strict(2)]	
				|	"int" Ids ";"
				|	"print" "(" AExp ")" ";" [strict]
				|	Block
				|	"if" "(" BExp ")" Block "else" Block [strict(1)]
				|	"while" "(" BExp")" Block 
				| 	"for" "(" Stmt BExp ";" Stmt ")" Block [seqstrict]
				|	Stmt Stmt

endmodule


module IMP 
	imports IMP-SYNTAX

	configuration
	<T>
		<k> $PGM:Stmt </k>
		<env> .Map </env>
		<store> .Map </store>
		<nextLoc> 0:Int </nextLoc>
		<in stream="stdin"> .List </in>
		<out stream="stdout"> .List </out> 
	</T>

	syntax KResult ::= Int | Bool

	rule I1:Int + I2:Int => I1 +Int I2
	rule I1:Int * I2:Int => I1 *Int I2
	rule I1:Int - I2:Int => I1 -Int I2
	rule I1:Int / I2:Int => I1 /Int I2 when I2 =/=Int 0

	rule I1:Int > I2:Int => I1 >Int I2
	rule I1:Int < I2:Int => I1 <Int I2

	rule true || _ => true
	rule false || B:BExp => B

	rule false && _ => false
	rule true && B:BExp => B

	rule ! true => false
	rule ! false => true

	rule _:Int ; => .K

	rule <k>  int (X:Id, Xs:Ids => Xs) = I:Int ;  ... </k>
		 <env> Env:Map => Env[X <- L]</env>
		 <store> Store:Map => Store[L <- I] </store> 
		 <nextLoc> L:Int => L +Int 1 </nextLoc>

	rule int .Ids = I:Int ;  => .K 

	rule <k> ( X:Id  = I:Int => I ) ... </k> 
		 <env> ... X |-> L ... </env>
		 <store> ... L |-> ( _ => I ) ... </store>

	rule <k> ( X:Id => I ) ... </k>
		 <env> ... X |-> L ... </env>
		 <store> ... L |-> I:Int ... </store>

	rule S1:Stmt S2:Stmt => S1 ~> S2 [structural]

	syntax K ::= Map

	rule { } => .K

	rule <k> { S:Stmt } => S ~> Env ...</k>
		 <env> Env:Map </env>

	rule <k> Env:Map => .K ... </k>
		 <env> _ => Env </env>

	rule if (true) B:Block else _ => B
	rule if (false) _ else B:Block => B

	rule <k> read() => I ... </k>
		 <in> ListItem(I:Int) => .List ... </in>

	rule <k> print(I:Int) ; => .K ... </k>
		 <out> ... .List => ListItem(I) </out> 

	rule <k> X:Id ++ => I:Int ... </k>
		 <env> ... X |-> L ... </env>
		 <store> ... L |-> (I:Int => I +Int 1) ... </store>

	rule <k> ++ X:Id => I +Int 1 ... </k>
		 <env> ... X |-> L ... </env>
		 <store> ... L |-> (I:Int => I +Int 1) ... </store>

	rule while (B:BExp) Bl:Block => if (B) { Bl while (B) Bl } else { } [structural]

	rule for (Init:Stmt B:BExp;  Step:Stmt) Bl:Block => Init while(B) { Bl Step } [structural]

	rule int Xs:Ids ; => int Xs = 0 ; [structural]

endmodule