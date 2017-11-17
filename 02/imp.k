module IMP-SYNTAX

	syntax AExp ::= Int 
				|	Id
				|	Id "++"
				|	"++" Id
				|	"read" "(" ")"
				|	AExp "*" AExp [seqstrict]
				|	AExp "/" AExp [seqstrict]
				>	AExp "+" AExp [seqstrict]
				|	AExp "-" AExp [seqstrict]
				|	"(" AExp ")"  [bracket]

	syntax BExp ::= Bool
				|	BExp "&&" BExp [strict(1)]
				|	BExp "||" BExp [strict(1)]
				|	"!" BExp	   [strict]
				|	AExp ">" AExp  [seqstrict]
				|	AExp "<" AExp  [seqstrict]
				|	AExp "==" AExp [seqstrict]
				|	"(" BExp ")"   [bracket]

	syntax Block ::= "{" "}"
				|	"{" Stmt "}"

	syntax Stmt ::= "int" Id "=" AExp ";" [strict(2)]
				|	Id "=" AExp ";"		  [strict(2)]
				|	Block
				|	"print" "(" AExp ")" ";" [strict(1)]
				|	"if" "(" BExp ")" Block "else" Block [strict(1)]
				|	"while" "(" BExp ")" Block
				|	Stmt Stmt 
endmodule


module IMP 
	imports IMP-SYNTAX

	syntax KResult ::= Int | Bool

	configuration
	<T>
		<k> $PGM:Stmt </k>
		<state> .Map </state>
		<in stream="stdin"> .List </in>
		<out stream="stdout"> .List </out>
	</T>

	rule I1:Int + I2:Int => I1 +Int I2
	rule I1:Int * I2:Int => I1 *Int I2
	rule I1:Int - I2:Int => I1 -Int I2
	rule I1:Int / I2:Int => I1 /Int I2 when I2 =/=Int 0

	rule true || _ => true
	rule false || E:BExp => E

	rule false && _ => false
	rule true && E:BExp => E

	rule ! true => false
	rule ! false => true

	rule I1:Int > I2:Int => I1 >Int I2
	rule I1:Int < I2:Int => I1 <Int I2
	rule I1:Int == I2:Int => I1 ==Int I2

	rule <k> ( int X:Id = I:Int ; => .K ) ... </k>
		 <state> State:Map => State[X <- I] </state>
		 when notBool(X in keys(State))

    rule S1:Stmt S2:Stmt => S1 ~> S2 [structural]

    rule <k> ( X:Id = I:Int ; => .K ) ... </k>
		 <state> State:Map => State[X <- I] </state>
		 when X in keys(State)

	rule <k> ( X:Id => I ) ... </k>
		 <state> ... X |-> I:Int ... </state>

	rule { } => .K
	rule { S:Stmt } => S 

	rule if (true) Bl:Block else _ => Bl
	rule if (false) _ else Bl:Block => Bl

	rule <k> ( read() => I ) ... </k>
		 <in> (ListItem(I:Int) => .List) ... </in>

	rule <k> ( print(I:Int) ; => .K ) ... </k>
		 <out> ... ( .List => ListItem(I) ) </out>

	rule while (B:BExp) Bl:Block => if (B) { Bl while (B) Bl } else { } [structural] 

	rule <k> ( X:Id ++ => I ) ... </k>
		 <state> ... X |-> (I:Int => I +Int 1) ... </state>

	rule <k> ( ++ X:Id => I +Int 1 ) ... </k>
		 <state> ... X |-> ( I:Int => I +Int 1) ...</state>

endmodule