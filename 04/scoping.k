module IMP-SYNTAX

  syntax Exp ::= Int | Bool | Id
               | "(" Exp ")"             [bracket]
               | "read" "(" ")"
               > Exp "*" Exp             [strict]
               | Exp "/" Exp             [strict]
               > Exp "+" Exp             [strict]
               | Exp "-" Exp             [strict]
               > Exp "<" Exp             [strict]
               | Exp "<=" Exp            [strict]
               | Exp ">" Exp             [strict]
               | Exp ">=" Exp            [strict]
               | Exp "==" Exp            [strict]
               | Exp "!=" Exp            [strict]
               > "!" Exp                 [strict]
               > Exp "&&" Exp            [strict(1)]
               | Exp "||" Exp            [strict(1)]
               > Id "=" Exp              [strict(2)]

  	syntax Ids  ::= List{Id,","}

	syntax Block ::= "{" "}"
				|	"{" Stmt "}"

	syntax Stmt ::= "int" Id "=" Exp ";" [strict(2)]
				|	Id "=" Exp ";"		  [strict(2)]
				|	Block
				|	"print" "(" Exp ")" ";" [strict(1)]
				|	"if" "(" Exp ")" Block "else" Block [strict(1)]
				|	"while" "(" Exp ")" Block
				|	Stmt Stmt 
endmodule


module IMP 
  	imports IMP-SYNTAX

  	syntax Val ::= Int | Bool 

  	syntax Exp ::= Val
  	syntax KResult ::= Val  

	configuration
	<T>
		<k> $PGM:Stmt </k>
		<env> .Map </env>
		<store> .Map </store>
		<in stream="stdin"> .List </in>
		<out stream="stdout"> .List </out>
		<nextLoc> 0 </nextLoc>
	</T>

   	rule I1:Int + I2:Int => I1 +Int I2
	rule I1:Int * I2:Int => I1 *Int I2
	rule I1:Int - I2:Int => I1 -Int I2
	rule I1:Int / I2:Int => I1 /Int I2 when I2 =/=Int 0

	rule true || _ => true
	rule false || E => E

	rule false && _ => false
	rule true && E => E

	rule ! true => false
	rule ! false => true

	rule I1:Int > I2:Int => I1 >Int I2
	rule I1:Int < I2:Int => I1 <Int I2
	rule I1:Int == I2:Int => I1 ==Int I2

	rule <k> ( int X:Id = I:Int ; => .K ) ... </k>
		 <env> Env:Map => Env[X <- L] </env>
		 <store> Store:Map => Store[L <- I] </store>
		 <nextLoc> L => L +Int 1 </nextLoc>

    rule S1:Stmt S2:Stmt => S1 ~> S2 [structural]

    rule <k> ( X:Id = I:Int ; => .K ) ... </k>
    	 <env> ... X |-> L ... </env>
		 <store> ... L|-> ( _ => I) ... </store>

	rule <k> ( X:Id => V ) ... </k>
		 <env> ... X |-> L ... </env>
		 <store> ... L |-> V ... </store>

	syntax K ::= Map

	rule { } => .K
	rule <k> { S:Stmt } => S ~> Env ... </k>
		 <env> Env:Map </env>

	rule <k> Env:Map => .K ... </k>
		 <env> _ => Env </env>

	rule if (true) Bl:Block else _ => Bl
	rule if (false) _ else Bl:Block => Bl

	rule <k> ( read() => I ) ... </k>
		 <in> (ListItem(I:Int) => .List) ... </in>

	rule <k> ( print(I:Int) ; => .K ) ... </k>
		 <out> ... ( .List => ListItem(I) ) </out>

	rule while (B) Bl:Block => if (B) { Bl while (B) Bl } else { } [structural] 

endmodule