module IMP-SYNTAX

  syntax Exp ::= Int | Bool | Id
               | "(" Exp ")"             [bracket]
               > Exp "(" Exps ")"         [strict]
               | "read" "(" ")"
               > Exp "*" Exp             				[strict]
               | Exp "/" Exp             				[strict]
               > Exp "+" Exp             				[strict]
               | Exp "-" Exp             				[strict]
               > Exp "<" Exp             				[strict]
               | Exp "<=" Exp            				[strict]
               | Exp ">" Exp             				[strict]
               | Exp ">=" Exp            				[strict]
               | Exp "==" Exp            				[strict]
               | Exp "!=" Exp            				[strict]
               > "!" Exp                 				[strict]
               > Exp "&&" Exp            				[strict(1)]
               | Exp "||" Exp            				[strict(1)]
               > Id "=" Exp              				[strict(2)]

  	syntax Ids  ::= List{Id,","}
  	syntax Exps ::= List{Exp,","}          				[strict]  

	syntax Block ::= "{" "}"
				|	"{" Stmt "}"

	syntax Stmt ::= Exp ";"								[strict] 
				|	"int" Id "=" Exp ";" 				[strict(2)]
				|	Exp ";"								[strict]
				|	Block
				|	"print" "(" Exp ")" ";" 			[strict(1)]
				|	"if" "(" Exp ")" Block "else" Block [strict(1)]
				|	"while" "(" Exp ")" Block
				|	"function" Id "(" Ids ")" Block
				|	"return" Exp ";" 					[strict]
				|	Stmt Stmt 
endmodule


module IMP 
	imports IMP-SYNTAX

  	syntax Val ::= Int | Bool | "lambda" "(" Ids "," Stmt "," Map ")"

  	syntax Vals ::= List{Val,","}
  	syntax Exp ::= Val
  	syntax KResult ::= Val | Vals

	configuration
	<T>
		<k> $PGM:Stmt </k>
		<env> .Map </env>
		<store> .Map </store>
		<in stream="stdin"> .List </in>
		<out stream="stdout"> .List </out>
		<fstack> .List </fstack>
		<nextLoc> 0 </nextLoc>
	</T>

	rule _:Int ; => .K 

    rule <k> function F(Xs) S => .K ...</k>
       <env> Env => Env[F <- L] </env>
       <store> Store => Store[L <- lambda(Xs, S, Env[F <- L])] </store>
       <nextLoc> L => L +Int 1 </nextLoc>

  	syntax KItem ::= (Map,K)

  	rule <k> lambda(Xs,S, StaticEnv:Map)(Vs) ~> K => mkDecls(Xs,Vs) S </k>
  		 <env> Env:Map => StaticEnv </env>
         <fstack> .List => ListItem((Env, K)) ... </fstack>

	rule <k> return I:Int ; ~> _ => I ~> K </k>
       <fstack> ListItem((Env, K)) => .List ...</fstack>
       <env> _ => Env </env>
       
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

    rule <k> ( X:Id = I:Int => I ) ... </k>
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

  syntax Stmt ::= "mkDecls" "(" Ids "," Vals ")"  [function]
  rule mkDecls((X:Id, Xs:Ids), (V:Int, Vs:Vals)) => int X = V; mkDecls(Xs,Vs)
  rule mkDecls(.Ids,.Vals) => {}

endmodule