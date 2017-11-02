module EXP-SYNTAX
	syntax Exp ::=  Int 
				|	Bool
				>	"+" Exp 	[strict]
				|	"-" Exp 	[strict]
				>	Exp "*" Exp [seqstrict]
				|	Exp "/" Exp [strict]
				>	Exp "+" Exp [seqstrict]
				|	Exp "-" Exp [seqstrict]
				|	Exp ">" Exp [strict]
				|	Exp "<" Exp  [strict]
				|	Exp ">=" Exp [strict]
				|	Exp "<=" Exp [strict]
				|	Exp "&&" Exp [strict(1)]
				|	Exp "||" Exp [strict(1)]
				|	"if" "(" Exp ")" Exp "else" Exp [strict(1)]
				|	Exp "?" Exp ":" Exp 
				|	"(" Exp ")"	[bracket]

endmodule

module EXP
	imports EXP-SYNTAX 

	syntax KResult ::= Int | Bool

	rule I1:Int + I2:Int => I1 +Int I2
	rule I1:Int * I2:Int => I1 *Int I2
	rule I1:Int / I2:Int => I1 /Int I2 when I2 =/=Int 0
	rule I1:Int - I2:Int => I1 -Int I2

	rule + I:Int => I
	rule - I:Int => 0 -Int I

	rule I1:Int > I2:Int => I1 >Int I2
	rule I1:Int < I2:Int => I1 <Int I2
	rule I1:Int >= I2:Int => I1 >=Int I2
	rule I1:Int <= I2:Int => I1 <=Int I2

	rule true && E1:Exp => E1
	rule false && _ => false

	rule true || _ => true
	rule false || E:Exp => E

	rule if (true) E:Exp else _ => E
	rule if (false) _ else E:Exp => E

	rule E1:Exp ? E2:Exp : E3:Exp => if (E1) E2 else E3 [structural]
endmodule


