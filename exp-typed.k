// Exp syntax
module EXP-TYPED-SYNTAX
	syntax Exp ::=  Int 
				|	Bool
				>	Exp "*" Exp [seqstrict]
				|	Exp "/" Exp [strict]
				>	Exp "+" Exp [seqstrict]
				|	Exp "-" Exp [seqstrict]
				|	Exp ">" Exp [strict]
				|	Exp "<" Exp  [strict]
				|	Exp ">=" Exp [strict]
				|	Exp "<=" Exp [strict]
				|	Exp "&&" Exp [strict]
				|	Exp "||" Exp [strict]
				|	"if" "(" Exp ")" Exp "else" Exp [strict]
				|	Exp "?" Exp ":" Exp 
				|	"(" Exp ")"	[bracket]

endmodule

// Exp semantics
module EXP-TYPED
	imports EXP-TYPED-SYNTAX 

	// Final value
	syntax Type ::= "int" | "bool"
	syntax Exp ::= Type
	syntax KResult ::= Type

	rule I:Int => int
	rule B:Bool => bool

	rule int + int => int
	rule int * int => int
	rule int / int => int
	rule int - int => int

	rule int > int => bool
	rule int < int => bool
	rule int >= int => bool
	rule int <= int => bool

	rule bool && bool => bool
	rule bool || bool => bool

	rule if (bool) int else int => int
	rule if (bool) bool else bool => bool

	rule E1:Exp ? E2:Exp : E3:Exp => if (E1) E2 else E3 [structural]
endmodule


