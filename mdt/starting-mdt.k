module MDT-SYNTAX

	/*
	 * TM states 
	 */
	syntax States ::= List{Int, ","}

	/*
	 * TM Final state declaration
	 */
	syntax FinalState ::= "Final" ":" Int
	
	/*
	 * TM Initial state declaration
	 */
	syntax InitialState ::= "Initial" ":" Int

	syntax Movement ::= "L" | "R" | "-"

	/*
	 * TM Transitions declaration
	 */
	syntax Transition ::= "(" Int "," String "," Int "," String "," Movement ")"
						|	Transition Transition [left]

	/*
	 * Initial tape declaration
	 */
	syntax InitialTape ::= List{String, ","}


	syntax Program ::= States FinalState InitialState Transition InitialTape

endmodule


module MDT
	imports MDT-SYNTAX

	/*
	 * transitions-configuration map key
	 */
	syntax K ::= "(" Int "," String ")"
	
	/*
	 * transitions-configuration map value
	 */
	syntax K ::= "(" Int "," String "," Movement ")"

	configuration 
	<T>
		<k> $PGM:Program </k>
		<tape> .Map </tape>
		<current-state> 0 </current-state>
		<current-position> 0 </current-position>
		<transitions> .Map </transitions>
		<states> .Set </states>
		<final-state> 0 </final-state>
		<initial-state> 0 </initial-state>
	</T>

	rule SI:States F:FinalState I:InitialState TT:Transition Tape:InitialTape 
		=> SI ~> F ~> I ~> TT ~> Tape [structural]

	rule T1:Transition T2:Transition => T1 ~> T2 [structural]

	rule <k> ( Q, Qs:States => Qs ) ... </k>
		 <states> S:Set => S SetItem(Q) </states> 

	rule <k> .States => .K ... </k>


	/*
	 * Equals with tape chars
	 */
	syntax Bool ::= K "==TapeChar" K [function]
	rule "*" ==TapeChar S:String => true
	rule S:String ==TapeChar "*" => true
	rule S1:String ==TapeChar S2:String => S1 ==String S2 when S1 =/=String "*" andBool S2 =/=String "*"
endmodule
