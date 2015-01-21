/**
 * This enumeration contains all constraint types.
 *
 * @author Andre Greiner-Petter
 *
 */
public enum ConstraintType {
	GEQ	(">="),	// greater or equal 
	LEQ	("<="), // less or equal
	EQ	("=" ), // equal
	LESS("<" ), // less
	GREATER(">");// greater
	
	// string representation of constraint type
	public final String type;
	
	/**
	 * Private constructor to define string representation
	 * @param type of constraint as string
	 */
	private ConstraintType(String type){
		this.type = type;
	}
	
	/**
	 * Test whether this constraint type corresponds with given values.
	 * For instance:
	 * 			GEQ.correct(0,1) -> true,  because 0 <= 1
	 * 			LEQ.correct(0,1) -> false, because 0 <= 1
	 * 
	 * @param consts constraint value
	 * @param sol solution value
	 * @return true if this corresponds with consts and sol.
	 */
	public boolean correct( double consts, double sol ){
		if ( this == GEQ ) return consts >= sol;
		if ( this == LEQ ) return consts <= sol;
		if ( this == EQ  ) return consts == sol;
		if ( this == LESS) return consts <  sol;
		if ( this == GREATER ) return consts > sol;
		return false;
	}
	
	/**
	 * Returns constraint type by given string
	 * @param type
	 * @return ConstraintType
	 */
	public static ConstraintType getType(final String type){
		switch(type){
		case ">=": return GEQ;
		case "<=": return LEQ;
		case "=" : return EQ;
		case ">" : return GREATER;
		case "<" : return LESS;
		default: return null;
		}
	}
	
	@Override
	public String toString(){
		return type;
	}
}
