import java.util.Arrays;
import java.util.LinkedList;

/**
 * The binary program handles informations and solve the loaded
 * program.
 *
 * @author Andre Greiner-Petter
 *
 */
public class BinaryProgram {
	// variables about program
	private ConstraintType[] types;
	private double[][] 		 vars;
	private double[] 		 solutionVec;
	
	// number of variables
	private int variables;
	
	// list of solutions (long value represents a binary solution vector)
	private LinkedList<Long> sols;
	
	/**
	 * Creates a binary program object.
	 * 		A * x = b
	 * @param constraints matrix of constraints (A)
	 * @param solutionVec solution vector (b)
	 * @param types could contains <,>,<=,>= and =
	 */
	public BinaryProgram( double[][] constraints, double[] solutionVec, ConstraintType[] types ){
		this.vars 			= constraints;
		this.solutionVec 	= solutionVec;
		this.types 			= types;
		this.sols			= new LinkedList<Long>();
		this.variables		= vars[0].length;
	}
	
	/**
	 * Solve this program. And prints a solution directly to console.
	 */
	public void solve(){
		// print first line (variable names)
		for ( int i = 0; i < variables; i++ )
			System.out.print("x_"+i+" ");
		System.out.print( System.lineSeparator() );
		
		// iterate through each solution
		jump: // marker to break if a solution is not feasible
		for ( long sol = 0; sol < (1<<variables); sol++ ){
			// test solution for each constraint
			for ( int i = 0; i < vars.length; i++ ){
				// if one constraint not holds, overjump the rest
				if ( !isSolution( vars[i], solutionVec[i], types[i], sol ) ){
					continue jump;
				}
			}
			// otherwise add this solution to other and prints it to console
			this.sols.add(sol);
			System.out.println( createBitStringOfLong(sol) );
		} // end loop of solutions
	} // end solve
	
	/**
	 * Returns true if the given solution holds given constraint.
	 * @param vars variables coefficients
	 * @param b solution site
	 * @param conType constraint type
	 * @param solution tested variable
	 * @return true if solution holds this constraint, false otherwise
	 */
	private boolean isSolution( double[] vars, double b, ConstraintType conType, long solution ){
		// sum of constraint
		double sum = 0;
		// for each coefficient
		for ( int i = 0; i < vars.length; i++ ){
			// shift 1xb to left site and use bitwise AND to find '1' variables in solution
			if ( ((1<<i) & solution) > 0 ){
				sum += vars[i];
			}
		}
		// return whether sum and b corresponds by constraint type
		return conType.correct(sum, b);
	}
	
	/**
	 * Creates a string representation of the bits of given long value.
	 * @param in long value
	 * @return string contains 0 and 1
	 */
	public String createBitStringOfLong( long in ){
		String str = "";
		
		for ( int i = 0; i < variables; i++ ){
			// if (1xb shifted by i)-Bit is set in 'in', print " 1 ", otherwise " 0 ". 
			str += ((1<<i) & in) != 0 ? " 1  " : " 0  ";
		}
		
		return str;
	}
	
	@Override
	public String toString(){
		// use Arrays.toString to print variable coefficients
		String str = "";
		for ( int i = 0; i < vars.length; i++ ){
			str += Arrays.toString(vars[i]) + " " + types[i] + " " + solutionVec[i] + System.lineSeparator();
		}
		
		return str;
	}
}
