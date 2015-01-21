import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * Simple solution to solve binary programs and list the solutions on console.
 *
 * @author Andre Greiner-Petter
 *
 */
public class BinarySolver {
	/**
	 * Returns a binary program structure from given file.
	 * @param path to file
	 * @return binary program
	 */
	public static BinaryProgram readFile( Path path ){
		// local variables
		String line;
		int variables, constraints;
		
		// try to read line by line
		try ( BufferedReader reader = new BufferedReader( new FileReader(path.toFile()) ) ) {
			// head informations
			line 		= reader.readLine();
			variables 	= Integer.parseInt(line.split(" ")[0]);
			line 		= reader.readLine();
			constraints = Integer.parseInt(line.split(" ")[0]);
			
			// read constraints
			double[][] vars 		= new double[constraints][variables];
			double[] sols 			= new double[constraints];
			ConstraintType[] types 	= new ConstraintType[constraints];
			
			for ( int i = 0; i < constraints; i++ ){
				// read A matrix
				line = reader.readLine();
				String[] lineComps = line.split(" ");
				for ( int j = 0; j < variables; j++ )
					vars[i][j] = Double.parseDouble(lineComps[j]);
				
				// get constraint types
				types[i] = ConstraintType.getType( lineComps[variables] );
				// read solution value
				sols[i] = Double.parseDouble(lineComps[variables+1]);
			}
			
			// return binary program
			return new BinaryProgram( vars, sols, types );
		} catch (IOException e) {
			e.printStackTrace();
		}
		// otherwise return null
		return null;
	}
	
	/**
	 * Reads a binary program and show solutions on console.
	 * @param args should contains a relative path to binary program file first.
	 */
	public static void main(String[] args) {
		// wrong usage.
		if ( args == null || args.length < 1 ){
			System.err.println("You have to specify a binary program file!");
			return;
		}
		
		// get path
		Path path = Paths.get(args[0]);
		
		// read from file
		BinaryProgram bp = BinarySolver.readFile( path );
		
		// solve the problem
		bp.solve();
	}

}
