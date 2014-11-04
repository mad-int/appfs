import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.BitSet;

/**
 * This is a programming exercise of the course
 * 		ADVANCED PROGRAMMING
 * at the technical university of berlin (TUB) in
 * the winter semester 2014/15.
 * Prof. Dr. Thorsten Koch
 * Assistant Matthias Miltenberger
 * 
 * Exercise 1
 * 		read and sort signed little endian 32 bit integers
 *
 * @author André Greiner-Petter
 *
 */
public class Ex1 {
	private static boolean flag = true;
	
	/**
	 * Input - binary file filled with signed little endian 32 bit integers.
	 * Ouput - prints numbers, starting from 0, in increasing order and each number at most once,
	 * 			one number per line, no leading zeros or spaces, lines ended by a single newline.
	 * @param args expected an absolute path to the ndata.dat file.
	 */
	public static void main(String[] args) {
		// catch wrong input
		if ( args.length < 1 ) {
			System.err.println("You have to specify the file to read.");
			System.err.println("java -jar ex1 <pathToFile>");
			return;
		}
		long start = System.currentTimeMillis();
		
		// boolean set for fast order
		// BitSet is quite slower than boolean array but needs 1/8 storage in general
		BitSet numbers = new BitSet(Integer.MAX_VALUE);
		
		// try to read from file
		try ( InputStream in = Files.newInputStream(
				Paths.get(args[0]), 
				StandardOpenOption.READ) 
				) {
			
			// 32 bit = 4 byte per integer
			byte[] buffer = new byte[4*2048];
			
			int readed = in.read(buffer);
			while ( readed > 0 ){
				for ( int i = 0; i < readed-4; i = i+4 ){
					int j = (	buffer[i] << 24 | 
								(buffer[i+1] & 0xFF) << 16 | 
								(buffer[i+2] & 0xFF) << 8 | 
								(buffer[i+3] & 0xFF) );
					// ignore negative values
					if ( j <= 0 ) continue;
					// store others, set the flag
					numbers.set(j);
				}
				readed = in.read(buffer);
			}
		} catch (IOException e) { // catch exceptions
			System.err.println("ERROR: " + e.getMessage());
			e.printStackTrace();
		}
		System.out.println("Finished reading after: " + (System.currentTimeMillis() - start));
		if (flag) return;
        // create an output file
        File outputFile = Paths.get(args[0]).getParent().resolve("output.txt").toFile();
        if ( outputFile.exists() ) outputFile.delete();
        try {
            outputFile.createNewFile();
        } catch (IOException e1) {
            System.err.println("Cannot create an output file.");
            e1.printStackTrace();
            return;
        }
       
        // use an OutputStreamWriter to define encoding
        try ( Writer out = new BufferedWriter( new OutputStreamWriter( new FileOutputStream(outputFile.getAbsoluteFile()), "US-ASCII" ) ) ) {
            // prints all true bits, nextSetBit(i) returns -1 if there are no more true bits after i
            for ( int i = numbers.nextSetBit(0); i >= 0; i = numbers.nextSetBit(i+1) )
                out.write(i + "\n");
           
            out.flush();
            System.out.println("Finished after " + (System.currentTimeMillis()-start) + "ms.");
        } catch (IOException e) { // catch exceptions
            System.err.println("Cannot write to output file: " + e.getMessage());
            e.printStackTrace();
        }
	}
}
