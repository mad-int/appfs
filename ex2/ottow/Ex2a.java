package exercises;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;

public class Ex2a {
	
	public static void main(String[] args) {

		String outi = new String();
		try{
			File file = new File("/Users/marla/Mathe/Java/APPFS/src/exercises/Ex2a.java");
			FileReader fr = new FileReader(file);System.out.println("hier");
			BufferedReader br = new BufferedReader(fr);
			
			String line = br.readLine();
			while(true){
	            outi = outi.concat(line);
	            outi = outi.concat("\n");
	            line = br.readLine();
				if(line==null) break;
			}
			br.close();
		}
		catch(Exception e){
			System.out.println("error while reading java-file");
		}
		System.out.println(outi);
	
	}
}
