package exercises;

import java.io.*;
import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.ArrayList;
import java.util.Arrays;

import com.sun.istack.internal.ByteArrayDataSource;
import com.sun.xml.internal.ws.encoding.MtomCodec.ByteArrayBuffer;

import sun.misc.IOUtils;


public class Ex1 {

	public Ex1(){
		
	}
	
	
	public byte[] readEx1(){
		BufferedInputStream in = null;
		BufferedOutputStream out = null;
		byte[] buf; 
		int[] arr = new int[62500002];
		try {
			in = new BufferedInputStream(new FileInputStream("ndata.dat"));
			buf = new byte[32];
			int count=0;
			while(in.read(buf)!=-1){
				arr[count]=byteArrayToInt(buf);
				count++;
				//System.out.println(" " + byteArrayToInt(buf));
			}
			System.out.println(arr.length + "Array size");
			in.close();
			System.out.println("vorher" + arr[0] + " " + arr[1] + " " + arr[2] + " " + arr[3]+ " " + arr[4]);
			Arrays.sort(arr);
			System.out.println("vorher" + arr[0] + " " + arr[1] + " " + arr[2] + " " + arr[3]+ " " + arr[4]);
			
			
			//out = new BufferedOutputStream(new FileOutputStream("outi.txt"));
			String str = new String();
			for(int i=1;i<arr.length;i++){
				if((arr[i]>=0)&&(arr[i-1]!=arr[i])){
					str = str + Character.toString((char) arr[i]) + "\n";
					//str = str + Integer.toString(arr[i]) + "\n";
					//System.out.println(arr[i]);
				}
			}
			System.out.println(str);
			Writer writer = new FileWriter("outi.txt");
			writer.write(str);
			writer.close();
			
		} catch (IOException e) {
			// TODO Auto-generated catch block
			System.out.println("error while reading ndata.dat file");
		}
		//System.out.println("tada "+byteArrayToInt(buf));
		
		//int intA = byteArrayToInt(buf);
		
		
		byte[] a = new byte[2];
		a[0] = 1;
		return a;
	}
	public static int byteArrayToInt(byte[] b){
		final ByteBuffer bb = ByteBuffer.wrap(b);
		bb.order(ByteOrder.LITTLE_ENDIAN);
		return bb.getInt();
	}

}
