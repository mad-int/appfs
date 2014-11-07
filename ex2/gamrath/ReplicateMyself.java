public class ReplicateMyself{
	public static void main(String[] args)
	{      
		String[] array = {    
				"public class ReplicateMyself{",
				"	public static void main(String[] args)",
				"	{      ",
				"		String[] array = {", 
				"			",
				"		};",
				"		char quote = 34;",
				"		for(int i = 0; i < 4; i++)       ",  
				"			System.out.println(array[i]);",
				"		for(int i = 0; i < array.length; i++)",   
				"			System.out.println(array[4] + quote + array[i] + quote + ',');",
				"		for(int i = 5; i < array.length; i++)   ",
				"			System.out.println(array[i]);",
				"	}",
				"}"				
		};
		char quote = 34;
		for(int i = 0; i < 4; i++)         
			System.out.println(array[i]);
		for(int i = 0; i < array.length; i++)   
			System.out.println(array[4] + quote + array[i] + quote + ',');
		for(int i = 5; i < array.length; i++)   
			System.out.println(array[i]);
	}
}

		
		