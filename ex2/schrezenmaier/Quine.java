public class Quine {

	private static final String code =
			"public class Quine {"
			+ "\n"
			+ "\n    private static final String code ="
			+ "\n"
			+ "\n"
			+ "\n    private static String modifyString(String s) {"
			+ "\n        String quoteReplacement = \"W\";"
			+ "\n        String newlineReplacement = \"Y\";"
			+ "\n        String result = \"            \""
			+ "\n                + quoteReplacement"
			+ "\n                + s.replace(\"\\n\", quoteReplacement + \"\\n            + \""
			+ "\n                        + quoteReplacement + newlineReplacement)"
			+ "\n                + quoteReplacement + \";\";"
			+ "\n        result = result.replace(\"\\\\\", \"\\\\\\\\\");"
			+ "\n        result = result.replace(\"\\\"\", \"\\\\\\\"\");"
			+ "\n        result = result.replace(quoteReplacement, \"\\\"\");"
			+ "\n        result = result.replace(newlineReplacement, \"\\\\n\");"
			+ "\n        return result;"
			+ "\n    }"
			+ "\n"
			+ "\n    private static String getCode() {"
			+ "\n        int thirdBreak = code.indexOf('\\n',"
			+ "\n                code.indexOf('\\n', code.indexOf('\\n') + 1) + 1);"
			+ "\n        return code.substring(0, thirdBreak + 1) + modifyString(code)"
			+ "\n                + code.substring(thirdBreak + 1, code.length());"
			+ "\n    }"
			+ "\n"
			+ "\n    public static void main(String[] args) {"
			+ "\n        System.out.println(getCode());"
			+ "\n    }"
			+ "\n"
			+ "\n}";

	private static String modifyString(String s) {
		String quoteReplacement = "W";
		String newlineReplacement = "Y";
		String result = "            "
				+ quoteReplacement
				+ s.replace("\n", quoteReplacement + "\n            + "
						+ quoteReplacement + newlineReplacement)
				+ quoteReplacement + ";";
		result = result.replace("\\", "\\\\");
		result = result.replace("\"", "\\\"");
		result = result.replace(quoteReplacement, "\"");
		result = result.replace(newlineReplacement, "\\n");
		return result;
	}
	
	private static String getCode() {
		int thirdBreak = code.indexOf('\n',
				code.indexOf('\n', code.indexOf('\n') + 1) + 1);
		return code.substring(0, thirdBreak + 1) + modifyString(code)
				+ code.substring(thirdBreak + 1, code.length());
	}

	public static void main(String[] args) {
		System.out.println(getCode());
	}

}
