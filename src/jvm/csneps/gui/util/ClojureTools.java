package csneps.gui.util;

public class ClojureTools {

	public static boolean matchingParens(String s){
		boolean inStr = false; 
		boolean inComment = false;
		
		int openCount = 0;
		int closeCount = 0;
		
		for (int i = 0; i < s.length(); i++){
			char c = s.charAt(i);
			
			if (c == '"') inStr = !inStr;
			if (c == ';') inComment = true;
			if (c == '\n' || c == '\r') inComment = false;
			else if (!inStr && !inComment && c == '(') openCount++; 
			else if (!inStr && !inComment && c == ')') closeCount++;
		}
		
		return openCount == closeCount;
	}
	
	
}
