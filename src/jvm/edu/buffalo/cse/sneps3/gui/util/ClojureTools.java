package edu.buffalo.cse.sneps3.gui.util;

public class ClojureTools {

	public static boolean matchingParens(String s){
		boolean inStr = false; 
		
		int openCount = 0;
		int closeCount = 0;
		
		for (int i = 0; i < s.length(); i++){
			char c = s.charAt(i);
			
			if (c == '"') inStr = !inStr;
			else if (!inStr && c == '(') openCount++; 
			else if (!inStr && c == ')') closeCount++;
		}
		
		return openCount == closeCount;
	}
	
	
}
