package edu.buffalo.cse.sneps3.gui.business;

import java.util.ArrayList;

import clojure.lang.PersistentList;
import clojure.lang.Symbol;

public class InteropUtils {

	@SuppressWarnings({ "rawtypes", "unchecked" })
	public static PersistentList arrayListToPersistentList(ArrayList a){
		ArrayList intermediate = new ArrayList();
		for (int i = 0; i < a.size(); i++){
			if(a.get(i) instanceof ArrayList)
				arrayListToPersistentList((ArrayList)a.get(i));
			else intermediate.add(Symbol.create((String) a.get(i)));
		}
		return (PersistentList) PersistentList.create(intermediate);
	}
	
}
