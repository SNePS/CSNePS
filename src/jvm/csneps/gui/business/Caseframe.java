package csneps.gui.business;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Collection;
import java.util.Map;
import java.util.Set;

import clojure.lang.APersistentSet;
import clojure.lang.ISeq;
import clojure.lang.IPersistentMap;
import clojure.lang.IPersistentVector;
import clojure.lang.PersistentVector;
import clojure.lang.Keyword;
import csneps.gui.GUI2;

public class Caseframe implements Comparable<Caseframe>{

	private static HashMap<String, Caseframe> cfs = new HashMap<String, Caseframe>();
	private static HashMap<String, Caseframe> fsymbols = new HashMap<String, Caseframe>();
	
	private static Keyword name_key = Keyword.intern("name");
	private static Keyword type_key = Keyword.intern("type");
	private static Keyword slots_key = Keyword.intern("slots");
	
	
	private IPersistentMap cf;
	
	private ArrayList<String> slotnames;
	private ArrayList<Slot> slots;
	
	private Caseframe(IPersistentMap cf){
		this.cf = cf;
	}
	
	public static Caseframe create(IPersistentMap newcf){
		Caseframe c = new Caseframe(newcf);
		String key = c.getName() + c.getSlotNames().toString();
		
		if (cfs.get(key) != null)
			return cfs.get(key); 
		else{
			cfs.put(key, c);
			if(GUI2.DEBUG)
				System.err.println("Created Caseframe: " + c.getName() + " " + c.getSlots() + " " + c.getType());
			return c;
		}
	}
	
	/**
	 * 
	 * @param cljcfs
	 * @return Returns only new caseframes created.
	 */
	public static ArrayList<Caseframe> createCaseframes(APersistentSet cljcfs){
		ArrayList<Caseframe> newcfs = new ArrayList<Caseframe>();
		for (Iterator<IPersistentMap> iter = cljcfs.iterator(); iter.hasNext(); ){
			IPersistentMap cljcf = iter.next();
			newcfs.add(create(cljcf));
		}
		return newcfs;
	}
	
	public static void clearCaseframes(){
		cfs.clear();
		fsymbols.clear();
	}
	
	public static Collection<Caseframe> reinitializeCaseframes(IPersistentMap fsyms, APersistentSet scs){
		cfs = new HashMap<String, Caseframe>();
		for (Iterator<IPersistentMap> iter = scs.iterator(); iter.hasNext(); ){
			create(iter.next());
		}
		
		for (Iterator<Map.Entry> iter = fsyms.iterator(); iter.hasNext(); ){
			Map.Entry e = iter.next();
			fsymbols.put(
					e.getKey().toString(),
					create((IPersistentMap)e.getValue()));
		}
			
		return getCaseframes();
	}
	
	public static Collection<Caseframe> getCaseframes(){
		return cfs.values();
	}
	
	public static Caseframe getCaseframe(String cf, ArrayList<String> slots){
		return cfs.get(cf + slots.toString());
	}
	
	public String getName(){
		return FnInterop.getCaseframeName(this.cf);
	}
	
	public SemanticType getType(){ 
		return SemanticType.getSemanticType(((Keyword)cf.valAt(type_key)).getName());
	}
	
	public ArrayList<String> getSlotNames(){
		if(slotnames == null){
			
			slotnames = new ArrayList<String>();
			IPersistentVector v = PersistentVector.create((ISeq)cf.valAt(slots_key));
			for(int i = 0; i < v.length(); i++){
				IPersistentMap cljslot = (IPersistentMap)v.nth(i);
				slotnames.add(cljslot.valAt(name_key).toString());
			} 
			
		}
		return slotnames;
	}
	
	public ArrayList<Slot> getSlots(){
		if(slots == null){
			slots = new ArrayList<Slot>();

			for(String s : getSlotNames()){
				slots.add(Slot.getSlot(s));
			}
		}
		
		return slots;
	}
	
	public static void addFSymbols(IPersistentMap fsyms){
		for (Map.Entry e : (Iterable<Map.Entry>) fsyms) {
			fsymbols.put(
					e.getKey().toString(),
					create((IPersistentMap) e.getValue()));
		}
	}
	
	public static void addFSymbol(String s, Caseframe c){
		fsymbols.put(s, c);
	}
	
	// We shouldn't cache this since (sameFrame ...) allows adding them.
	public Set<String> getFSymbols(){
		Set<String> fsyms = new HashSet<String>();
		if(FnInterop.quotedppQ(cf)) return fsyms;
		
		for(String k : fsymbols.keySet()){
			if(fsymbols.get(k) == this) fsyms.add(k);
		}
		return fsyms;
	}
	
	public String toString(){
		return getName();
	}

	public int compareTo(Caseframe c) {
		return this.toString().toLowerCase().compareTo(c.toString().toLowerCase());
	}
	
}
