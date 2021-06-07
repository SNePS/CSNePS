package csneps.gui.business;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Collection;

import clojure.lang.IPersistentMap;
import clojure.lang.ASeq;
import clojure.lang.RT;
import clojure.lang.Keyword;
import csneps.api.ISlot;
import csneps.gui.GUI2;

public class Slot implements Comparable<Slot>, ISlot {

	private static HashMap<String, Slot> slots = new HashMap<String, Slot>();
	
	private IPersistentMap slot;
	
	private Slot(IPersistentMap slot){
		this.slot = slot;
	}
	
	public static Slot create(IPersistentMap slot){
		return createHelper((IPersistentMap)RT.first(RT.vals(slot)));
	}
	
	public static Slot createHelper(IPersistentMap slot){
		Slot s = new Slot(slot);
		if(slots.get(s.getName()) != null) return slots.get(s.getName());
		else{
			slots.put(s.getName(), s);
			if(GUI2.DEBUG)
				System.err.println("Slot Added: " + s.getName() + " " + s.getType() + " " + s.getMin() + " " + s.getMax());
			return s;
		}
	}
	
	public static Collection<Slot> getSlots(){
		return slots.values();
	}
	
	public static void clearSlots(){
		slots.clear();
	}
	
	@SuppressWarnings("unchecked")
	public static Collection<Slot> reinitializeSlots(IPersistentMap sls, Boolean clear){
		if(clear) slots.clear();
		for (Iterator<IPersistentMap> iter = ((ASeq)RT.vals(sls)).iterator(); iter.hasNext(); ){
			createHelper(iter.next());
			//Slot s = new Slot((IPersistentMap)iter.next());
			//if(slots.get(s.getName()) == null) slots.put(s.getName(), s);
		}
			
		return getSlots();
	}
	
	public static Slot getSlot(String s){
		return slots.get(s);
	}
	
	public String getName(){
		return slot.valAt(Keyword.intern("name")).toString();
	}
	
	public SemanticType getType(){
		return SemanticType.getSemanticType(slot.valAt(Keyword.intern("type")).toString());
	}
	
	public Long getMin(){
		return (Long)slot.valAt(Keyword.intern("min"));
	}
	
	public Long getMax(){
		if (slot.valAt(Keyword.intern("max")) != null)
			return (Long)slot.valAt(Keyword.intern("max"));
		return null;
	}
	
    @Override
    public String toString(){
        return getName();
    }

	@Override
	public int compareTo(Slot s) {
		return this.toString().compareTo(s.toString());
	}
	
}
