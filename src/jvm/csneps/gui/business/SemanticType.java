package csneps.gui.business;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Collection;
import java.util.Set;

import clojure.lang.APersistentSet;
import clojure.lang.IPersistentMap;
import clojure.lang.Keyword;
import clojure.lang.RT;
import csneps.gui.GUI2;

public class SemanticType {
	private static HashMap<String, SemanticType> semtypes = new HashMap<String, SemanticType>();
	
	private String typename;
	private ArrayList<String> parents;
	
	private SemanticType(String typename, ArrayList<String> parents){
		this.typename = typename;
		this.parents = parents;
	}
	
	
	public static SemanticType create(String typename, ArrayList<String> parents){
		if (semtypes.get(typename) != null)
			return semtypes.get(typename);
		else{
			SemanticType s = new SemanticType(typename, parents);
			semtypes.put(typename, s);
			if(GUI2.DEBUG) System.out.println("Added Semantic Type: " + s.getName() + " with parents " + s.getParents());
			return s;
		}
	}
	
	public static Collection<SemanticType> getSemanticTypes(){
		return semtypes.values();
	}
	
	public static Collection<SemanticType> reinitializeSemanticTypes(IPersistentMap stsfull){
		semtypes = new HashMap<String, SemanticType>();
		semtypes.put("Entity", new SemanticType("Entity", new ArrayList<String>()));
		
		IPersistentMap sts = (IPersistentMap)stsfull.valAt(Keyword.intern("parents"));
		
		//System.out.println("STS: " + sts);
		
		for (Iterator iter = sts.iterator(); iter.hasNext(); ){
			Object entry = iter.next();
    		Keyword key = (Keyword)RT.first(entry);
    		Object[] vals = ((APersistentSet)RT.second(entry)).toArray();
    		ArrayList<String> strvals = new ArrayList<String>();
    		for (int i = 0; i < vals.length; i++){
    			strvals.add(((Keyword)vals[i]).getName());
    		}
    		//System.out.println("Name: " + key.getName() + " Parents: " + strvals);
    		
    		create(key.getName(), strvals);
    	}
		
		return semtypes.values();
	}
	
	public static SemanticType getSemanticType(String name){
		return semtypes.get(name);
	}
	
	public ArrayList<SemanticType> getParents(){
		ArrayList<SemanticType> p = new ArrayList<SemanticType>();
		for(String s : parents)
			p.add(semtypes.get(s));
		return p;
	}
	
	public Set<SemanticType> getAncestors(){
		Set<SemanticType> a = new HashSet<SemanticType>();
		for(SemanticType p : getParents()){
			a.add(p);
			a.addAll(p.getAncestors());
		}
		return a;
	}
	
	public boolean hasAncestor(SemanticType p){
		return getAncestors().contains(p);
	}
	
	public boolean hasParent(SemanticType p){
		for(String s : parents)
			if (s.equals(p.getName())) return true;
		return false;
	}
	
	public String getName(){
		return typename;
	}
	
	public String toString(){
		return typename;
	}
}
