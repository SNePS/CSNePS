package edu.buffalo.cse.sneps3.gui.business;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;

import clojure.lang.APersistentSet;
import clojure.lang.IPersistentMap;
import clojure.lang.ISeq;
import clojure.lang.Keyword;
import clojure.lang.MapEntry;
import clojure.lang.Ref;
import edu.buffalo.cse.sneps3.gui.GUI2;

/**
 * Java wrapper for CSNePS Contexts.
 * @author Daniel R. Schlegel
 */
public class Context implements Comparable{

	private static HashMap<String,Context> contexts = new HashMap<String,Context>();
	
	private static Keyword hyps_key = Keyword.intern("hyps");
	private static Keyword ders_key = Keyword.intern("ders");
	private static Keyword name_key = Keyword.intern("name");
	private static Keyword parents_key = Keyword.intern("parents");
	
	private static Context currentContext;
	
    private IPersistentMap context;
    
    private Context(IPersistentMap context){
        this.context = context;
    }
    
    public static Context create(IPersistentMap context){
    	Context c = new Context(context);
    	if(contexts.get(c.getName()) != null) 
    		return contexts.get(c.getName());
    	else{
    		contexts.put(c.getName(), c);
    		if(GUI2.DEBUG) 
    			System.err.println("Created context: " + c.getName());
    		return c;
    	}
    }
    
    public static ArrayList<Context> createContexts(IPersistentMap cljcts){
    	ArrayList<Context> cts = new ArrayList<Context>();
    	for (Iterator<MapEntry> itr = cljcts.iterator(); itr.hasNext(); ){
    		cts.add(create((IPersistentMap)itr.next().getValue()));
    	}
    	return cts;
    }
    
    public static void clearContexts(){
    	contexts.clear();
    }
    
    public static Context getContext(String name){
    	return contexts.get(name);
    }
    
    public static Collection<Context> getContexts(){
    	return contexts.values();
    }
    
    public static Context getCurrentContext(){
    	return currentContext;
    }
    
    public static void setCurrentContext(Context c){
    	currentContext = c;
    	for(Term t : Term.getTerms()){
    		t.resetAsserted();
    	}
    }
    
    public String getName(){
    	return context.valAt(name_key).toString();
    }
    
    public ArrayList<Context> getParents(){
    	ArrayList<Context> p = new ArrayList<Context>();
    	ISeq cljp = (ISeq)context.valAt(parents_key);
	    while(cljp.next() != null){
	    	p.add(create((IPersistentMap)cljp.first()));
	    	cljp = cljp.next();
	    }
    	return p;
    }
    
    //TODO: We're calling this really often. Maybe some efficiency changes can happen.
    public HashSet<Term> getHyps(){
    	HashSet<Term> hyps = new HashSet<Term>();
    	APersistentSet cljhyps = (APersistentSet)((Ref)context.valAt(hyps_key)).deref();
    	//System.out.println(cljhyps);
    	for (Iterator<IPersistentMap> itr = cljhyps.iterator(); itr.hasNext(); ){
    		hyps.add(Term.create(itr.next())); 
    	}
    	return hyps;
    }
    
    public HashSet<Term> getDers(){
    	HashSet<Term> ders = new HashSet<Term>();
    	APersistentSet cljders = (APersistentSet)((Ref)context.valAt(ders_key)).deref();
    	for (Iterator<IPersistentMap> itr = cljders.iterator(); itr.hasNext(); ){
    		ders.add(Term.create(itr.next()));
    	}
    	return ders;
    }
    
    IPersistentMap getClojureContext(){
    	return context;
    }

    @Override
    public String toString(){
        return getName();
    }

	@Override
	public int compareTo(Object arg0) {
		return this.toString().compareTo(arg0.toString());
	}
}
