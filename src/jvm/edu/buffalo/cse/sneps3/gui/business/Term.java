package edu.buffalo.cse.sneps3.gui.business;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import clojure.lang.APersistentSet;
import clojure.lang.ASeq;
import clojure.lang.IPersistentMap;
import clojure.lang.IPersistentSet;
import clojure.lang.Keyword;
import clojure.lang.MapEntry;
import clojure.lang.RT;
import clojure.lang.Ref;

public class Term {

	private static HashMap<String, Term> terms = new HashMap<String, Term>();
	
	private static Keyword name_key = Keyword.intern("name");
	private static Keyword type_key = Keyword.intern("type");
	private static Keyword caseframe_key = Keyword.intern("caseframe");
	private static Keyword upcablesetw_key = Keyword.intern("up-cablesetw");
	private static Keyword downcableset_key = Keyword.intern("down-cableset");
	private static Keyword activation_key = Keyword.intern("activation-value");
	private static Keyword i_channels_key = Keyword.intern("i-channels");
	private static Keyword y_channels_key = Keyword.intern("y-channels");
	
	private IPersistentMap term;
	
	private Caseframe cf;
	private ArrayList<Term> upcablesetterms;
	private HashMap<Slot, Set<Term>> upcableset;
	private HashMap<Slot, Set<Term>> downcableset;
	private ArrayList<Channel> ichannels = new ArrayList<Channel>();
	private ArrayList<Channel> ychannels = new ArrayList<Channel>();
	
	
	private String termstring;
	private String termname;
	
	private Boolean isasserted = null;
	
	private Term(IPersistentMap term){
		this.term = term;
	}
	
	public static Term create(IPersistentMap term){
		Term t = new Term(term);
		if(terms.get(t.getName()) != null) return terms.get(t.getName());
		
		terms.put(t.getName(), t);
		return t;
	}
	
	public static HashSet<Term> createTerms(IPersistentMap term){
		HashSet<Term> hs = new HashSet<Term>();
		for(Iterator<MapEntry> itr = term.iterator(); itr.hasNext(); ){
			hs.add(create((IPersistentMap)itr.next().getValue()));
		}
		return hs;
	}
	
	public static void clearTerms(){
		terms.clear();
	}
	
	public static Collection<Term> reinitializeTerms(APersistentSet sts){
		terms = new HashMap<String, Term>();
		for (Iterator<IPersistentMap> iter = sts.iterator(); iter.hasNext(); ){
			create(iter.next());
		}
			
		return getTerms();
	}
	
	IPersistentMap getClojureTerm(){
		return term;
	}
	
	public static Collection<Term> getTerms(){
		return terms.values();
	}
	
	public static Term getTerm(String name){
		return terms.get(name);
	}
	
	public String getName(){
		if (termname == null) termname = term.valAt(name_key).toString();
		return termname;
	}
	
	public String getType(){
		return ((Keyword)term.valAt(type_key)).getName();
	}
	
	public Double getActivation(){
		return (Double)term.valAt(activation_key);
	}
	
	public Caseframe getCaseframe(){
		if(cf != null) return cf;
		
		Object caseframe = term.valAt(caseframe_key);
		if(caseframe == null) return null;
		cf = Caseframe.create((IPersistentMap)caseframe); //Will return already created cf if it exists.
		return cf;
	}
	
	public ArrayList<Term> getUpCablesetTerms(){
		if(upcablesetterms != null) return upcablesetterms;
		
		upcablesetterms = new ArrayList<Term>();
		
		//The up-cablesetw is a map from slot to set of terms.
		IPersistentMap ucmap = (IPersistentMap)term.valAt(upcablesetw_key);
		ASeq sets = (ASeq)RT.vals(ucmap);
		for(Iterator iter = sets.iterator(); iter.hasNext(); ){
			IPersistentSet termset = (IPersistentSet)iter.next();
			ASeq seq = (ASeq)termset.seq();
			for(Iterator iter2 = seq.iterator(); iter2.hasNext(); )
				upcablesetterms.add(Term.create((IPersistentMap)iter2.next()));
		}
		
		return upcablesetterms;
	}
	
	public HashMap<Slot, Set<Term>> getUpCableset(){
		if(upcableset != null) return upcableset;
		
		upcableset = new HashMap<Slot, Set<Term>>();
		
		//The up-cablesetw is a map from slot to set of terms.
		IPersistentMap ucmap = (IPersistentMap)((Ref)term.valAt(upcablesetw_key)).deref();
		for(Iterator<MapEntry> iter = ucmap.iterator(); iter.hasNext(); ){
			MapEntry e = iter.next();
			Slot relation = Slot.create((IPersistentMap)e.key());
			ASeq termset = (ASeq)((IPersistentSet)e.val()).seq();
			HashSet<Term> val = new HashSet<Term>();
			for(Iterator<IPersistentMap> iter2 = termset.iterator(); iter2.hasNext(); )
				val.add(Term.create((IPersistentMap)iter2.next()));
			upcableset.put(relation, val);
		}
		
		return upcableset;
	}
	
	public HashMap<Slot, Set<Term>> getDownCableset(){
		if (!isMolecular()) return null;
		if (downcableset != null) return downcableset;
		
		downcableset = new HashMap<Slot, Set<Term>>();
		
		List dcs = (List)term.valAt(downcableset_key);
		ArrayList<Slot> termslots = getCaseframe().getSlots();
		
		for(int i = 0; i < dcs.size(); i++){
			List terms = (List)((APersistentSet)dcs.get(i)).seq();
			HashSet<Term> termset = new HashSet<Term>();
			for(int j = 0; j < terms.size(); j++){
				termset.add(Term.create((IPersistentMap)terms.get(j)));
			}
			downcableset.put(termslots.get(i), termset);
		}
		
		return downcableset;
		
	}
	
	//The number of i-channels can increase. Compare arity of cache with the one in the term
	//to determine if we have to do real work.
	public ArrayList<Channel> getIChannels(){
		APersistentSet i = (APersistentSet)((Ref)term.valAt(i_channels_key)).deref();
		if(i.count() == ichannels.size()) return ichannels;
		
		//Do the real work.
		for (Iterator<IPersistentMap> iter = i.iterator(); iter.hasNext(); ){
			Channel c = Channel.create(iter.next());
			if(ichannels.contains(c)) continue;
			ichannels.add(c);
		}
		return ichannels;
	}
	
	//The number of y-channels can increase. Compare arity of cache with the one in the term
	//to determine if we have to do real work.
	public ArrayList<Channel> getYChannels(){
		APersistentSet y = (APersistentSet)((Ref)term.valAt(y_channels_key)).deref();
		if(y.count() == ychannels.size()) return ychannels;
		
		//Do the real work.
		for (Iterator<IPersistentMap> iter = y.iterator(); iter.hasNext(); ){
			Channel c = Channel.create(iter.next());
			if(ychannels.contains(c)) continue;
			ychannels.add(c);
		}
		return ychannels;
	}
	
	public Boolean isMolecular(){
		return FnInterop.molecularTermQ(term);
	}
	
	public void resetAsserted(){
		isasserted = isAsserted(Context.getCurrentContext());
	}
	
	public Boolean isAsserted(){
		if(Context.getCurrentContext()==null) return false;
		if(isasserted!=null) return isasserted;
		return (isasserted = isAsserted(Context.getCurrentContext()));
	}
	
	public Boolean isAsserted(Context inContext){
		return inContext.getHyps().contains(this);
	}
	
	/**
	 * Two terms are equal if they have the same name.
	 * @param term2
	 * @return
	 */
	public boolean equals(Term term2){
		return this.getName().equals(term2.getName());
	}
	
	public String toString(){
		if(this.termstring == null) termstring = FnInterop.termString(term);
		return termstring;
	}
	
	
}
