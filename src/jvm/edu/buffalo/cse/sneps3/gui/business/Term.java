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
import clojure.lang.Var;

public class Term {

	private final static HashMap<String, Term> terms = new HashMap<String, Term>();
	private final static HashMap<String, HashMap<Slot, Set<Term>>> upcableset = new HashMap<String, HashMap<Slot, Set<Term>>>();
	private final static HashMap<String, Set<Term>> restrictionset = new HashMap<String, Set<Term>>();
	private final static HashMap<String, Set<Term>> dependenciesset = new HashMap<String, Set<Term>>();
	private final static HashMap<String, HashSet<Channel>> ichannels = new HashMap<String, HashSet<Channel>>(); 
	private final static HashMap<String, HashSet<Channel>> uchannels = new HashMap<String, HashSet<Channel>>(); 
	private final static HashMap<String, HashSet<Channel>> gchannels = new HashMap<String, HashSet<Channel>>(); 
	

	Var i_channels_ref;
    Var u_channels_ref;
    Var g_channels_ref;
    Var ant_in_channels_ref;
    Var up_cableset_w_ref;
    Var restriction_set_ref;
    Var dependencies_ref;
    Var down_cableset_ref;
    Var caseframe_ref;   
	
	
	
	private final static Keyword name_key = Keyword.intern("name");
	private final static Keyword type_key = Keyword.intern("type");
	private final static Keyword min_key = Keyword.intern("min");
	private final static Keyword max_key = Keyword.intern("max");
	//private final static Keyword caseframe_key = Keyword.intern("caseframe");
	//private final static Keyword upcablesetw_key = Keyword.intern("up-cablesetw");
	//private final static Keyword downcableset_key = Keyword.intern("down-cableset");
	private final static Keyword activation_key = Keyword.intern("activation-value");
	//private final static Keyword i_channels_key = Keyword.intern("i-channels");
	//private final static Keyword u_channels_key = Keyword.intern("u-channels");
	//private final static Keyword g_channels_key = Keyword.intern("g-channels");
	
	private IPersistentMap term;
	
	private ArrayList<Term> upcablesetterms;
	private Caseframe caseframe;
	private HashMap<Slot, Set<Term>> downcableset;
	
	
	private String termstring;
	private String termname;
	
	private String fsymbol;
	
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
		terms.clear();
		for (Iterator<IPersistentMap> iter = sts.iterator(); iter.hasNext(); ){
			create(iter.next());
		}
			
		return getTerms();
	}
	
	public IPersistentMap getClojureTerm(){
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
	
	public Long getMin(){
		return (Long)term.valAt(min_key);
	}
	
	public Long getMax(){
		return (Long)term.valAt(max_key);
	}
	
	public Caseframe getCaseframe(){
		return caseframe;
	}
	
	public void setCaseframe(Caseframe c){
		caseframe = c;
	}
	
	public String getFSymbol(){
		if (fsymbol != null) return fsymbol;
		fsymbol = FnInterop.getTermPredicate(this);
		return fsymbol;
	}
	
	public ArrayList<Term> getUpCablesetTerms(){
		if(upcablesetterms != null) return upcablesetterms;
		
		upcablesetterms = new ArrayList<Term>();
		
		return upcablesetterms;
	}
	
	public HashMap<Slot, Set<Term>> getUpCableset(){
		if (upcableset.get(this.getName()) != null){
			return upcableset.get(this.getName());
		}
		return null;
	}
	
	private void addToUpCableset(Slot s, Term t){
		if (upcableset.get(this.getName()) == null)
			upcableset.put(this.getName(), new HashMap<Slot,Set<Term>>());
		
		HashMap<Slot, Set<Term>> ucs = upcableset.get(this.getName());
		
		if(ucs.get(s) == null)
			ucs.put(s, new HashSet<Term>());
		
		Set<Term> slotterms = ucs.get(s);
		
		slotterms.add(t);
		
		if(upcablesetterms == null) upcablesetterms = new ArrayList<Term>();
		upcablesetterms.add(t);
	}
	
	public HashMap<Slot, Set<Term>> getDownCableset(){
		return downcableset;
	}
	
	public void setDownCableset(HashMap<Slot, Set<Term>> dcs){
		if (dcs == null) return;
		
		downcableset = dcs;
		
		for (Slot s : dcs.keySet()){
			for (Term t : dcs.get(s)){
				t.addToUpCableset(s, this);
			}
		}
	}
	
	public Set<Term> getRestrictionset(){
		return restrictionset.get(this.getName());
	}
	
	public void setRestrictionset(Set<Term> rs){
		if (rs == null) return;
		
		restrictionset.put(this.getName(), rs);
	}
	
	public Set<Term> getDependencies(){
		return dependenciesset.get(this.getName());
	}
	
	public void setDependencies(Set<Term> rs){
		if (rs == null) return;
		
		dependenciesset.put(this.getName(), rs);
	}
	
	//The number of i-channels can increase. Compare arity of cache with the one in the term
	//to determine if we have to do real work.
	public ArrayList<Channel> getIChannels(){
//		APersistentSet i = (APersistentSet)((Ref)term.valAt(i_channels_key)).deref();
//		if(i.count() == ichannels.size()) return ichannels;
//		
//		//Do the real work.
//		for (Iterator<IPersistentMap> iter = i.iterator(); iter.hasNext(); ){
//			Channel c = Channel.create(iter.next());
//			if(ichannels.contains(c)) continue;
//			ichannels.add(c);
//		}
//		return ichannels;
		
		return null;
	}
	
	//The number of u-channels can increase. Compare arity of cache with the one in the term
	//to determine if we have to do real work.
	public ArrayList<Channel> getUChannels(){
//		APersistentSet u = (APersistentSet)((Ref)term.valAt(u_channels_key)).deref();
//		if(u.count() == uchannels.size()) return uchannels;
//		
//		//Do the real work.
//		for (Iterator<IPersistentMap> iter = u.iterator(); iter.hasNext(); ){
//			Channel c = Channel.create(iter.next());
//			if(uchannels.contains(c)) continue;
//			uchannels.add(c);
//		}
//		return uchannels;
		
		return null;
	}
	
	//The number of u-channels can increase. Compare arity of cache with the one in the term
	//to determine if we have to do real work.
	public ArrayList<Channel> getGChannels(){
//		APersistentSet g = (APersistentSet)((Ref)term.valAt(g_channels_key)).deref();
//		if(g.count() == gchannels.size()) return gchannels;
//		
//		//Do the real work.
//		for (Iterator<IPersistentMap> iter = g.iterator(); iter.hasNext(); ){
//			Channel c = Channel.create(iter.next());
//			if(gchannels.contains(c)) continue;
//			gchannels.add(c);
//		}
//		return gchannels;
		
		return null;
	}
	
	public Boolean isMolecular(){
		return FnInterop.molecularTermQ(term);
	}
	
	public Boolean isVariable(){
		return FnInterop.variableTermQ(term);
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
