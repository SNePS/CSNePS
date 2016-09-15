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
	
	private final static HashMap<String, Set<Channel>> ichannels = new HashMap<String, Set<Channel>>(); 
	private final static HashMap<String, Set<Channel>> uchannels = new HashMap<String, Set<Channel>>(); 
	private final static HashMap<String, Set<Channel>> gchannels = new HashMap<String, Set<Channel>>(); 
	private final static HashMap<String, Set<Channel>> antinchannels = new HashMap<String, Set<Channel>>(); 
	
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
	private final static Keyword activation_key = Keyword.intern("activation-value");
	
	private IPersistentMap term;
	
	private ArrayList<Term> upcablesetterms;
	private Caseframe caseframe;
	private HashMap<Slot, Set<Term>> downcableset;
	
	
	private String termstring;
	private String termname;
	
	private String fsymbol;
	
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
	
	////////////////
	/// Channels ///
	////////////////
	
	public Set<Channel> getUChannels(){
		return uchannels.get(this.getName());
	}

	public void addUChannels(Set<Channel> chs){
		if (chs == null) return;
	
		if(!uchannels.containsKey(this.getName()))
			uchannels.put(this.getName(), new HashSet<Channel>());
		
		uchannels.get(this.getName()).addAll(chs);
	}
	
	public Set<Channel> getIChannels(){
		return ichannels.get(this.getName());
	}

	public void addIChannels(Set<Channel> chs){
		if (chs == null) return;
	
		if(!ichannels.containsKey(this.getName()))
			ichannels.put(this.getName(), new HashSet<Channel>());
		
		ichannels.get(this.getName()).addAll(chs);
	}
	
	public Set<Channel> getGChannels(){
		return gchannels.get(this.getName());
	}

	public void addGChannels(Set<Channel> chs){
		if (chs == null) return;
	
		if(!gchannels.containsKey(this.getName()))
			gchannels.put(this.getName(), new HashSet<Channel>());
		
		gchannels.get(this.getName()).addAll(chs);
	}
	
	public Set<Channel> getAntInChannels(){
		return antinchannels.get(this.getName());
	}

	public void addAntInChannels(Set<Channel> chs){
		if (chs == null) return;
	
		if(!antinchannels.containsKey(this.getName()))
			antinchannels.put(this.getName(), new HashSet<Channel>());
		
		antinchannels.get(this.getName()).addAll(chs);
	}
	
	public Boolean isMolecular(){
		return FnInterop.molecularTermQ(term);
	}
	
	public Boolean isVariable(){
		return FnInterop.variableTermQ(term);
	}
	
	public void resetAsserted(){
		// Noop 
	}
	
	public Boolean isAsserted(){
		return isAsserted(Context.getCurrentContext());
	}
	
	public Boolean isAsserted(Context inContext){
		return FnInterop.isAsserted(this, inContext);
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
