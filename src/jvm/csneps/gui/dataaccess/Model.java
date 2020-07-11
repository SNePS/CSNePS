package csneps.gui.dataaccess;

/* The Model class is designed to perform all of the interaction
 * from Clojure to Java. This keeps track of the data held by
 * Clojure and updates the implementors of IView.
 */

import java.util.*;

import clojure.lang.PersistentVector;
import clojure.lang.RT;
import clojure.lang.Var;
import clojure.lang.Ref;
import clojure.lang.PersistentHashSet;
import clojure.lang.IPersistentMap;
import clojure.lang.APersistentSet;
import csneps.gui.GUI2;
import csneps.gui.business.Caseframe;
import csneps.gui.business.Channel;
import csneps.gui.business.Context;
import csneps.gui.business.SemanticType;
import csneps.gui.business.Slot;
import csneps.gui.business.IView;
import csneps.gui.business.Term;

/**
 *
 * @author dan
 */
public class Model {

	ArrayList<IView> views;
	
    Var slots_ref;
    Var caseframes_ref;
    Var fsymbols_ref;
    Var terms_ref;
    Var contexts_ref;
    Var curr_context_ref;
    Var types_ref;
    
    // Parts of terms which have been abstracted away from Terms themselves.
    Var i_channels_ref;
    Var u_channels_ref;
    Var g_channels_ref;
    Var ant_in_channels_ref;
    Var up_cableset_w_ref;
    Var restriction_set_ref;
    Var dependencies_ref;
    Var down_cableset_ref;
    Var caseframe_ref;   

    public Model(){ 
        views = new ArrayList<IView>();
    }
    
    public void registerView(IView i){
        views.add(i);
        if(GUI2.DEBUG) System.out.println("Registered view for " + i);
    }
    
    /*****************************
     *** References to Clojure ***
     *****************************/
    
    public void setTypesRef(Var v){
    	types_ref = v;
    }
    
    public void setTermsRef(Var v){
    	terms_ref = v;
    }
    
    public void setCaseframesRef(Var v){
    	caseframes_ref = v;
    }
    
    public void setFSymbolsRef(Var v){
    	fsymbols_ref = v;
    }
    
    public void setSlotsRef(Var v){
    	slots_ref = v;
    }
    
    public void setContextsRef(Var v){
    	contexts_ref = v;
    }
    
    public void setCurrentContextRef(Var v){
    	curr_context_ref = v;
    }
    
    public void setIChannelsRef(Var v){
    	i_channels_ref = v;
    }
    
    public void setUChannelsRef(Var v){
    	u_channels_ref = v;
    }
    
    public void setGChannelsRef(Var v){
    	g_channels_ref = v;
    }
    
    public void setAntInChannelsRef(Var v){
    	ant_in_channels_ref = v;
    }
    
    public void setUpCablesetWRef(Var v){
    	up_cableset_w_ref = v;
    }
    
    public void setRestrictionSetWRef(Var v){
    	restriction_set_ref = v;
    }
    
    public void setDependenciesRef(Var v){
    	dependencies_ref = v;
    }
    
    public void setDownCablesetRef(Var v){
    	down_cableset_ref = v;
    }
    
    public void setCaseframeRef(Var v){
    	caseframe_ref = v;
    }
    
    /********************************
     *** Initialization Functions ***
     ********************************/
    
    public void initializeTypes(){
    	Collection<SemanticType> c = SemanticType.reinitializeSemanticTypes((IPersistentMap)((Ref)types_ref.get()).deref());
    	
    	for(IView i : views){ 
    		i.stUpdate(c, false);
        }
    }
    
    public void initializeSlots(){
    	Slot.reinitializeSlots((IPersistentMap)((Ref)slots_ref.get()).deref(), true);
    }
    
    public void initializeCaseframes(){
    	Collection<Caseframe> c = Caseframe.reinitializeCaseframes(
    			(IPersistentMap)((Ref)fsymbols_ref.get()).deref(), 
    			(APersistentSet)((Ref)caseframes_ref.get()).deref());
    	
    	for(IView i : views){
    		i.cfUpdate(c, true);
        }
    }
    
    private void initializeTerm(Term term){
    	term.setCaseframe(getCaseframe(term.getClojureTerm()));
		term.setDownCableset(getDownCableset(term));
		term.setRestrictionset(getRestrictionset(term));
		term.setDependencies(getDepends(term));
    }
    
    public void initializeTerms(){
    	initializeTerms(PersistentHashSet.create(RT.vals((IPersistentMap)((Ref)terms_ref.get()).deref())));
    }
    
    public void initializeTerms(APersistentSet termset){
    	Collection<Term> t = Term.reinitializeTerms(termset);
    	
    	for (Term term : t){
    		initializeTerm(term);
    	}
    	
    	for(IView i : views){
    		i.termUpdate(t, false);
        }
    }
    
    public void initializeContexts(){
    	contextsChanged((IPersistentMap)((Ref)contexts_ref.get()).deref(), true);
    	currentContextChanged((IPersistentMap)((Ref)curr_context_ref.get()).deref());
    } 
    
    /************************
     *** Update Functions ***
     ************************/
    
    public void termsChanged(IPersistentMap changedterms, Boolean clear){
    	if(clear){
    		Term.clearTerms();
    		for(IView i : views){
        		i.termUpdate(new ArrayList<Term>(), clear);
            }
    		return;
    	}
    	ArrayList<Term> t = new ArrayList<Term>(Term.createTerms(changedterms));
    	
    	for (Term term : t){
    		initializeTerm(term);
    	}
    	
    	for(IView i : views){
    		i.termUpdate(t, clear);
        }
    }
    
    public void currentContextHypsChanged(APersistentSet hyps){
    	if(GUI2.DEBUG) System.err.println(PersistentVector.create(hyps.seq()));
    	//System.out.println("Changed: " + PersistentVector.create(hyps.seq()));
    	for (Iterator itr = hyps.iterator(); itr.hasNext(); ){
    		Term.getTerm(itr.next().toString()).resetAsserted();
    	}
    }
    
    public void contextsChanged(IPersistentMap addedcontexts, Boolean clear){
    	if(clear)
    		Context.clearContexts();
    	
    	ArrayList<Context> newcts = Context.createContexts(addedcontexts);
    	
    	for(IView i : views){
    		i.ctUpdate(newcts, clear);
        }
    }
    
    public void currentContextChanged(IPersistentMap currct){
    	Context.setCurrentContext(Context.create(currct));
        for(IView i : views){
            i.ctCurrent(Context.getCurrentContext());
        }
    }
    
    public void slotsChanged(IPersistentMap addedslots, Boolean clear){
    	if(clear)
    		Slot.clearSlots();
    	
    	ArrayList<Slot> s = new ArrayList<Slot>(Slot.reinitializeSlots(addedslots, clear));
    	
    	for(IView i : views){
    		i.slotUpdate(s, clear);
        }
    }
    
    public void caseframesChanged(APersistentSet addedcfs, Boolean clear){
    	if(clear)
    		Caseframe.clearCaseframes();
    	
    	ArrayList<Caseframe> c = Caseframe.createCaseframes(addedcfs);
    	for(IView i : views){
    		i.cfUpdate(c, clear);
        }
    }
    
    public void fsymbolsChanged(IPersistentMap addedfsyms){
    	Caseframe.addFSymbols(addedfsyms);
    	
    	//System.out.println("FSymbols added: " + addedfsyms.toString());
    }
    
    //Note: This gets ALL types, not just new ones.
    public void typesChanged(IPersistentMap changedtypes, Boolean remove){
    	initializeTypes();
    	
    	//System.out.println("Type added: " + types.toString());
    }
    
    public void termNameIChannelMapChanged(IPersistentMap changed, Boolean reset){
    	if(GUI2.DEBUG) 
    		System.out.println(reset + " Ich " + changed.toString());
    		
    	Map<String, Set<Channel>> chs = Channel.createChannelCollection(changed);

    	for (String tname : chs.keySet())
    		Term.getTerm(tname).addIChannels(chs.get(tname));

    	for (IView i : views){
			i.channelUpdate(chs, Channel.ChannelType.ICHANNEL, reset);
		}
    }
    
    public void termNameGChannelMapChanged(IPersistentMap changed, Boolean reset){
    	if(GUI2.DEBUG) 
			System.out.println(reset + " Gch " + changed.toString());
			
		Map<String, Set<Channel>> chs = Channel.createChannelCollection(changed);
		
		for (String tname : chs.keySet())
    		Term.getTerm(tname).addGChannels(chs.get(tname));

		for (IView i : views){
			i.channelUpdate(chs, Channel.ChannelType.GCHANNEL, reset);
		}
    }

    public void termNameUChannelMapChanged(IPersistentMap changed, Boolean reset){
    	if(GUI2.DEBUG) 
			System.out.println(reset + " Uch " + changed.toString());
			
		Map<String, Set<Channel>> chs = Channel.createChannelCollection(changed);
		
		for (String tname : chs.keySet())
    		Term.getTerm(tname).addUChannels(chs.get(tname));

		for (IView i : views){
			i.channelUpdate(chs, Channel.ChannelType.UCHANNEL, reset);
		}
    }
    
    /****************
     * Pull Methods *
     ****************/
    
    // Some things are better "pulled" then "pushed". 
    
    public Caseframe getCaseframe(IPersistentMap term){
    	IPersistentMap namecfmap = (IPersistentMap)((Ref)caseframe_ref.get()).deref();
    	if(namecfmap.containsKey(term)){
    		return Caseframe.create((IPersistentMap)namecfmap.entryAt(term).getValue());
    	}
    	return null;
    }
    
    public HashMap<IPersistentMap, Caseframe> getPartialNameCFMap(ArrayList<IPersistentMap> terms){
    	IPersistentMap namecfmap = (IPersistentMap)((Ref)caseframe_ref.get()).deref();
    	
    	HashMap<IPersistentMap, Caseframe> namecfmapret = new HashMap<IPersistentMap, Caseframe>();
    	
    	for(IPersistentMap i : terms){
    		if(namecfmap.containsKey(i)){
    			namecfmapret.put(i, Caseframe.create((IPersistentMap)namecfmap.entryAt(i).getValue()));
    		}
    	}
    	
    	return namecfmapret;
    }
    
    public HashMap<IPersistentMap, Caseframe> getNameCFMap(){
    	IPersistentMap namecfmap = (IPersistentMap)((Ref)caseframe_ref.get()).deref();
    	
    	HashMap<IPersistentMap, Caseframe> namecfmapret = new HashMap<IPersistentMap, Caseframe>();
    	
    	for (Iterator<Map.Entry> iter = namecfmap.iterator(); iter.hasNext(); ){
			Map.Entry e = iter.next();
			namecfmapret.put((IPersistentMap)e.getKey(),
					         Caseframe.create((IPersistentMap)e.getValue()));
		}
    	return namecfmapret;
    }
    
    private HashMap<Slot, Set<Term>> getDownCableset(Term term){
    	if (!term.isMolecular()) return null;
    	
    	HashMap<Slot, Set<Term>> downcableset = new HashMap<Slot, Set<Term>>();
    	
    	ArrayList<Slot> termslots = term.getCaseframe().getSlots();

    	IPersistentMap namedcsmap = (IPersistentMap)((Ref)down_cableset_ref.get()).deref();
    	
    	List dcs = (List)namedcsmap.valAt(term.getClojureTerm());
    	
		for(int i = 0; i < dcs.size(); i++){
			List terms = (List)((APersistentSet)dcs.get(i)).seq();
			HashSet<Term> termset = new HashSet<Term>();
			if(terms != null) // If a slot is unfilled, when terms is turned to a seq it will be null.
				for(int j = 0; j < terms.size(); j++){
					termset.add(Term.create((IPersistentMap)terms.get(j)));
				}
			downcableset.put(termslots.get(i), termset);
		}
    	
    	return downcableset;
    }
    
    private Set<Term> getRestrictionset(Term term){
    	if (!term.isVariable()) return null;
    	
    	HashSet<Term> restrictionset = new HashSet<Term>();
    	
    	IPersistentMap namersmap = (IPersistentMap)((Ref)restriction_set_ref.get()).deref();
    	
    	APersistentSet rs = (APersistentSet)namersmap.valAt(term.getClojureTerm());
    	
    	if(GUI2.DEBUG) System.out.println("Term: " + term.getName() + " RS: " + rs);
    	
    	for (Iterator itr = rs.iterator(); itr.hasNext(); ){
    		restrictionset.add(Term.create((IPersistentMap)itr.next()));
    	}
    	
    	return restrictionset;
    }
    
    private Set<Term> getDepends(Term term){
    	if (!term.isVariable()) return null;
    	
    	HashSet<Term> depset = new HashSet<Term>();
    	
    	IPersistentMap namedepmap = (IPersistentMap)((Ref)dependencies_ref.get()).deref();
    	
    	APersistentSet deps = (APersistentSet)namedepmap.valAt(term.getClojureTerm());
    	
    	if (deps == null) return depset;
    	
    	if(GUI2.DEBUG) System.out.println("Term: " + term.getName() + " Deps: " + deps);
    	
    	for (Iterator itr = deps.iterator(); itr.hasNext(); ){
    		depset.add(Term.create((IPersistentMap)itr.next()));
    	}
    	
    	return depset;
    }
    
    
}

