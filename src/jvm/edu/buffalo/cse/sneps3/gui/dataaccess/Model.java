package edu.buffalo.cse.sneps3.gui.dataaccess;

/* The Model class is designed to perform all of the interaction
 * from Clojure to Java. This keeps track of the data held by
 * Clojure and updates the implementors of IView.
 */

import java.util.*;

import clojure.lang.ASeq;
import clojure.lang.IPersistentSet;
import clojure.lang.PersistentVector;
import clojure.lang.RT;
import clojure.lang.Var;
import clojure.lang.Ref;
import clojure.lang.PersistentArrayMap;
import clojure.lang.PersistentHashSet;
import clojure.lang.IPersistentMap;
import clojure.lang.APersistentSet;
import edu.buffalo.cse.sneps3.gui.business.Caseframe;
import edu.buffalo.cse.sneps3.gui.GUI2;
import edu.buffalo.cse.sneps3.gui.business.Context;
import edu.buffalo.cse.sneps3.gui.business.SemanticType;
import edu.buffalo.cse.sneps3.gui.business.Slot;
import edu.buffalo.cse.sneps3.gui.business.IView;
import edu.buffalo.cse.sneps3.gui.business.Term;

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
    	Slot.reinitializeSlots((IPersistentMap)((Ref)slots_ref.get()).deref());
    }
    
    public void initializeCaseframes(){
    	Collection<Caseframe> c = Caseframe.reinitializeCaseframes(
    			(IPersistentMap)((Ref)fsymbols_ref.get()).deref(), 
    			(APersistentSet)((Ref)caseframes_ref.get()).deref());
    	
    	for(IView i : views){
    		i.cfUpdate(c, true);
        }
    }
    
    public void initializeTerms(){
    	initializeTerms(PersistentHashSet.create(RT.vals((IPersistentMap)((Ref)terms_ref.get()).deref())));
    }
    
    public void initializeTerms(APersistentSet termset){
    	Collection<Term> t = Term.reinitializeTerms(termset);
    	
    	for (Term term : t){
    		term.setCaseframe(getCaseframe(term.getClojureTerm()));
    		term.setDownCableset(getDownCableset(term));
    	}
    	
    	for(IView i : views){
    		i.termUpdate(t, false);
        }
    }
    
    public void initializeContexts(){
    	contextsChanged((IPersistentMap)((Ref)contexts_ref.get()).deref(), false);
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
    		// What takes longer - doing the deref in getCaseframe for all terms changed,
    		// or building an arraylist of names of terms changed, and using the function
    		// for that?
    		term.setCaseframe(getCaseframe(term.getClojureTerm()));
    		term.setDownCableset(getDownCableset(term));
    	}
    	for(IView i : views){
    		i.termUpdate(t, clear);
        }
    }
    
    public void currentContextHypsChanged(APersistentSet hyps){
    	if(GUI2.DEBUG) System.err.println(PersistentVector.create(hyps.seq()));
    	for (Iterator itr = hyps.iterator(); itr.hasNext(); ){
    		Term.getTerm(itr.next().toString()).resetAsserted();
    	
    		//Term.create((IPersistentMap)itr.next()).resetAsserted();
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
    	
    	ArrayList<Slot> s = new ArrayList<Slot>(Slot.reinitializeSlots(addedslots));
    	
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
    	if(GUI2.DEBUG) System.out.println("Ich" + changed.toString());
    }
    
    public void termNameGChannelMapChanged(IPersistentMap changed, Boolean reset){
    	
    }

    public void termNameUChannelMapChanged(IPersistentMap changed, Boolean reset){
	
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
    
    public HashMap<Slot, Set<Term>> getDownCableset(Term term){
    	if (!term.isMolecular()) return null;
    	
    	HashMap<Slot, Set<Term>> downcableset = new HashMap<Slot, Set<Term>>();
    	
    	ArrayList<Slot> termslots = term.getCaseframe().getSlots();

    	IPersistentMap namedcsmap = (IPersistentMap)((Ref)down_cableset_ref.get()).deref();
    	
    	List dcs = (List)namedcsmap.valAt(term.getClojureTerm());
    	
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
    
    
}

