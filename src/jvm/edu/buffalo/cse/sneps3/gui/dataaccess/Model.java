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
	
	//New stuff:
	
	ArrayList<IView> views;
	
    Var slots_ref;
    Var caseframes_ref;
    Var fsymbols_ref;
    Var terms_ref;
    Var contexts_ref;
    Var curr_context_ref;
    Var types_ref;

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
    	for(IView i : views){
    		i.termUpdate(t, clear);
        }
    }
    
    public void currentContextHypsChanged(APersistentSet hyps){
    	System.err.println(PersistentVector.create(hyps.seq()));
    	for (Iterator itr = hyps.iterator(); itr.hasNext(); ){
    		Term.create((IPersistentMap)itr.next()).resetAsserted();
    	}
    }
    
    public void contextsChanged(IPersistentMap addedcontexts, Boolean clear){
    	if(clear){
    		Context.clearContexts();
    		for(IView i : views){
        		i.ctUpdate(new ArrayList<Context>(), true);
            }
    		return;
    	}
    	
    	ArrayList<Context> newcts = Context.createContexts(addedcontexts);
    	for(IView i : views){
    		i.ctUpdate(newcts, false);
        }
    }
    
    public void currentContextChanged(IPersistentMap currct){
    	Context.setCurrentContext(Context.create(currct));
        for(IView i : views){
            i.ctCurrent(Context.getCurrentContext());
        }
    }
    
    public void slotsChanged(IPersistentMap addedslots, Boolean clear){
    	if(clear){
    		Slot.clearSlots();
    		for(IView i : views){
        		i.slotUpdate(new ArrayList<Slot>(), true);
            }
    		return;
    	}

    	ArrayList<Slot> s = new ArrayList<Slot>();
    	s.add(Slot.create(addedslots));
    	for(IView i : views){
    		i.slotUpdate(s, false);
        }
    }
    
    public void caseframesChanged(APersistentSet addedcfs, Boolean clear){
    	if(clear){
    		Caseframe.clearCaseframes();
    		for(IView i : views){
        		i.cfUpdate(new ArrayList<Caseframe>(), true);
            }
    		return;
    	}
    	
    	ArrayList<Caseframe> c = Caseframe.createCaseframes(addedcfs);
    	for(IView i : views){
    		i.cfUpdate(c, false);
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
}

