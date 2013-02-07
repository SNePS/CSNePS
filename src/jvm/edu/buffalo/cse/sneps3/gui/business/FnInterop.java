package edu.buffalo.cse.sneps3.gui.business;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import edu.buffalo.cse.sneps3.gui.business.InteropUtils;
import edu.buffalo.cse.sneps3.gui.dataaccess.Controller;
import clojure.lang.IPersistentList;
import clojure.lang.IPersistentMap;
import clojure.lang.ISeq;
import clojure.lang.Keyword;
import clojure.lang.MapEntry;
import clojure.lang.PersistentHashSet;
import clojure.lang.PersistentList;
import clojure.lang.IPersistentMap;
import clojure.lang.RT;
import clojure.lang.Symbol;

public class FnInterop {

	///////////
	/// gui ///
	///////////
	
	// These all have to do with the REPL.
	
	public static void evaulateExpression(){
		
	}
	
	
	//////////////
	/// snuser ///
	//////////////
	
	public static void clearkb(Boolean clearall){
		Controller.snuser_clearkb(clearall);
	}
	
	public static Term assertTerm(String expr){
		return Term.create(Controller.snuser_assert((PersistentList)RT.readString(expr)));
	}
	
	public static Term assertETerm(String expr){
		// Not implemented in CSNePS.
		return null;
	}
	
	public static Term defineTerm(String expr){
		return Term.create(Controller.snuser_define_term((PersistentList)RT.readString(expr)));
	}
	
	public static Term defineTerm(String expr, SemanticType type){
		
		return null;
	}
	
	@SuppressWarnings({ "rawtypes", "unchecked" })
	public static Caseframe defineCaseframe(String type, String name, ArrayList<Slot> slots){
		//Controller.snuser_define_caseframe();
		ArrayList slotsyms = new ArrayList();
		ArrayList<Symbol> cfnameal = new ArrayList<Symbol>();
		cfnameal.add(Symbol.intern("quote"));
		cfnameal.add(Symbol.intern(name));
		slotsyms.add(PersistentList.create(cfnameal));
	    for(Slot s : slots)
	    	slotsyms.add(Symbol.intern(s.getName()));

	   return Caseframe.create(Controller.snuser_define_caseframe(Keyword.intern(type), PersistentList.create(slotsyms)));
	}
	
	@SuppressWarnings("rawtypes")
	public static Caseframe defineCaseframe(String type, ArrayList<Slot> slots, ArrayList<String> fsymbols){
		ArrayList<Symbol> slotsyms = new ArrayList<Symbol>();
	    for(Slot s : slots)
	    	slotsyms.add(Symbol.intern(s.getName()));
	    ArrayList<Object> fsyms = new ArrayList<Object>();
	    for(String s : fsymbols)
	    	fsyms.add(RT.readString(s));
	    
	    return Caseframe.create(Controller.snuser_define_caseframe(Keyword.intern(type), PersistentList.create(slotsyms), PersistentList.create(fsyms)));
	}
	
	//////////////
	/// sneps3 ///
	//////////////
	
	public static SemanticType defineType(String newtype, ArrayList<SemanticType> parents){
		ArrayList<String> parentstrings = new ArrayList<String>();
		ArrayList<Symbol> parentsyms = new ArrayList<Symbol>();
		
		for(SemanticType t : parents){
			parentstrings.add(t.getName());
			parentsyms.add(Symbol.intern(t.getName()));
		}
		
		Controller.sneps3_define_type(Symbol.intern(newtype), PersistentList.create(parentsyms));
		return SemanticType.create(newtype, parentstrings);
	}
	
	public static Boolean molecularTermQ(IPersistentMap term){
		return Controller.sneps3_molecular_term_qmark(term);
	}
	
	//////////////////
	/// caseframes ///
	//////////////////
	
	public static Boolean quotedppQ(IPersistentMap cf){
		return Controller.caseframes_quotedpp_qmark(cf);
	}
	
	public static String getCaseframeName(IPersistentMap cf){
		return Controller.caseframes_caseframe_name(cf).toString();
	} 
	
	//////////////////////
	/// sneps3-printer ///
	//////////////////////
	
	public static String termString(IPersistentMap term){
		return Controller.sneps3_printer_term_printer(term);
	}
	
	/////////////////
	/// relations ///
	/////////////////
	
	public static Slot defineSlot(String name, SemanticType type, Integer min, Integer max, String posadjust, String negadjust){
		return Slot.createHelper(Controller.relations_define_slot(Symbol.intern(name), Keyword.intern(type.getName()), min, max, Symbol.intern(posadjust), Symbol.intern(negadjust)));
	}
	
	////////////////
	/// contexts ///
	////////////////
	
	public static Context defineContext(String name, ArrayList<Context> parents, Set<Term> hyps){
		ArrayList<Object> cljparents = new ArrayList<Object>();
		PersistentHashSet cljhyps = PersistentHashSet.create(new ArrayList<Object>());
		for(Term t : hyps)
			cljhyps.add(t.getClojureTerm());
		for(Context c : parents)
			cljparents.add(c.getClojureContext());
		
		return Context.create(Controller.contexts_define_context(Symbol.intern(name), PersistentList.create(cljparents), cljhyps));
	}
	
	public static void setCurrentContext(Context c){
		Controller.contexts_set_current_context(c.getClojureContext());
	}
	
	/////////////////////////
	/// sneps3.core.build ///
	/////////////////////////
	
	public static void addToContext(Term term, Context c){
		Controller.build_add_to_context(c.getClojureContext(), term.getClojureTerm());
	}
	
	public static Set<Term> find(String pattern){
		Set<Term> results = new HashSet<Term>();
		IPersistentMap clores = Controller.build_find((ISeq)RT.readString(pattern));
		for(Iterator<MapEntry> itr = clores.iterator(); itr.hasNext(); ){
			results.add(Term.create((IPersistentMap)itr.next().key()));
		}
		
		return results;
	}
	
	//////////////////////
	/// sneps3-printer ///
	//////////////////////
	
	public static void writeKBToTextFile(String fname, String headerfname){
		Controller.sneps3_printer_print_kb_to_text_file(fname, headerfname);
	}
	
	public static void writeKBToTextFile(String fname){
		Controller.sneps3_printer_print_kb_to_text_file(fname);
	}
	
}
