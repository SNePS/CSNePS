package csneps.gui.business;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

import clojure.lang.*;
import csneps.gui.dataaccess.Controller;

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

	public static void adoptRule(Term rule) {
		Controller.snuser_adopt_rule(Symbol.intern(rule.getName()));
	}
	
	public static void clearkb(Boolean clearall){
		Controller.snuser_clearkb(clearall);
	}
	
	public static Term assertTerm(String expr){
		Object read = RT.readString(expr);
		if (read instanceof PersistentList)
			return Term.create(Controller.snuser_assert((PersistentList) read));
		else if (read instanceof Symbol)
			return Term.create(Controller.snuser_assert((Symbol) read));
		return null;
	}

	// TODO add something like this.
//	public static void assertTerm(Term term){
//		Controller.snuser_assert(term.getClojureTerm());
//	}

	// This is called somewhere but maybe shouldn't be?
	public static Term assertETerm(String expr){
		// Not implemented in CSNePS.
		return null;
	}
	
	public static Term defineTerm(String expr){
		return Term.create(Controller.snuser_define_term(RT.readString(expr)));
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
	
	public static Caseframe defineCaseframe(String type, ArrayList<Slot> slots, ArrayList<String> fsymbols){
		ArrayList<Symbol> slotsyms = new ArrayList<Symbol>();
	    for(Slot s : slots)
	    	slotsyms.add(Symbol.intern(s.getName()));
	    ArrayList<Object> fsyms = new ArrayList<Object>();
	    for(String s : fsymbols)
	    	fsyms.add(RT.readString(s));
	    
	    return Caseframe.create(Controller.snuser_define_caseframe(Keyword.intern(type), PersistentList.create(slotsyms), PersistentList.create(fsyms)));
	}

	public static void unadoptRule(Term rule) {
		Controller.snuser_unadopt_rule(Symbol.intern(rule.getName()));
	}
	
	///////////////////
	/// csneps.core ///
	///////////////////
	
	public static SemanticType defineType(String newtype, ArrayList<SemanticType> parents){
		ArrayList<String> parentstrings = new ArrayList<String>();
		ArrayList<Symbol> parentsyms = new ArrayList<Symbol>();
		
		for(SemanticType t : parents){
			parentstrings.add(t.getName());
			parentsyms.add(Symbol.intern(t.getName()));
		}
		
		Controller.csneps_core_define_type(Symbol.intern(newtype), PersistentList.create(parentsyms));
		return SemanticType.create(newtype, parentstrings);
	}

	public static Set<Term> partOfTerms(Term term){
		Set<Term> result = new HashSet<>();
		ISeq terms = Controller.csneps_core_part_of_terms(term.getClojureTerm()).seq();

		while(terms != null && terms.count() > 0){
			IPersistentMap t = (IPersistentMap) terms.first();
			result.add(Term.create(t));
			terms = terms.next();
		}

		return result;
	}
	
	// TODO: Investigate whether these should be written in a similar way to analytic/generic below.
	public static Boolean molecularTermQ(IPersistentMap term){
		return Controller.csneps_core_molecular_term_qmark(term);
	}
	
	public static Boolean variableTermQ(IPersistentMap term){
		return Controller.csneps_core_variable_term_qmark(term);
	}

	public static boolean analyticTermQ(Term term) {
		Boolean result = Controller.csneps_core_analytic_term_qmark(term.getClojureTerm());
		if (result == null) return false;
		return result;
	}

	public static Boolean genericTermQ(Term term) {
		Boolean result = Controller.csneps_core_generic_term_qmark(term.getClojureTerm());
		if (result == null) return false;
		return result;
	}

	public static String getSemanticTypeOf(Term term){
		Keyword semtype = Controller.csneps_core_semantic_types_semantic_type_of(term.getClojureTerm());
		return semtype.getName();
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

	public static String getDescription(IPersistentMap term){
		return Controller.caseframes_description(term);
	}

	public static Caseframe findFrame(String sym) {
		return Caseframe.create(Controller.caseframes_find_frame(Symbol.intern(sym)));
	}

	public static void sameFrame(String newFrame, String oldFrame) {
		Controller.caseframes_sameframe(Symbol.intern(newFrame), Symbol.intern(oldFrame));
	}
	
	///////////////
	/// printer ///
	///////////////
	
	public static String termString(IPersistentMap term){
		return Controller.csneps_core_printer_term_printer(term);
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
	
	public static Boolean isAsserted(Term t, Context c){
		return isAsserted(t, c, false);
	}
	
	public static Boolean isAsserted(Term t, Context c, Boolean local){
		IPersistentMap a = Controller.contexts_asserted_qmark(t.getClojureTerm(), c.getClojureContext(), local);
		if (a != null) return true;
		return false;
	}
	
	public static Boolean isOntologyTerm(Term t) {
		return Controller.contexts_ontology_term_qmark(t.getClojureTerm());
	}
	
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

	public static Context getCurrentContext(){
		return Context.create(Controller.contexts_current_context());
	}
	
	public static Set<Term> hyps(Context c){
		Set<Term> hyps = new HashSet<Term>();
		ISeq clhyps = Controller.contexts_hyps(c.getClojureContext()).seq();
		
		while(clhyps != null && clhyps.count() > 0){
			hyps.add(Term.getTerm(clhyps.first().toString()));
			clhyps = clhyps.next();
		}
		
		return hyps;
	}
	
	/////////////////////////
	/// csneps.core.build ///
	/////////////////////////
	
	public static void addToContext(Term term, Context c){
		Controller.build_add_to_context(c.getClojureContext(), term.getClojureTerm());
	}
	
	public static Set<Term> find(String pattern){
		Set<Term> results = new HashSet<Term>();
		// Seq of vector pairs - ([wft1!: (Isa x y) {?q y}] [wft2!: (Isa x z) {?q z}])
		ISeq clores = Controller.build_find((ISeq)RT.readString(pattern));
		
		while(clores != null && clores.count() > 0){
			IPersistentVector item = (IPersistentVector)clores.first();
			results.add(Term.create((IPersistentMap)item.nth(0)));
			clores = clores.next();
			//System.out.println(results);
		}
		
		return results;
	}
	
	public static String getTermPredicate(Term term){
		return Controller.build_term_predicate(term.getClojureTerm());
	}
	
	public static void unassertTerm(Term term){
		Controller.build_unassert(term.getClojureTerm());
	}
	
	///////////////////////////
	/// csneps.core.printer ///
	///////////////////////////
	
	public static void writeKBToTextFile(String fname, String headerfname){
		Controller.csneps_printer_print_kb_to_text_file(fname, headerfname);
	}
	
	public static void writeKBToTextFile(String fname){
		Controller.csneps_printer_print_kb_to_text_file(fname);
	}

	///////////////
	/// Clojure ///
	///////////////

	public static void loadString(String string) {
		Controller.clojure_load_string(string);
	}
	
}
