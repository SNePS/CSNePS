package csneps.gui.dataaccess;

/* The Controller class is designed to perform all of the interaction
 * from Java to Clojure. From here we make the function calls to 
 * manipulate the data held in the Model. 
 */

import clojure.java.api.Clojure;

import clojure.lang.*;

public class Controller {
	/// snuser
	private static IFn snuser_adopt_rule_fn, snuser_assert_fn, snuser_clearkb_fn, snuser_define_term_fn,
			snuser_define_caseframe_fn, snuser_load_fn, snuser_unadopt_rule_fn;

	/// slots
	private static IFn relations_define_slot_fn;

	/// csneps
	private static IFn csneps_define_type_fn;

	/// build
	private static IFn build_add_to_context_fn, build_find_fn, build_term_predicate_fn, build_unassert_fn;

	/// caseframes
	private static IFn caseframes_description_fn, caseframes_find_frame_fn, caseframes_caseframe_name_fn,
			caseframes_sameframe_fn, caseframes_quotedpp_qmark_fn;

	private static IFn contexts_asserted_qmark_fn, contexts_ontology_term_qmark_fn, contexts_define_context_fn,
			contexts_hyps_fn, contexts_set_current_context_fn, contexts_current_context_fn;

	private static IFn clojure_core_load_string_fn;

	private static IFn csneps_core_part_of_terms_fn, csneps_core_molecular_term_qmark_fn,
			csneps_core_generic_term_qmark_fn, csneps_core_analytic_term_qmark_fn,
			csneps_core_variable_term_qmark_fn, csneps_core_printer_term_printer_fn;

	private static IFn csneps_core_semantic_types_semantic_type_of_fn;

	private static IFn csneps_print_print_kb_to_text_file_fn;
	private static IFn csneps_gui_start_gui_fn, csneps_gui_add_watches_fn;
	private static IFn snip_pathsfrom_fn;
	
	public static ISeq build_find(ISeq pattern){
		if (build_find_fn == null) 
			build_find_fn = Clojure.var("csneps.core.find", "find");
		try {
			return (ISeq)build_find_fn.invoke(pattern);
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}
	
	public static String build_term_predicate(IPersistentMap term){
		if (build_term_predicate_fn == null) 
			build_term_predicate_fn = Clojure.var("csneps.utils.coreutils", "term-predicate");
		try {
			return build_term_predicate_fn.invoke(term).toString();
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}
	
	public static void build_unassert(IPersistentMap term){
		if (build_unassert_fn == null)
			build_unassert_fn = Clojure.var("csneps.core.build", "unassert");
		try{
			build_unassert_fn.invoke(term);
		} catch (Exception e) {e.printStackTrace();}
	}
	
	public static void build_add_to_context(IPersistentMap context, IPersistentMap term){
		if (build_add_to_context_fn == null)
			build_add_to_context_fn = Clojure.var("csneps.core.build", "add-to-context");
		try{
			build_add_to_context_fn.invoke(term, context);
		} catch (Exception e) {e.printStackTrace();}
	}
	
	public static Boolean contexts_ontology_term_qmark(IPersistentMap term){
		if (contexts_ontology_term_qmark_fn == null) 
			contexts_ontology_term_qmark_fn = Clojure.var("csneps.core.contexts", "ontology-term?");
		try{
			return (contexts_ontology_term_qmark_fn.invoke(term) == Boolean.valueOf(false) ? false : true);
		} catch (Exception e) {e.printStackTrace();}
		return false;
	}
	
	public static IPersistentMap contexts_asserted_qmark(IPersistentMap term, IPersistentMap context, Boolean local){
		if (contexts_asserted_qmark_fn == null) 
			contexts_asserted_qmark_fn = Clojure.var("csneps.core.contexts", "asserted?");
		try{
			return (IPersistentMap)contexts_asserted_qmark_fn.invoke(term, context, Keyword.intern("local"), local);
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}

	public static IPersistentMap contexts_current_context(){
		if (contexts_current_context_fn == null)
			contexts_current_context_fn = Clojure.var("csneps.core.contexts", "currentContext");
		try{
			return (IPersistentMap)contexts_current_context_fn.invoke();
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}
	
	public static IPersistentMap contexts_define_context(Symbol name, IPersistentList parents, IPersistentSet hyps){
		if (contexts_define_context_fn == null)
			contexts_define_context_fn = Clojure.var("csneps.core.contexts", "defineContext");
		try{
			return (IPersistentMap)contexts_define_context_fn.invoke(name, Keyword.intern("parents"), parents, Keyword.intern("hyps"), hyps);
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}
	
	public static IPersistentSet contexts_hyps(IPersistentMap context){
		if (contexts_hyps_fn == null) 
			contexts_hyps_fn = Clojure.var("csneps.core.contexts", "hyps");
		try{
			return (IPersistentSet)contexts_hyps_fn.invoke(context);
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}
	
	public static void contexts_set_current_context(IPersistentMap context){
		if (contexts_set_current_context_fn == null) 
			contexts_set_current_context_fn = Clojure.var("csneps.core.contexts", "setCurrentContext");
		try{
			contexts_set_current_context_fn.invoke(context);
		} catch (Exception e) {e.printStackTrace();}
	}
	
	public static void csneps_printer_print_kb_to_text_file(String fname){
		if (csneps_print_print_kb_to_text_file_fn == null)
			csneps_print_print_kb_to_text_file_fn = Clojure.var("csneps.core.printer", "writeKBToTextFile");
		try{
			csneps_print_print_kb_to_text_file_fn.invoke(fname);
		} catch (Exception e) {e.printStackTrace();}
	}
	
	public static void csneps_printer_print_kb_to_text_file(String fname, String header){
		if (csneps_print_print_kb_to_text_file_fn == null)
			csneps_print_print_kb_to_text_file_fn = Clojure.var("csneps.core.printer", "writeKBToTextFile");
		try{
			csneps_print_print_kb_to_text_file_fn.invoke(fname, header);
		} catch (Exception e) {e.printStackTrace();}
	}

	public static Keyword csneps_core_semantic_types_semantic_type_of(IPersistentMap term){
		if (csneps_core_semantic_types_semantic_type_of_fn == null)
			csneps_core_semantic_types_semantic_type_of_fn = Clojure.var("csneps.core.semantic-types", "semantic-type-of");
		try{
			return (Keyword) csneps_core_semantic_types_semantic_type_of_fn.invoke(term);
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}
	
	public static void snuser_clearkb(Boolean clearall){
		if (snuser_clearkb_fn == null)
			snuser_clearkb_fn = Clojure.var("csneps.core.snuser", "clearkb");
		try{
			snuser_clearkb_fn.invoke(clearall);
		} catch (Exception e) {e.printStackTrace();}
	}
	
	public static IPersistentMap snuser_define_term(Object expr){
		if (snuser_define_term_fn == null)
			snuser_define_term_fn = Clojure.var("csneps.core.snuser", "defineTerm");
		try{
			return (IPersistentMap)snuser_define_term_fn.invoke(expr);
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}
	
	public static IPersistentMap snuser_define_term(PersistentList expr, Keyword type){
		if (snuser_define_term_fn == null)
			snuser_define_term_fn = Clojure.var("csneps.core.snuser", "defineTerm");
		try{
			return (IPersistentMap)snuser_define_term_fn.invoke(expr, type);
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}

	public static void snuser_adopt_rule(Symbol name){
		if (snuser_adopt_rule_fn == null)
			snuser_adopt_rule_fn = Clojure.var("csneps.core.snuser", "adopt-rule");
		try{
			snuser_adopt_rule_fn.invoke(name);
		} catch (Exception e) {e.printStackTrace();}
	}
	
	public static IPersistentMap snuser_assert(PersistentList expr){
		if (snuser_assert_fn == null)
			snuser_assert_fn = Clojure.var("csneps.core.snuser", "assert");
		try{
			return (IPersistentMap)snuser_assert_fn.invoke(expr);
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}

	public static IPersistentMap snuser_assert(Symbol expr){
		if (snuser_assert_fn == null)
			snuser_assert_fn = Clojure.var("csneps.core.snuser", "assert");
		try{
			return (IPersistentMap)snuser_assert_fn.invoke(expr);
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}


	
	public static IPersistentMap snuser_define_caseframe(Keyword type, IPersistentList slots){
		if (snuser_define_caseframe_fn == null)
			snuser_define_caseframe_fn = Clojure.var("csneps.core.snuser", "defineCaseframe");
		try{
			return (IPersistentMap)snuser_define_caseframe_fn.invoke(type, slots);
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}
	
	public static IPersistentMap snuser_define_caseframe(Keyword type, IPersistentList slots, IPersistentList fsymbols){
		if (snuser_define_caseframe_fn == null)
			snuser_define_caseframe_fn = Clojure.var("csneps.core.snuser", "defineCaseframe");
		try{
			return (IPersistentMap)snuser_define_caseframe_fn.invoke(type, slots, Keyword.intern("fsymbols"), fsymbols);
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}

	public static void snuser_unadopt_rule(Symbol name){
		if (snuser_unadopt_rule_fn == null)
			snuser_unadopt_rule_fn = Clojure.var("csneps.core.snuser", "unadopt-rule");
		try{
			snuser_unadopt_rule_fn.invoke(name);
		} catch (Exception e) {e.printStackTrace();}
	}
	
	public static IPersistentMap relations_define_slot(Symbol name, Keyword type, Integer min, Integer max, Symbol posadjust, Symbol negadjust){
		if (relations_define_slot_fn == null)
			relations_define_slot_fn = Clojure.var("csneps.core.relations", "define-slot");
		try{
			return (IPersistentMap)relations_define_slot_fn.invoke(name, Keyword.intern("type"), type, Keyword.intern("min"), min,
					Keyword.intern("max"), max, Keyword.intern("posadjust"), posadjust, Keyword.intern("negadjust"), negadjust);
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}
	
	public static String caseframes_caseframe_name(IPersistentMap cf){
		if (caseframes_caseframe_name_fn == null)
			caseframes_caseframe_name_fn = Clojure.var("csneps.core.caseframes", "caseframe-name");
		try{
			return caseframes_caseframe_name_fn.invoke(cf).toString();
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}

	public static String caseframes_description(IPersistentMap term){
		if (caseframes_description_fn == null)
			caseframes_description_fn = Clojure.var("csneps.core.caseframes", "description");
		return (String) caseframes_description_fn.invoke(term);
	}

	public static void caseframes_sameframe(Symbol newFrame, Symbol oldFrame){
		if (caseframes_sameframe_fn == null)
			caseframes_sameframe_fn = Clojure.var("csneps.core.caseframes", "sameFrame");
		caseframes_sameframe_fn.invoke(newFrame, oldFrame);
	}

	public static IPersistentMap caseframes_find_frame(Symbol sym){
		if (caseframes_find_frame_fn == null)
			caseframes_find_frame_fn = Clojure.var("csneps.core.caseframes", "find-frame");
		return (IPersistentMap) caseframes_find_frame_fn.invoke(sym);
	}
	
	public static Boolean caseframes_quotedpp_qmark(IPersistentMap cf){
		if (caseframes_quotedpp_qmark_fn == null)
			caseframes_quotedpp_qmark_fn = Clojure.var("csneps.core.caseframes", "quotedpp?");
		try{
			return (Boolean)caseframes_quotedpp_qmark_fn.invoke(cf);
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}

	public static Boolean csneps_core_analytic_term_qmark(IPersistentMap term){
		if (csneps_core_analytic_term_qmark_fn == null)
			csneps_core_analytic_term_qmark_fn = Clojure.var("csneps.core", "analyticTerm?");
		try{
			return csneps_core_analytic_term_qmark_fn.invoke(term) instanceof Keyword;
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}

	public static Boolean csneps_core_generic_term_qmark(IPersistentMap term){
		if (csneps_core_generic_term_qmark_fn == null)
			csneps_core_generic_term_qmark_fn = Clojure.var("csneps.core", "genericTerm?");
		try{
			return csneps_core_generic_term_qmark_fn.invoke(term) instanceof Keyword;
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}

	public static Boolean csneps_core_molecular_term_qmark(IPersistentMap term){
		if (csneps_core_molecular_term_qmark_fn == null)
			csneps_core_molecular_term_qmark_fn = Clojure.var("csneps.core", "molecularTerm?");
		try{
			return (Boolean)csneps_core_molecular_term_qmark_fn.invoke(term);
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}
	
	public static Boolean csneps_core_variable_term_qmark(IPersistentMap term){
		if (csneps_core_variable_term_qmark_fn == null)
			csneps_core_variable_term_qmark_fn = Clojure.var("csneps.core", "variableTerm?");
		try{
			return (Boolean)csneps_core_variable_term_qmark_fn.invoke(term);
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}
	
	public static void csneps_core_define_type(Symbol newtype, IPersistentList parents){
		if(csneps_define_type_fn == null)
			csneps_define_type_fn = Clojure.var("csneps.core", "define-type");
		try{
			csneps_define_type_fn.invoke(newtype, parents);
		} catch (Exception e) {e.printStackTrace();}
	}
	
	public static String csneps_core_printer_term_printer(IPersistentMap term){
		if(csneps_core_printer_term_printer_fn == null)
			csneps_core_printer_term_printer_fn = Clojure.var("csneps.core.printer", "term-printer");
		try{ 
			return csneps_core_printer_term_printer_fn.invoke(term).toString();
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}

	////////////
	/// snip ///
	////////////

	public static IPersistentSet snip_pathsfrom(IPersistentMap term, IPersistentList path){
		if (snip_pathsfrom_fn == null)
			snip_pathsfrom_fn = Clojure.var("csneps.snip", "pathsfrom");
		return (IPersistentSet) snip_pathsfrom_fn.invoke(term, path);
	}

	///////////////////
	/// Used by API ///
	///////////////////

	public static IPersistentSet csneps_core_part_of_terms(IPersistentMap term){
		if (csneps_core_part_of_terms_fn == null)
			csneps_core_part_of_terms_fn = Clojure.var("csneps.core", "part-of-terms");
		return (IPersistentSet) csneps_core_part_of_terms_fn.invoke(term);
	}

	public static void gui_startGUI() {
		if (csneps_gui_start_gui_fn == null)
			csneps_gui_start_gui_fn = Clojure.var("csneps.gui", "startGUI");
		csneps_gui_start_gui_fn.invoke();
	}

	public static void gui_startGUI(IPersistentSet terms) {
		if (csneps_gui_start_gui_fn == null)
			csneps_gui_start_gui_fn = Clojure.var("csneps.gui", "startGUI");
		csneps_gui_start_gui_fn.invoke(terms);
	}

	public static void gui_add_watches(Model m) {
		if (csneps_gui_add_watches_fn == null)
			csneps_gui_add_watches_fn = Clojure.var("csneps.gui", "add-watches");
		csneps_gui_add_watches_fn.invoke(m);
	}

	public static void snuser_load(String filename) {
		if (snuser_load_fn == null)
			snuser_load_fn = Clojure.var("csneps.core.snuser", "load");
		snuser_load_fn.invoke(filename);
	}

	public static void clojure_load_string(String string) {
		if (clojure_core_load_string_fn == null)
			clojure_core_load_string_fn = Clojure.var("clojure.core", "load-string");
		clojure_core_load_string_fn.invoke(string);
	}
}
