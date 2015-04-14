package edu.buffalo.cse.sneps3.gui.dataaccess;

/* The Controller class is designed to perform all of the interaction
 * from Java to Clojure. From here we make the function calls to 
 * manipulate the data held in the Model. 
 */

import clojure.lang.IPersistentList;
import clojure.lang.IPersistentMap;
import clojure.lang.IPersistentSet;
import clojure.lang.ISeq;
import clojure.lang.PersistentList;
import clojure.lang.Var;
import clojure.lang.Symbol;
import clojure.lang.Keyword;
import clojure.lang.RT;

public class Controller {

	private static Var snuser_assert_fn;
	private static Var snuser_clearkb_fn;
	private static Var snuser_define_term_fn;
	private static Var snuser_define_caseframe_fn;
	private static Var relations_define_slot_fn;
	private static Var sneps3_define_type_fn;
	private static Var build_add_to_context_fn;
	private static Var build_find_fn;
	private static Var build_term_predicate_fn;
	private static Var build_unassert_fn;
	private static Var caseframes_caseframe_name_fn;
	private static Var caseframes_quotedpp_qmark_fn;
	private static Var contexts_define_context_fn;
	private static Var contexts_set_current_context_fn;
	private static Var sneps3_molecular_term_qmark_fn;
	private static Var sneps3_printer_term_printer_fn;
	private static Var sneps3_print_print_kb_to_text_file_fn;
	
	public static IPersistentMap build_find(ISeq pattern){
		if (build_find_fn == null) 
			build_find_fn = RT.var("csneps.core.build", "find");
		try {
			return (IPersistentMap)build_find_fn.invoke(pattern);
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}
	
	public static String build_term_predicate(IPersistentMap term){
		if (build_term_predicate_fn == null) 
			build_term_predicate_fn = RT.var("csneps.core.build", "term-predicate");
		try {
			return build_term_predicate_fn.invoke(term).toString();
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}
	
	public static void build_unassert(IPersistentMap term){
		if (build_unassert_fn == null)
			build_unassert_fn = RT.var("csneps.core.build", "unassert");
		try{
			build_unassert_fn.invoke(term);
		} catch (Exception e) {e.printStackTrace();}
	}
	
	public static void build_add_to_context(IPersistentMap context, IPersistentMap term){
		if (build_add_to_context_fn == null)
			build_add_to_context_fn = RT.var("csneps.core.build", "add-to-context");
		try{
			build_add_to_context_fn.invoke(term, context);
		} catch (Exception e) {e.printStackTrace();}
	}
	
	public static IPersistentMap contexts_define_context(Symbol name, IPersistentList parents, IPersistentSet hyps){
		if (contexts_define_context_fn == null)
			contexts_define_context_fn = RT.var("csneps.core.contexts", "defineContext");
		try{
			return (IPersistentMap)contexts_define_context_fn.invoke(name, Keyword.intern("parents"), parents, Keyword.intern("hyps"), hyps);
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}
	
	public static void contexts_set_current_context(IPersistentMap context){
		if (contexts_set_current_context_fn == null) 
			contexts_set_current_context_fn = RT.var("csneps.core.contexts", "setCurrentContext");
		try{
			contexts_set_current_context_fn.invoke(context);
		} catch (Exception e) {e.printStackTrace();}
	}
	
	public static void sneps3_printer_print_kb_to_text_file(String fname){
		if (sneps3_print_print_kb_to_text_file_fn == null)
			sneps3_print_print_kb_to_text_file_fn = RT.var("csneps.core.printer", "writeKBToTextFile");
		try{
			sneps3_print_print_kb_to_text_file_fn.invoke(fname);
		} catch (Exception e) {e.printStackTrace();}
	}
	
	public static void sneps3_printer_print_kb_to_text_file(String fname, String header){
		if (sneps3_print_print_kb_to_text_file_fn == null)
			sneps3_print_print_kb_to_text_file_fn = RT.var("csneps.core.printer", "writeKBToTextFile");
		try{
			sneps3_print_print_kb_to_text_file_fn.invoke(fname, header);
		} catch (Exception e) {e.printStackTrace();}
	}
	
	public static void snuser_clearkb(Boolean clearall){
		if (snuser_clearkb_fn == null)
			snuser_clearkb_fn = RT.var("csneps.core.snuser", "clearkb");
		try{
			snuser_clearkb_fn.invoke(clearall);
		} catch (Exception e) {e.printStackTrace();}
	}
	
	public static IPersistentMap snuser_define_term(PersistentList expr){
		if (snuser_define_term_fn == null)
			snuser_define_term_fn = RT.var("csneps.core.snuser", "defineTerm");
		try{
			return (IPersistentMap)snuser_define_term_fn.invoke(expr);
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}
	
	public static IPersistentMap snuser_define_term(PersistentList expr, Keyword type){
		if (snuser_define_term_fn == null)
			snuser_define_term_fn = RT.var("csneps.core.snuser", "defineTerm");
		try{
			return (IPersistentMap)snuser_define_term_fn.invoke(expr, type);
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}
	
	public static IPersistentMap snuser_assert(PersistentList expr){
		if (snuser_assert_fn == null)
			snuser_assert_fn = RT.var("csneps.core.snuser", "assert");
		try{
			return (IPersistentMap)snuser_assert_fn.invoke(expr);
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}
	
	public static IPersistentMap snuser_define_caseframe(Keyword type, IPersistentList slots){
		if (snuser_define_caseframe_fn == null)
			snuser_define_caseframe_fn = RT.var("csneps.core.snuser", "defineCaseframe");
		try{
			return (IPersistentMap)snuser_define_caseframe_fn.invoke(type, slots);
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}
	
	public static IPersistentMap snuser_define_caseframe(Keyword type, IPersistentList slots, IPersistentList fsymbols){
		if (snuser_define_caseframe_fn == null)
			snuser_define_caseframe_fn = RT.var("csneps.core.snuser", "defineCaseframe");
		try{
			return (IPersistentMap)snuser_define_caseframe_fn.invoke(type, slots, Keyword.intern("fsymbols"), fsymbols);
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}
	
	public static IPersistentMap relations_define_slot(Symbol name, Keyword type, Integer min, Integer max, Symbol posadjust, Symbol negadjust){
		if (relations_define_slot_fn == null)
			relations_define_slot_fn = RT.var("csneps.core.relations", "define-slot");
		try{
			return (IPersistentMap)relations_define_slot_fn.invoke(name, Keyword.intern("type"), type, Keyword.intern("pos"), min,
					Keyword.intern("neg"), max, Keyword.intern("posadjust"), posadjust, Keyword.intern("negadjust"), negadjust);
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}
	
	public static String caseframes_caseframe_name(IPersistentMap cf){
		if (caseframes_caseframe_name_fn == null)
			caseframes_caseframe_name_fn = RT.var("csneps.core.caseframes", "caseframe-name");
		try{
			return caseframes_caseframe_name_fn.invoke(cf).toString();
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}
	
	public static Boolean caseframes_quotedpp_qmark(IPersistentMap cf){
		if (caseframes_quotedpp_qmark_fn == null)
			caseframes_quotedpp_qmark_fn = RT.var("csneps.core.caseframes", "quotedpp?");
		try{
			return (Boolean)caseframes_quotedpp_qmark_fn.invoke(cf);
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}
	
	public static Boolean sneps3_molecular_term_qmark(IPersistentMap term){
		if (sneps3_molecular_term_qmark_fn == null)
			sneps3_molecular_term_qmark_fn = RT.var("csneps.core", "molecularTerm?");
		try{
			return (Boolean)sneps3_molecular_term_qmark_fn.invoke(term);
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}
	
	public static void sneps3_define_type(Symbol newtype, IPersistentList parents){
		if(sneps3_define_type_fn == null)
			sneps3_define_type_fn = RT.var("csneps.core", "define-type");
		try{
			sneps3_define_type_fn.invoke(newtype, parents);
		} catch (Exception e) {e.printStackTrace();}
	}
	
	public static String sneps3_printer_term_printer(IPersistentMap term){
		if(sneps3_printer_term_printer_fn == null)
			sneps3_printer_term_printer_fn = RT.var("csneps.core.printer", "print-term");
		try{ 
			return sneps3_printer_term_printer_fn.invoke(term).toString();
		} catch (Exception e) {e.printStackTrace();}
		return null;
	}
}
