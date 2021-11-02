package csneps.gui.business;

import java.util.*;

import clojure.lang.IPersistentMap;
import clojure.lang.IPersistentVector;
import clojure.lang.ISeq;
import clojure.lang.Keyword;
import clojure.lang.MapEntry;
import clojure.lang.PersistentVector;
import csneps.api.IContext;
import csneps.gui.GUI2;

/**
 * Java wrapper for CSNePS Contexts.
 * 
 * @author Daniel R. Schlegel
 */
public class Context implements Comparable<Context>, IContext {

	private static final HashMap<String, Context> contexts = new HashMap<String, Context>();

	private static final Keyword name_key = Keyword.intern("name");
	private static final Keyword parents_key = Keyword.intern("parents");

	private static Context currentContext;

	private final IPersistentMap context;

	private Context(IPersistentMap context) {
		this.context = context;
	}

	public static Context create(IPersistentMap context) {
		Context c = new Context(context);
		if (contexts.get(c.getName()) != null) {
			return contexts.get(c.getName());
		} else {
			contexts.put(c.getName(), c);
			if (GUI2.DEBUG)
				System.err.println("Created context: " + c.getName());
			return c;
		}
	}

	@SuppressWarnings("unchecked")
	public static ArrayList<Context> createContexts(IPersistentMap cljcts) {
		ArrayList<Context> cts = new ArrayList<Context>();
		for (Iterator<MapEntry> itr = cljcts.iterator(); itr.hasNext();) {
			cts.add(create((IPersistentMap) itr.next().getValue()));
		}
		return cts;
	}

	public static void clearContexts() {
		contexts.clear();
		currentContext = null;
	}

	public static Context getContext(String name) {
		return contexts.get(name);
	}

	public static Collection<Context> getContexts() {
		return contexts.values();
	}

	public static Context getCurrentContext() {
		return currentContext;
	}

	public static void setCurrentContext(Context c) {
		currentContext = c;
		for (Term t : Term.getTerms()) {
			t.resetAsserted();
		}
	}

	public String getName() {
		return context.valAt(name_key).toString();
	}

	public List<Context> getParents() {
		List<Context> p = new ArrayList<Context>();
		IPersistentVector v = PersistentVector.create((ISeq) context.valAt(parents_key));
		for (int i = 0; i < v.length(); i++) {
			if (v.nth(i) != null) // Vector created from empty list contains nil.
				p.add(create((IPersistentMap) v.nth(i)));
		}
		return p;
	}

	// This relies in calls into the CSNePS implementation since getting the hyps in
	// a context
	// requires checking parent contexts as well, and I didn't want to reimplement
	// that here.
	// There may be some cues we can use for caching based on the upated hyps call
	// which does
	// happen from gui.clj.
	public Set<Term> getHyps() {
		return FnInterop.hyps(this);
	}

	IPersistentMap getClojureContext() {
		return context;
	}

	@Override
	public String toString() {
		return getName();
	}

	@Override
	public int compareTo(Context arg0) {
		return this.toString().compareTo(arg0.toString());
	}
}
