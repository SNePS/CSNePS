package csneps.api;

import csneps.gui.business.Context;
import csneps.gui.business.Term;

import java.util.List;
import java.util.Set;

public interface IContext {
    Set<Term> getHyps();
    String getName();
    List<Context> getParents();
}
