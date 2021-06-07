package csneps.api;

import csneps.gui.business.Slot;
import csneps.gui.business.Term;

import java.util.*;

public interface ITerm {
    String getName();
    ICaseframe getCaseframe();
    Set<Term> getDependencies();
    String getFSymbol();
    Set<Term> getRestrictionset();
    Map<Slot, Set<Term>> getUpCableset();
    List<Term> getUpCablesetTerms();
    Boolean isMolecular();
    Boolean isVariable();
}
