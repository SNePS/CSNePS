package csneps.api;

import csneps.gui.business.SemanticType;
import csneps.gui.business.Slot;
import csneps.gui.business.Term;

import java.util.*;

public interface ITerm {
    String getName();
    String getSyntacticType();
    SemanticType getSemanticType();
    ICaseframe getCaseframe();
    Set<Term> getDependencies();
    String getDescription();
    String getFSymbol();
    String getVarLabel();
    Set<Term> getRestrictionset();
    Map<Slot, Set<Term>> getUpCableset();
    List<Term> getUpCablesetTerms();
    Boolean isMolecular();
    Boolean isVariable();
    boolean isArbitrary();
    boolean isIndefinite();
    boolean isGeneric();
    boolean isAnalytic();
}
