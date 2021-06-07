package csneps.api;

import csneps.gui.business.SemanticType;

import java.util.List;
import java.util.Set;

public interface ISemanticType {
    Set<SemanticType> getAncestors();
    String getName();
    List<SemanticType> getParents();
    boolean hasAncestor(SemanticType p);
    boolean hasParent(SemanticType p);
}
