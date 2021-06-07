package csneps.api;

import csneps.gui.business.Slot;

import java.util.List;
import java.util.Set;

public interface ICaseframe {
    Set<String> getFSymbols();
    String getName();
    List<Slot> getSlots();
    ISemanticType getType();
}
