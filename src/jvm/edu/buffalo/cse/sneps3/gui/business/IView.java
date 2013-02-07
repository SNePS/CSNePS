package edu.buffalo.cse.sneps3.gui.business;

import java.util.ArrayList;
import java.util.Collection;


public interface IView {
    //Context updates
    public void ctUpdate(ArrayList<Context> c, Boolean clear);
    //Current context
    public void ctCurrent(Context c);
    //SemanticType updates
    public void stUpdate(Collection<SemanticType> v, Boolean clear);
    //Caseframe updates
    public void cfUpdate(Collection<Caseframe> cf, boolean clear);
    //Slot updates
    public void slotUpdate(Collection<Slot> slot, Boolean clear);
    //Term updates
    public void termUpdate(Collection<Term> term, Boolean clear);
}
