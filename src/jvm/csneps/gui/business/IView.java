package csneps.gui.business;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.Set;


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
    //Channel updates
    public void channelUpdate(Map<String, Set<Channel>> chs, Channel.ChannelType type, Boolean clear);
}
