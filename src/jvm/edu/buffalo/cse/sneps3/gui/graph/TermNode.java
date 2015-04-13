package edu.buffalo.cse.sneps3.gui.graph;

import java.util.ArrayList;

import edu.buffalo.cse.sneps3.gui.GUI2;
import edu.buffalo.cse.sneps3.gui.business.Caseframe;
import edu.buffalo.cse.sneps3.gui.business.SemanticType;
import edu.buffalo.cse.sneps3.gui.business.Slot;
import edu.buffalo.cse.sneps3.gui.business.Term;

public class TermNode<E extends IEdge> implements ITermNode<E> {

	private Term term;
	private ArrayList<E> inEdges = new ArrayList<E>();
	private ArrayList<E> outEdges = new ArrayList<E>();
	
	private boolean visible = false;
	
	public TermNode(Term term){
		this.term = term;
	}
	
	/* (non-Javadoc)
	 * @see edu.buffalo.cse.sneps3.gui.graph.ITermNode#getTerm()
	 */
	@Override
	public Term getTerm(){
		return term;
	}

	public String toString(){
        if (term.isAsserted()) {
            return term.getName() + "!";
        }
        return term.getName();
	}
	
	/* (non-Javadoc)
	 * @see edu.buffalo.cse.sneps3.gui.graph.ITermNode#getInEdges()
	 */
	@Override
	public ArrayList<E> getInEdges(){
		return inEdges;
	}
	
	/* (non-Javadoc)
	 * @see edu.buffalo.cse.sneps3.gui.graph.ITermNode#addInEdge(edu.buffalo.cse.sneps3.gui.graph.Edge)
	 */
	@Override
	public void addInEdge(E e){
		inEdges.add(e);
	}
	
	/* (non-Javadoc)
	 * @see edu.buffalo.cse.sneps3.gui.graph.ITermNode#getOutEdges()
	 */
	@Override
	public ArrayList<E> getOutEdges(){
		return outEdges;
	}
	
	/* (non-Javadoc)
	 * @see edu.buffalo.cse.sneps3.gui.graph.ITermNode#addOutEdge(edu.buffalo.cse.sneps3.gui.graph.Edge)
	 */
	@Override
	public void addOutEdge(E e){
		outEdges.add(e);
	}
	
	/* (non-Javadoc)
	 * @see edu.buffalo.cse.sneps3.gui.graph.ITermNode#isVisible()
	 */
	@Override
	public boolean isVisible(){
		return visible;
	}
	
	public boolean inCollapsedForm(){
		return GUI2.getInstance().getGraph().vertexCollapsed(this);
	}
	
	/* (non-Javadoc)
	 * @see edu.buffalo.cse.sneps3.gui.graph.ITermNode#show()
	 */
	@Override
	public void show(){
		visible = true;
	}
	
	/* (non-Javadoc)
	 * @see edu.buffalo.cse.sneps3.gui.graph.ITermNode#hide()
	 */
	@Override
	public void hide(){
		visible = false;
	}
	

	/* (non-Javadoc)
	 * @see edu.buffalo.cse.sneps3.gui.graph.ITermNode#getDownCablesetVisibleCount()
	 */
	@Override
	public int getDownCablesetVisibleCount(){
        int noVis = 0;
        for (IEdge e : outEdges) {
            if (e.getTo().isVisible() || e.getTo().inCollapsedForm()) {
                noVis++;
            }
        }
        return noVis;
	}
	
    /* (non-Javadoc)
	 * @see edu.buffalo.cse.sneps3.gui.graph.ITermNode#isDownCablesetVisible()
	 */
    @Override
	public boolean isDownCablesetVisible() {
        int noVis = getDownCablesetVisibleCount();
        if (noVis == outEdges.size()) {
            return true;
        }
        return false;
    }

    /* (non-Javadoc)
	 * @see edu.buffalo.cse.sneps3.gui.graph.ITermNode#isDownCablesetPartialVisible()
	 */
    @Override
	public boolean isDownCablesetPartialVisible() {
        int noVis = getDownCablesetVisibleCount();
        if (noVis > 0) {
            return true;
        }
        return false;
    }
    
	/* (non-Javadoc)
	 * @see edu.buffalo.cse.sneps3.gui.graph.ITermNode#getUpCablesetVisibleCount()
	 */
	@Override
	public int getUpCablesetVisibleCount(){
        int noVis = 0;
        for (IEdge e : inEdges) {
            if (e.getFrom().isVisible() || e.getFrom().inCollapsedForm()) {
                noVis++;
            }
        }
        return noVis;
	}

    /* (non-Javadoc)
	 * @see edu.buffalo.cse.sneps3.gui.graph.ITermNode#isUpCablesetVisible()
	 */
    @Override
	public boolean isUpCablesetVisible() {
        int noVis = getUpCablesetVisibleCount();
        if (noVis == inEdges.size()) {
            return true;
        }
        return false;
    }

    /* (non-Javadoc)
	 * @see edu.buffalo.cse.sneps3.gui.graph.ITermNode#isUpCablesetPartialVisible()
	 */
    @Override
	public boolean isUpCablesetPartialVisible() {
    	int noVis = getUpCablesetVisibleCount();
        if (noVis > 0) {
            return true;
        }
        return false;
    }
    
    /* (non-Javadoc)
	 * @see edu.buffalo.cse.sneps3.gui.graph.ITermNode#getInEdgesMinusFS()
	 */
    @Override
	public ArrayList<E> getInEdgesMinusFS(){
        ArrayList<E> ret = new ArrayList<E>();
        for (E e : inEdges) {
            if(!e.getFrom().getTerm().isMolecular() || e.getFrom().getTerm().getCaseframe().getFSymbols().isEmpty() ||
                    !e.getFrom().getTerm().getCaseframe().getSlots().get(0).getName().equals(e.toString()) || e.getFrom().isVisible()){
                ret.add(e);
            }
        }
        if(GUI2.DEBUG) System.out.println("UP: " + ret);
        return ret;
    }
    
    /* (non-Javadoc)
	 * @see edu.buffalo.cse.sneps3.gui.graph.ITermNode#getRelationsPartOf()
	 */
    @Override
	public ArrayList<Caseframe> getRelationsPartOf() {
        ArrayList<Caseframe> cfs = new ArrayList<Caseframe>();
        //if (cf != null) {
        //    cfs.add(cf);
        //}
        for (IEdge e : inEdges) {
            if (e.getFrom().getTerm().isMolecular()) {
                cfs.add(e.getFrom().getTerm().getCaseframe());
            }
        }
        for (IEdge e : outEdges) {
            if (e.getTo().getTerm().isMolecular()) {
                cfs.add(e.getTo().getTerm().getCaseframe());
            }
        }
        return cfs;
    }
    
    private ArrayList<ITermNode<IEdge>> getOutNodesForRelation(String relation){
    	ArrayList<ITermNode<IEdge>> relnodes = new ArrayList<ITermNode<IEdge>>();
    	for(IEdge e : getOutEdges()){
    		if(e.getRelation().equals(relation)) relnodes.add(e.getTo());
    	}
    	return relnodes;
    }
    
    /**
     * If it's possible to collapse this node, this will return the edge to display.
     * @return
     */
    @SuppressWarnings("unchecked")
	public CollapsedEdge getCollapsedEdge(){
    	if(getOutEdges().size() == 2
    			&& getInEdges().isEmpty()
    			&& getTerm().getCaseframe().getSlots().size() == 2
    			&& getTerm().getCaseframe().getType().equals(SemanticType.getSemanticType("Propositional"))){
    		Caseframe termcf = getTerm().getCaseframe();
    		return new CollapsedEdge(getTerm().getCaseframe().getName(), 
    				getOutNodesForRelation(termcf.getSlotNames().get(0)).get(0),
    				getOutNodesForRelation(termcf.getSlotNames().get(1)).get(0),
    				(ITermNode<IEdge>)this);
    	}
    	else if (getOutEdges().size() == 3
    			&& getInEdges().isEmpty()
    			&& getTerm().getCaseframe().getType().equals(SemanticType.getSemanticType("Propositional"))
    			&& !getTerm().getCaseframe().getFSymbols().isEmpty()){
    		Caseframe termcf = getTerm().getCaseframe();
    		CollapsedEdge e = new CollapsedEdge(getOutNodesForRelation(termcf.getSlotNames().get(0)).get(0).toString(),
    				getOutNodesForRelation(termcf.getSlotNames().get(1)).get(0),
    				getOutNodesForRelation(termcf.getSlotNames().get(2)).get(0),
    				(ITermNode<IEdge>)this);
    		e.addAdditionalReplacedTerm(getOutNodesForRelation(termcf.getSlotNames().get(0)).get(0));
    		return e;
    	}
    	return null;
    }
}
