package csneps.gui.graph;

import csneps.gui.business.Slot;
import csneps.gui.business.Term;

public class Edge implements IEdge{

	private ITermNode<IEdge> from;
	private ITermNode<IEdge> to;
	private Slot slot;
	private String relationName;
	
	public Edge(Slot slot, ITermNode<IEdge> from, ITermNode<IEdge> to) {
        this.from = from;
        this.to = to;
        this.slot = slot;
        this.relationName = slot.getName();
	}
	
	Edge(String relation, ITermNode<IEdge> from, ITermNode<IEdge> to) {
        this.from = from;
        this.to = to;
        this.relationName = relation;
	}
	
	/* (non-Javadoc)
	 * @see csneps.gui.graph.IEdge#getFrom()
	 */
	@Override
	public ITermNode<IEdge> getFrom(){
		return from;
	}
	
	/* (non-Javadoc)
	 * @see csneps.gui.graph.IEdge#getTo()
	 */
	@Override
	public ITermNode<IEdge> getTo(){
		return to;
	}
	
	public Slot getRelation(){
		return slot;
	}
	
	/* (non-Javadoc)
	 * @see csneps.gui.graph.IEdge#getRelation()
	 */
	@Override
	public String getRelationName(){
		return relationName;
	}
	
	public String toString(){
		if (slot == Slot.getSlot("andorargs")){
			String type = this.getFrom().getTerm().getSyntacticType();
			if(type.equals("Param2op") || type.equals("Andor")) 
				return relationName + " (" + this.getFrom().getTerm().getMin() + ", " + this.getFrom().getTerm().getMax() +")";
			return type.toLowerCase();
		}
		if (slot == Slot.getSlot("threshargs")){
			String type = this.getFrom().getTerm().getSyntacticType();
			if(type.equals("Param2op") || type.equals("Thresh")) 
				return relationName + " (" + this.getFrom().getTerm().getMin() + ", " + this.getFrom().getTerm().getMax() +")";
			return type.toLowerCase();
		}
		if (slot == Slot.getSlot("nor")){
			Term wft = this.getFrom().getTerm();
			if(wft.getDownCableset().get(slot).size() == 1) return "not";
		}
		
		
		return relationName;
	}
	
    @Override
    public int hashCode() {
        int hash = 7;
        hash = 89 * hash + (this.relationName != null ? this.relationName.hashCode() : 0);
        hash = 89 * hash + (this.from != null ? this.from.hashCode() : 0);
        hash = 89 * hash + (this.from != null ? this.from.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final Edge other = (Edge)obj;

        if(this.from.equals(other.from) && this.to.equals(other.to) && this.relationName.equals(other.relationName)) return true;

        return false;
    }
}
