package edu.buffalo.cse.sneps3.gui.graph;

import edu.buffalo.cse.sneps3.gui.business.Slot;

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
	 * @see edu.buffalo.cse.sneps3.gui.graph.IEdge#getFrom()
	 */
	@Override
	public ITermNode<IEdge> getFrom(){
		return from;
	}
	
	/* (non-Javadoc)
	 * @see edu.buffalo.cse.sneps3.gui.graph.IEdge#getTo()
	 */
	@Override
	public ITermNode<IEdge> getTo(){
		return to;
	}
	
	public Slot getRelation(){
		return slot;
	}
	
	/* (non-Javadoc)
	 * @see edu.buffalo.cse.sneps3.gui.graph.IEdge#getRelation()
	 */
	@Override
	public String getRelationName(){
		return relationName;
	}
	
	public String toString(){
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
