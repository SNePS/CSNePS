package edu.buffalo.cse.sneps3.gui.graph;

public class RestrictionEdge extends Edge{
    public RestrictionEdge(ITermNode<IEdge> start, ITermNode<IEdge> end){
        super("restriction",start,end);
    }
}
