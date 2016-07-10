package edu.buffalo.cse.sneps3.gui.graph;

public class DependencyEdge extends Edge{
    public DependencyEdge(ITermNode<IEdge> start, ITermNode<IEdge> end){
        super("dependency",start,end);
    }
}
