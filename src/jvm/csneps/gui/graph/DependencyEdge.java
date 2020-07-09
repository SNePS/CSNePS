package csneps.gui.graph;

public class DependencyEdge extends Edge{
    public DependencyEdge(ITermNode<IEdge> start, ITermNode<IEdge> end){
        super("dependency",start,end);
    }
}
