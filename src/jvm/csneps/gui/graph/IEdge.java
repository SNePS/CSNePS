package csneps.gui.graph;

public interface IEdge {

	public ITermNode<IEdge> getFrom();

	public ITermNode<IEdge> getTo();

	public String getRelationName();

}