package edu.buffalo.cse.sneps3.gui.graph;

public class ChannelEdge extends Edge {

	public enum ChannelType { ICHANNEL, YCHANNEL }
	
	private ChannelType type;
	
	public ChannelEdge(String relation, ITermNode<IEdge> from, ITermNode<IEdge> to, ChannelType type) {
		super(relation, from, to);
		this.type = type;
	}
	
	public ChannelType getType(){
		return type;
	}
}
