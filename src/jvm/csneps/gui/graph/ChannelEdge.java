package csneps.gui.graph;

import csneps.gui.business.Channel;

public class ChannelEdge extends Edge {
	
	private Channel.ChannelType type;
	
	public ChannelEdge(String relation, ITermNode<IEdge> from, ITermNode<IEdge> to, Channel.ChannelType type) {
		super(relation, from, to);
		this.type = type;
	}
	
	public Channel.ChannelType getType(){
		return type;
	}
}
