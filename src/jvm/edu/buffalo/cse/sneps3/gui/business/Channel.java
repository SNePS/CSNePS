package edu.buffalo.cse.sneps3.gui.business;

import clojure.lang.APersistentSet;
import clojure.lang.IPersistentMap;
import clojure.lang.Keyword;
import clojure.lang.Ref;

public class Channel {

	private static Keyword originator = Keyword.intern("originator");
	private static Keyword destination = Keyword.intern("destination");
	private static Keyword waiting_msgs = Keyword.intern("waiting-msgs");
	private static Keyword valve_open = Keyword.intern("valve-open");
	
	private IPersistentMap channel;
	
	private Channel(IPersistentMap channel){
		this.channel = channel;
	}
	
	public static Channel create(IPersistentMap channel){
		Channel c = new Channel(channel);
		return c;
	}
	
	public Term originator(){
		IPersistentMap orig = (IPersistentMap)channel.valAt(originator);
		return Term.create(orig);
	}
	
	public Term destination(){
		IPersistentMap dest = (IPersistentMap)channel.valAt(destination);
		return Term.create(dest);
	}
	
	public Boolean isValveOpen(){
		return (Boolean)((Ref)channel.valAt(valve_open)).deref();
	}
	
	public Integer waitingMsgCount(){
		APersistentSet msgset = (APersistentSet)((Ref)channel.valAt(waiting_msgs)).deref();
		return msgset.count();
	}
	
	public boolean equals(Channel channel2){
		return (originator().equals(channel2.originator()) &&
				destination().equals(channel2.destination()));
	}
	
}
