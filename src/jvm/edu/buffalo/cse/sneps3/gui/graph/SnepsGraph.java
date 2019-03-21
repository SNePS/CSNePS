package edu.buffalo.cse.sneps3.gui.graph;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

//import org.apache.commons.collections15.Factory;

import com.google.common.base.Supplier;
import edu.buffalo.cse.sneps3.gui.GUI2;
import edu.buffalo.cse.sneps3.gui.business.Caseframe;
import edu.uci.ics.jung.graph.AbstractTypedGraph;
import edu.uci.ics.jung.graph.DirectedGraph;
import edu.uci.ics.jung.graph.MultiGraph;
import edu.uci.ics.jung.graph.util.EdgeType;
import edu.uci.ics.jung.graph.util.Pair;

public class SnepsGraph<V extends ITermNode<E>, E extends IEdge> extends AbstractTypedGraph<V,E> implements DirectedGraph<V,E>, MultiGraph<V,E> {
	private static final long serialVersionUID = 1L;
	
	private HashMap<String, V> vertices;
	private HashSet<E> edges;
	
	private boolean collapsed = false;

	//when things are added, removed, shown, or hidden it will make the graph "dirty."
	private boolean VisibleNodesEdgesDirty;
	Collection<E> visibleEdges;
	Collection<V> visibleVertices; 
	
	private HashMap<V, CollapsedEdge> collapsedVertices;
	
	public SnepsGraph(){
		super(EdgeType.DIRECTED);
		vertices = new HashMap<String, V>();
		edges = new HashSet<E>();
		
		visibleVertices = new HashSet<V>();
		visibleEdges = new HashSet<E>();
		VisibleNodesEdgesDirty = false;
		
		collapsedVertices = new HashMap<V, CollapsedEdge>();
	}


    /**
     * Returns a {@code Factory} that creates an instance of this graph type.
     * @param <V> the vertex type for the graph factory
     * @param <E> the edge type for the graph factory
     */
	public static <V extends ITermNode<E>,E extends IEdge> Supplier<DirectedGraph<V,E>> getFactory() {
		return new Supplier<DirectedGraph<V,E>> () {
			public DirectedGraph<V,E> get() {
				return new SnepsGraph<V,E>();
			}
		};
	}
    
    public Collection<E> getEdges() {
    	if(VisibleNodesEdgesDirty) cleanGraph();
    	
        return Collections.unmodifiableCollection(visibleEdges);
    }

    public Collection<V> getVertices() {
        if(VisibleNodesEdgesDirty) cleanGraph();
    	
        return Collections.unmodifiableCollection(visibleVertices);
    }

    public boolean containsVertex(V vertex) {
    	return (vertices.get(vertex.getTerm().getName())!=null);
    }
    
    public V getVertex(String name){
    	return vertices.get(name);
    }
    
/*    public ITermNode<IEdge> findVertex(String s){
    	return vertices.get(s);
    }*/
    
    public boolean containsEdge(E edge) {
    	return edges.contains(edge);
    }

	protected Collection<E> getIncoming_internal(V vertex)
    {
        return vertices.get(vertex.getTerm().getName()).getInEdges(); 
    }
    
	protected Collection<E> getOutgoing_internal(V vertex)
    {
        return vertices.get(vertex.getTerm().getName()).getOutEdges();
    }
    
    public boolean addVertex(V vertex) {
    	if(vertex == null) {
    		throw new IllegalArgumentException("vertex may not be null");
    	}
        if (!containsVertex(vertex)) {
            vertices.put(vertex.getTerm().getName(), vertex);
        	VisibleNodesEdgesDirty = true;
            return true;
        } 
        else if (!vertex.isVisible()){
        	vertex.show();
        	return true;
        }
        else {
            return false;
        }
    }

    public boolean removeVertex(V vertex) {
        if (!containsVertex(vertex))
            return false;
        
        if(collapsed) expandVertex(vertex);
        
        // copy to avoid concurrent modification in removeEdge
        Set<E> incident = new HashSet<E>(getIncoming_internal(vertex));
        incident.addAll(getOutgoing_internal(vertex));
        
        for (E edge : incident)
            removeEdge(edge);
        
        vertices.remove(vertex.getTerm().getName());
        
        VisibleNodesEdgesDirty = true;
        
        return true;
    }
    
    public boolean hideVertex(V vertex){
    	if (!containsVertex(vertex))
            return false;
    	
    	vertex.hide();

    	//Hide all predecessors since they are molecular.
    	for(E e : getIncoming_internal(vertex)){ 
    		hideVertex((V)e.getFrom());
    	}
    	
    	//If this has been replaced by a collapsed arc, get rid of that.
    	if(collapsed){
    		edges.remove((E)collapsedVertices.get(vertex));
    		collapsedVertices.remove(vertex);
    	}
		
    	
    	VisibleNodesEdgesDirty = true;
    	
    	return true;
    }
    
    public void removeAll(){
		vertices = new HashMap<String, V>();
		edges = new HashSet<E>();
		
		visibleVertices = new HashSet<V>();
		visibleEdges = new HashSet<E>();
		VisibleNodesEdgesDirty = false;
		
		collapsed = false;
		collapsedVertices = new HashMap<V, CollapsedEdge>();
    }
    
    public void hideAll(){
    	for(V v : getVertices())
    		if(v.isVisible()) hideVertex(v);
    }
    
    public void showAll(){
    	for(V v : vertices.values())
    		if(!v.isVisible()) showVertex(v);
    }
    
    public boolean showVertex(V vertex){
    	if (!containsVertex(vertex))
            return false;
    	if(collapsed){
    		showVertex_Collapsed(vertex);
    		return true;
    	}
    	
    	vertex.show();
    	
    	//Show all successors (which will only be there if this is molecular)
    	for(E e : getOutgoing_internal(vertex)){
    		showVertex((V)e.getTo());
    	}
    	
        VisibleNodesEdgesDirty = true;
    	
    	return true;
    }
    
    public boolean removeEdge(E edge) {
        if (!containsEdge(edge))
            return false;
        
        Pair<V> endpoints = this.getEndpoints(edge);
        V source = endpoints.getFirst();
        V dest = endpoints.getSecond();
        
        // remove edge from incident vertices' adjacency sets
        getOutgoing_internal(source).remove(edge);
        getIncoming_internal(dest).remove(edge);
        
        edges.remove(edge);
        return true;
    }
    
    private void cleanGraph(){
    	visibleVertices = getVisibleVertices(vertices.values());
    	visibleEdges = getVisibleEdges(edges);
    	VisibleNodesEdgesDirty = false;
    }
    
    private Collection<V> getVisibleVertices(Collection<V> c){
    	Set<V> visverts = new HashSet<V>();
    	for(V v : c){
    		if(v.isVisible()) visverts.add(v);
    	}
    	return visverts;
    }

    private Collection<E> getVisibleEdges(Collection<E> c){
    	Set<E> visedges = new HashSet<E>();
    	for (E e : c){
    		if(e.getTo().isVisible() && e.getFrom().isVisible()) visedges.add(e);
    	}
    	return visedges;
    }
    
    
    
    public Collection<E> getInEdges(V vertex) {
        if (!containsVertex(vertex))
            return null;
        
        return Collections.unmodifiableCollection(getVisibleEdges(getIncoming_internal(vertex)));
    }

    public Collection<E> getOutEdges(V vertex) {
        if (!containsVertex(vertex))
            return null;
        
        return Collections.unmodifiableCollection(getVisibleEdges(getOutgoing_internal(vertex)));
    }

    public Collection<V> getPredecessors(V vertex) {
        if (!containsVertex(vertex))
            return null;

        Set<V> preds = new HashSet<V>();
        for (E edge : getIncoming_internal(vertex))
        	if(this.getSource(edge).isVisible()) preds.add(this.getSource(edge));
        
        return Collections.unmodifiableCollection(preds);
    }

    public Collection<V> getSuccessors(V vertex) {
        if (!containsVertex(vertex))
            return null;
        
        Set<V> succs = new HashSet<V>();
        for (E edge : getOutgoing_internal(vertex))
        	if(this.getDest(edge).isVisible()) succs.add(this.getDest(edge));
        
        return Collections.unmodifiableCollection(succs);
    }

    public Collection<V> getNeighbors(V vertex) {
        if (!containsVertex(vertex))
            return null;
        
        Collection<V> neighbors = new HashSet<V>();
        for (E edge : getIncoming_internal(vertex))
        	if(this.getSource(edge).isVisible()) neighbors.add(this.getSource(edge));
        for (E edge : getOutgoing_internal(vertex))
        	if(this.getDest(edge).isVisible()) neighbors.add(this.getDest(edge));
        return Collections.unmodifiableCollection(neighbors);
    }

    public Collection<E> getIncidentEdges(V vertex) {
        if (!containsVertex(vertex))
            return null;
        
        Collection<E> incident = new HashSet<E>();
        incident.addAll(getVisibleEdges(getIncoming_internal(vertex)));
        incident.addAll(getVisibleEdges(getOutgoing_internal(vertex)));
        return incident;
    }

    @Override
    public E findEdge(V v1, V v2) {
        if (!containsVertex(v1) || !containsVertex(v2))
            return null;
        for (E edge : getOutgoing_internal(v1))
            if (this.getDest(edge).equals(v2))
                return edge;
        
        return null;
    }
    
    private boolean addEdge_Helper(E edge, Pair<? extends V> endpoints, EdgeType edgeType, boolean metaEdge){
    	this.validateEdgeType(edgeType);
        Pair<V> new_endpoints = getValidatedEndpoints(edge, endpoints);
        if (new_endpoints == null)
            return false;
        
        edges.add(edge);
        
        V source = new_endpoints.getFirst();
        V dest = new_endpoints.getSecond();

        if (!containsVertex(source))
            this.addVertex(source);
        
        if (!containsVertex(dest))
            this.addVertex(dest);
        
        if(!metaEdge){
	        getIncoming_internal(dest).add(edge);
	        getOutgoing_internal(source).add(edge);

	        if(collapsed) collapseMolecule(source);
        }
        
        VisibleNodesEdgesDirty = true;
        
        return true;
    }
    
	@Override
    public boolean addEdge(E edge, Pair<? extends V> endpoints, EdgeType edgeType) 
	{
		return addEdge_Helper(edge, endpoints, edgeType, false);
	}
	
	@SuppressWarnings("unchecked")
	public boolean addRestrictionEdge(RestrictionEdge edge, Pair<? extends V> endpoints){
		return addEdge_Helper((E)edge, endpoints, EdgeType.DIRECTED, true);
	}
	
	@SuppressWarnings("unchecked")
	public boolean addDependencyEdge(DependencyEdge edge, Pair<? extends V> endpoints){
		return addEdge_Helper((E)edge, endpoints, EdgeType.DIRECTED, true);
	}
	
	public boolean addCollapsedEdge(E edge, Pair<? extends V> endpoints, EdgeType edgeType) 
		{
			this.validateEdgeType(edgeType);
	        Pair<V> new_endpoints = getValidatedEndpoints(edge, endpoints);
	        if (new_endpoints == null)
	            return false;
	        
	        edges.add(edge);
	        
	        return true;
		}

    
    public V getSource(E edge) {
        if (!containsEdge(edge))
            return null;
        return (V)edge.getFrom();
    }

    public V getDest(E edge) {
        if (!containsEdge(edge))
            return null;
        return (V)edge.getTo();
    }

    public boolean isSource(V vertex, E edge) {
        if (!containsEdge(edge) || !containsVertex(vertex))
            return false;
        return vertex.equals(this.getEndpoints(edge).getFirst());
    }

    public boolean isDest(V vertex, E edge) {
        if (!containsEdge(edge) || !containsVertex(vertex))
            return false;
        return vertex.equals(this.getEndpoints(edge).getSecond());
    }

    public Pair<V> getEndpoints(E edge) {
        return new Pair<V>((V)edge.getFrom(), (V)edge.getTo());
    }

	public int getEdgeCount() {
		return edges.size();
	}

	public int getVertexCount() {
		return vertices.size();
	}
	
	/***********************************
	 *** Navigating Around The Graph ***
	 ***********************************/
	
	public void showInEdges(V vertex){
		for(E e : getIncoming_internal(vertex))
			showVertex((V)e.getFrom());
	}
	
	public void showInEdges(V vertex, Caseframe relation){
		for(E e : getIncoming_internal(vertex))
			if(e.getFrom().getTerm().getCaseframe() == relation)
				showVertex((V)e.getFrom());
	}
	
	public void showInEdges(V vertex, String relation){
		for(E e : getIncoming_internal(vertex))
			if(e.getFrom().getTerm().getFSymbol().equals(relation))
				showVertex((V)e.getFrom());
	}
	
	public Set<Caseframe> getInHiddenCaseframes(V vertex){
		HashSet<Caseframe> hiddencfs = new HashSet<Caseframe>();
		for(E e : getIncoming_internal(vertex))
			if(!e.getFrom().isVisible() || e.getFrom().inCollapsedForm())
				hiddencfs.add(e.getFrom().getTerm().getCaseframe());
		return hiddencfs;
	}
	
	public Set<String> getInHiddenFSymbols(V vertex){
		HashSet<String> hiddenfsyms = new HashSet<String>();
		for(E e : getIncoming_internal(vertex))
			if(!e.getFrom().isVisible() || e.getFrom().inCollapsedForm())
				hiddenfsyms.add(e.getFrom().getTerm().getFSymbol());
		return hiddenfsyms;
	}
	
	public void hideInEdges(V vertex){
		for(E e : getIncoming_internal(vertex))
			hideVertex((V)e.getFrom());
	}
	
	public void hideInEdges(V vertex, Caseframe relation){
		for(E e : getIncoming_internal(vertex))
			if(e.getFrom().getTerm().getCaseframe() == relation)
				hideVertex((V)e.getFrom());
	}
	
	public void hideInEdges(V vertex, String relation){
		for(E e : getIncoming_internal(vertex))
			if(e.getFrom().getTerm().getFSymbol() == relation)
				hideVertex((V)e.getFrom());
	}
	
	public Set<Caseframe> getInShownCaseframes(V vertex){
		HashSet<Caseframe> showncfs = new HashSet<Caseframe>();
		for(E e : getIncoming_internal(vertex))
			if(e.getFrom().isVisible() && !e.getFrom().inCollapsedForm())
				showncfs.add(e.getFrom().getTerm().getCaseframe());
		return showncfs;
	}
	
	public Set<String> getInShownFSymbols(V vertex){
		HashSet<String> shownfsyms = new HashSet<String>();
		for(E e : getIncoming_internal(vertex))
			if(e.getFrom().isVisible() || !e.getFrom().inCollapsedForm())
				shownfsyms.add(e.getFrom().getTerm().getFSymbol());
		return shownfsyms;
	}
	
	public void showOutEdges(V vertex){
		for(E e : getOutgoing_internal(vertex))
			showVertex((V)e.getTo());
	}
	
	public void showOutEdges(V vertex, Caseframe relation){
		for(E e : getOutgoing_internal(vertex))
			if(e.getTo().getTerm().getCaseframe() == relation)
				showVertex((V)e.getTo());
	}
	
	public Set<Caseframe> getOutHiddenCaseframes(V vertex){
		HashSet<Caseframe> hiddencfs = new HashSet<Caseframe>();
		for(E e : getIncoming_internal(vertex))
			if(!e.getTo().isVisible() || e.getTo().inCollapsedForm())
				hiddencfs.add(e.getTo().getTerm().getCaseframe());
		return hiddencfs;
	}
	
	public void hideOutEdges(V vertex){
		for(E e : getOutgoing_internal(vertex))
			hideVertex((V)e.getTo());
	}
	
	public void hideOutEdges(V vertex, Caseframe relation){
		for(E e : getOutgoing_internal(vertex))
			if(e.getTo().getTerm().getCaseframe() == relation)
				hideVertex((V)e.getTo());
	}
	
	public Set<Caseframe> getOutShownCaseframes(V vertex){
		HashSet<Caseframe> showncfs = new HashSet<Caseframe>();
		for(E e : getIncoming_internal(vertex))
			if(e.getTo().isVisible() && !e.getTo().inCollapsedForm())
				showncfs.add(e.getTo().getTerm().getCaseframe());
		return showncfs;
	}
	
	/***********************************
	 ******** Collapsed Graph **********
	 ***********************************/
	
	public void showVertex_Collapsed(V vertex){
		
		//Show all successors (which will only be there if this is molecular)
    	for(E e : getOutgoing_internal(vertex)){
    		showVertex_Collapsed((V)e.getTo());
    	}
		
		CollapsedEdge coll = vertex.getCollapsedEdge();
		if(coll != null){
			collapsedVertices.put(vertex, coll);
			addCollapsedEdge((E)coll, new Pair<V>((V)coll.getFrom(), (V)coll.getTo()), EdgeType.DIRECTED);
		}
		else vertex.show();
    	
        VisibleNodesEdgesDirty = true;
    }
	
	public boolean collapseVertex(V vertex){
		CollapsedEdge e = vertex.getCollapsedEdge();
		
		//if (GUI2.DEBUG) 
		//	System.out.println("Attempting to collapse: " + vertex + " (" + e + ")");
		
		if(e != null){
			collapsedVertices.put(vertex, e);
			addCollapsedEdge((E)e, new Pair<V>((V)e.getFrom(), (V)e.getTo()), EdgeType.DIRECTED);
			hideVertex(vertex);
			for(ITermNode<IEdge> t : e.getAdditionalReplacedTerms()){
				if(getSuccessors((V)t).isEmpty() && getPredecessors((V)t).isEmpty()){
					t.hide();
					VisibleNodesEdgesDirty = true;
				}
			}
			return true;
		}
		
		return false;
	}
	
	public void collapseMolecule(V molpart){
		if(!collapseVertex(molpart)){
			for(V v : getPredecessors(molpart)){
				collapseVertex(v);
			}
		}
	}

	public void collapseGraph(){
		for(V v : getVertices())
			collapseVertex(v);
		collapsed = true;
	}
	
	public void expandVertex(V vertex){
		boolean oldcollapsedval = collapsed;
		collapsed = false;
		showVertex(vertex);
		removeEdge((E)collapsedVertices.get(vertex));
		collapsedVertices.remove(vertex);
		collapsed = oldcollapsedval;
	}
	
	public void expandGraph(){
		collapsed = false;
		//Making a copy to avoid concurrent modification.
		HashMap<V, CollapsedEdge> cv = new HashMap<V, CollapsedEdge>(collapsedVertices);
		for(V v : cv.keySet())
			expandVertex(v);
	}
	
	public boolean isCollapsed(){
		return collapsed;
	}
	
	public boolean vertexCollapsed(V vertex){
		if(!collapsed) return false;
		return (collapsedVertices.get(vertex) != null);
	}
	
	
}
