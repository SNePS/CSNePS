package csneps.gui.graph;

import java.util.ArrayList;

public class CollapsedEdge extends Edge{

    private ITermNode<IEdge> replacesTerm;
    
    private ArrayList<ITermNode<IEdge>> addlReplacedTerms;

    public CollapsedEdge(String relation, ITermNode<IEdge> start, ITermNode<IEdge> end, ITermNode<IEdge> replacesTerm){
        super(relation,start,end);
        this.replacesTerm = replacesTerm;
        addlReplacedTerms = new ArrayList<ITermNode<IEdge>>();
    }

    public ITermNode<IEdge> getReplacedTerm(){
        return replacesTerm;
    }
    
    void addAdditionalReplacedTerm(ITermNode<IEdge> term){
    	addlReplacedTerms.add(term);
    }
    
    public ArrayList<ITermNode<IEdge>> getAdditionalReplacedTerms(){
    	return addlReplacedTerms;
    }
	
}
