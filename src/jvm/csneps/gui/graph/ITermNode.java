package csneps.gui.graph;

import java.util.ArrayList;

import csneps.gui.business.Caseframe;
import csneps.gui.business.Term;

public interface ITermNode<E extends IEdge> {

	public Term getTerm();

	public ArrayList<E> getInEdges();

	public void addInEdge(E e);

	public ArrayList<E> getOutEdges();

	public void addOutEdge(E e);

	public boolean isVisible();
	
	public boolean inCollapsedForm();

	public void show();

	public void hide();

	public int getDownCablesetVisibleCount();

	public boolean isDownCablesetVisible();

	public boolean isDownCablesetPartialVisible();

	public int getUpCablesetVisibleCount();

	public boolean isUpCablesetVisible();

	public boolean isUpCablesetPartialVisible();

	public ArrayList<E> getInEdgesMinusFS();

	public ArrayList<Caseframe> getRelationsPartOf();
	
	public CollapsedEdge getCollapsedEdge();

}