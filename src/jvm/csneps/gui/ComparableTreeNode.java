/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package csneps.gui;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.MutableTreeNode;

/**
 *
 * @author dan
 */
public class ComparableTreeNode<T> extends DefaultMutableTreeNode implements Comparable<ComparableTreeNode<T>>{
	private static final long serialVersionUID = 4091648073735653660L;
	
	public ComparableTreeNode(Comparable<T> c) {
        super(c);
    }
	
	// Essentially does stepwise insertion sort. 
	private void insertInOrder(MutableTreeNode newChild) {
		if(this.children == null) {
			super.insert(newChild, 0);
			return;
		}
		
		for(int i = 0; i < this.children.size(); i++) { 
			String stringrep = this.children.get(i).toString();
			if (stringrep.compareTo(newChild.toString()) >= 0) {
				super.insert(newChild, i);
				return;
			}
		}
		super.insert(newChild, this.children.size() - 1);
	}
	
    @Override
    public void insert(final MutableTreeNode newChild, final int childIndex) {
    		insertInOrder(newChild);
        //super.insert(newChild, childIndex);
        //Collections.sort(this.children);
    }

    @SuppressWarnings("unchecked")
	@Override
	public int compareTo(ComparableTreeNode<T> o) {
		return ((Comparable<T>)this.getUserObject()).compareTo((T)o.getUserObject());
	}
}