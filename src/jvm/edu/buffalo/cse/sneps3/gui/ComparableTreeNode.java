/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.buffalo.cse.sneps3.gui;

import java.util.Collections;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.MutableTreeNode;

/**
 *
 * @author dan
 */
public class ComparableTreeNode extends DefaultMutableTreeNode implements Comparable {
    public ComparableTreeNode(Comparable c) {
        super(c);
    }
    @Override
    public void insert(final MutableTreeNode newChild, final int childIndex) {
        super.insert(newChild, childIndex);
        Collections.sort(this.children);
    }
    public int compareTo(final Object o) {
        return this.toString().compareToIgnoreCase(o.toString());
    }
}