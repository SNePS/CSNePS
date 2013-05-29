/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/*
 * SemanticTypesPanel.java
 *
 * Created on Feb 27, 2010, 3:27:49 PM
 */

package edu.buffalo.cse.sneps3.gui;

import edu.buffalo.cse.sneps3.gui.business.Context;
import edu.buffalo.cse.sneps3.gui.business.Slot;
import edu.buffalo.cse.sneps3.gui.business.Caseframe;
import edu.buffalo.cse.sneps3.gui.business.SemanticType;
import edu.buffalo.cse.sneps3.gui.business.Term;

import java.awt.BorderLayout;
import java.util.ArrayList;
import java.util.Vector;
import java.util.Collection;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;

import edu.buffalo.cse.sneps3.gui.business.IView;



/**
 *
 * @author dan
 */
public class SemanticTypesPanel extends javax.swing.JPanel implements IView {

    DefaultTreeModel treeModel;
    DefaultMutableTreeNode rootNode;

    //GUI2 parent;

    SemanticTypeForm stf;

    /** Creates new form SemanticTypesPanel */
    public SemanticTypesPanel() {
        initComponents();

        this.setLayout(new BorderLayout());
        this.removeAll();
        this.add(jScrollPane1, BorderLayout.CENTER);
        this.add(jToolBar1, BorderLayout.SOUTH);

        rootNode = new DefaultMutableTreeNode("Semantic Types");
        treeModel = new DefaultTreeModel(rootNode);
        jTree1.setModel(treeModel);
    }

    public void fixSizes(){
        jSplitPane1.setDividerLocation(this.getHeight()-20);
    }

    public void repopulate(Collection<SemanticType> s){
    	//Entity is the root.
    	ComparableTreeNode entity = new ComparableTreeNode("Entity");
    	rootNode.add(entity);
    	populateChildrenOf(entity, s);
    	
        /*for(SemanticType st : s){
            if(st.getParents().size() == 0){
                //DefaultMutableTreeNode t = new DefaultMutableTreeNode(st.name);
                ComparableTreeNode t = new ComparableTreeNode(st.getName());
                rootNode.add(t);
                populateChildrenOf(t, s);
            }
        }*/
        expandAll(jTree1);
    }

    public void populateChildrenOf(ComparableTreeNode r, Collection<SemanticType> s){
        for(SemanticType st : s){
        	//System.out.println("ST: " + st);
        	//System.out.println("ST Parents: " + st.getParents());
            for(SemanticType p : st.getParents()){
                if(p.getName().equals(r.toString())){
                    //DefaultMutableTreeNode n = new DefaultMutableTreeNode(st.name);
                    ComparableTreeNode n = new ComparableTreeNode(st.getName());
                    r.add(n);
                    populateChildrenOf(n, s);
                }
            }
        }
    }

    public void expandAll(JTree tree) {
        int row = 0;
        while (row < tree.getRowCount()) {
            tree.expandRow(row);
            row++;
        }
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jSplitPane1 = new javax.swing.JSplitPane();
        jPanel1 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTree1 = new javax.swing.JTree();
        jPanel2 = new javax.swing.JPanel();
        jToolBar1 = new javax.swing.JToolBar();
        jButton1 = new javax.swing.JButton();
        jButton2 = new javax.swing.JButton();
        jButton3 = new javax.swing.JButton();

        setMaximumSize(new java.awt.Dimension(250, 32767));
        setPreferredSize(new java.awt.Dimension(250, 300));

        jSplitPane1.setDividerLocation(270);
        jSplitPane1.setDividerSize(0);
        jSplitPane1.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        jSplitPane1.setResizeWeight(1.0);

        jScrollPane1.setViewportView(jTree1);

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 250, Short.MAX_VALUE)
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 270, Short.MAX_VALUE)
        );

        jSplitPane1.setTopComponent(jPanel1);

        jToolBar1.setRollover(true);

        jButton1.setText("+");
        jButton1.setFocusable(false);
        jButton1.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButton1.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });
        jToolBar1.add(jButton1);

        jButton2.setText("-");
        jButton2.setEnabled(false);
        jButton2.setFocusable(false);
        jButton2.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButton2.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar1.add(jButton2);

        jButton3.setText("Modify");
        jButton3.setEnabled(false);
        jButton3.setFocusable(false);
        jButton3.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButton3.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar1.add(jButton3);

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jToolBar1, javax.swing.GroupLayout.DEFAULT_SIZE, 250, Short.MAX_VALUE)
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addComponent(jToolBar1, javax.swing.GroupLayout.PREFERRED_SIZE, 25, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jSplitPane1.setRightComponent(jPanel2);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jSplitPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 250, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jSplitPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 300, Short.MAX_VALUE)
        );
    }// </editor-fold>//GEN-END:initComponents

    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
        stf = new SemanticTypeForm();
        //stf.setParent(GUI2.);
        stf.setVisible(true);
}//GEN-LAST:event_jButton1ActionPerformed


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButton1;
    private javax.swing.JButton jButton2;
    private javax.swing.JButton jButton3;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JToolBar jToolBar1;
    private javax.swing.JTree jTree1;
    // End of variables declaration//GEN-END:variables

    public void ctUpdate(ArrayList<Context> c, Boolean clear) {
        //Do nothing
    }

    public void stUpdate(Collection<SemanticType> v, Boolean clear) {
        rootNode = new DefaultMutableTreeNode("Semantic Types");
        treeModel = new DefaultTreeModel(rootNode);
        jTree1.setModel(treeModel);
        //for(SemanticType s : v){
        //    rootNode.add(new DefaultMutableTreeNode(s));
        //}
        repopulate(v);
        jTree1.repaint();
    }

    public void cfUpdate(Collection<Caseframe> cf, boolean clear) {
    }

    public void slotUpdate(Collection<Slot> slot, Boolean clear) {

    }

    public void ctCurrent(Context c) {
    }

	@Override
	public void termUpdate(Collection<Term> term, Boolean clear) {
		// TODO Auto-generated method stub
		
	}

}