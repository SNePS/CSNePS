/*
 * SemanticTypesPanel.java
 *
 * Created on Feb 27, 2010, 3:27:49 PM
 */

package edu.buffalo.cse.sneps3.gui;

import edu.buffalo.cse.sneps3.gui.business.Caseframe;
import edu.buffalo.cse.sneps3.gui.business.Context;
import edu.buffalo.cse.sneps3.gui.business.FnInterop;
import edu.buffalo.cse.sneps3.gui.business.IView;
import edu.buffalo.cse.sneps3.gui.business.Slot;
import edu.buffalo.cse.sneps3.gui.business.SemanticType;
import edu.buffalo.cse.sneps3.gui.business.Term;

import java.awt.BorderLayout;
import java.util.ArrayList;
import java.util.Collection;
import javax.swing.JTree;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.text.Position.Bias;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

/**
 * @author Daniel R. Schlegel
 */
public class ContextsPanel extends javax.swing.JPanel implements IView, TreeSelectionListener {

	private static final long serialVersionUID = 327228274883745064L;
	
	DefaultTreeModel treeModel;
    DefaultMutableTreeNode rootNode;

    CreateContextForm ccf;

    /** Creates new ContextsPanel */
    public ContextsPanel() {
        initComponents();

        this.setLayout(new BorderLayout());
        this.removeAll();
        this.add(jScrollPane1, BorderLayout.CENTER);
        this.add(jToolBar1, BorderLayout.SOUTH);
        

        rootNode = new DefaultMutableTreeNode("Contexts");
        treeModel = new DefaultTreeModel(rootNode);
        jTree_contexts.setModel(treeModel);
        jTree_contexts.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        jTree_contexts.addTreeSelectionListener(this);
    }

    public void valueChanged(TreeSelectionEvent e) {
        //Returns the last path element of the selection.
        //This method is useful only when the selection model allows a single selection.
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) jTree_contexts.getLastSelectedPathComponent();

        if (node == null) return; // Nothing is selected.
        
        String ct = node.getUserObject().toString();
        
        if(!ct.equals("Contexts")){
        	FnInterop.setCurrentContext((Context)node.getUserObject());
        }
    }

    public void repopulate(Collection<Context> c){
    	System.out.println("Repopulating.." + c);
        for(Context ct : c){
            if(ct.getParents().isEmpty()){
                ComparableTreeNode t = new ComparableTreeNode(ct);
                rootNode.add(t);
                populateChildrenOf(t, c);  
            }
        }
        expandAll(jTree_contexts);
    }

    public void populateChildrenOf(ComparableTreeNode r, Collection<Context> c){
        for(Context ct : c){
            for(Context p : ct.getParents()){
                if(p.getName().equals(r.toString())){
                    ComparableTreeNode n = new ComparableTreeNode(ct);
                    r.add(n);
                    populateChildrenOf(n, c);
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

    private void initComponents() {

        jSplitPane1 = new javax.swing.JSplitPane();
        jPanel1 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTree_contexts = new javax.swing.JTree();
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

        jScrollPane1.setViewportView(jTree_contexts);

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
    }

    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {
        ccf = new CreateContextForm();
        ccf.setVisible(true);
}

    private javax.swing.JButton jButton1;
    private javax.swing.JButton jButton2;
    private javax.swing.JButton jButton3;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JToolBar jToolBar1;
    private javax.swing.JTree jTree_contexts;

    public void ctUpdate(ArrayList<Context> v, Boolean clear) {
    	if(clear){
    		rootNode = new DefaultMutableTreeNode("Contexts");
            treeModel = new DefaultTreeModel(rootNode);
            jTree_contexts.setModel(treeModel);
    	}
        repopulate(Context.getContexts());
        jTree_contexts.repaint();
    }

    public void stUpdate(Collection<SemanticType> v, Boolean clear) {
        //Do nothing
    }

    public void cfUpdate(Collection<Caseframe> cf, boolean clear) {
    }

    public void slotUpdate(Collection<Slot> slot, Boolean clear) {

    }

    public void ctCurrent(Context c) {
        jTree_contexts.removeTreeSelectionListener(this);
        TreePath path = jTree_contexts.getNextMatch(c.getName(), 0, Bias.Forward);
        jTree_contexts.setSelectionPath(path);
        jTree_contexts.addTreeSelectionListener(this);
    }

	@Override
	public void termUpdate(Collection<Term> term, Boolean clear) {
	}

}
