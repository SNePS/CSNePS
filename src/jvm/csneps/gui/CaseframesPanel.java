/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/*
 * CaseframesPanel.java
 *
 * Created on Feb 27, 2010, 4:58:54 PM
 */

package csneps.gui;

import csneps.gui.business.*;

import java.awt.BorderLayout;
import java.util.*;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreeNode;

/**
 *
 * @author dan
 */
public class CaseframesPanel extends javax.swing.JPanel implements IView {
	private static final long serialVersionUID = 1L;
	SortedTreeModel treeModel;
    DefaultMutableTreeNode rootNode;
    DefineCaseframeForm cff;

    /** Creates new form CaseframesPanel */
    public CaseframesPanel() {
        initComponents();

        this.setLayout(new BorderLayout());
        this.removeAll();
        this.add(jScrollPane1, BorderLayout.CENTER);
        this.add(jToolBar1, BorderLayout.SOUTH);

        GUI2.model.registerView(this);
        rootNode = new DefaultMutableTreeNode("Caseframes");
        treeModel = new SortedTreeModel(rootNode);
        jTree1.setModel(treeModel);
    }

    public void fixSizes(){
        jSplitPane1.setDividerLocation(this.getHeight()-20);
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jSplitPane1 = new javax.swing.JSplitPane();
        jPanel1 = new javax.swing.JPanel();
        jToolBar1 = new javax.swing.JToolBar();
        jButton_plus = new javax.swing.JButton();
        jButton_minus = new javax.swing.JButton();
        jButton1 = new javax.swing.JButton();
        jButton_details = new javax.swing.JButton();
        jPanel2 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTree1 = new javax.swing.JTree();

        jSplitPane1.setDividerLocation(200);
        jSplitPane1.setDividerSize(0);
        jSplitPane1.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        jSplitPane1.setResizeWeight(1.0);

        jToolBar1.setRollover(true);

        jButton_plus.setText("+");
        jButton_plus.setFocusable(false);
        jButton_plus.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButton_plus.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jButton_plus.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton_plusActionPerformed(evt);
            }
        });
        jToolBar1.add(jButton_plus);

        jButton_minus.setText("-");
        jButton_minus.setEnabled(false);
        jButton_minus.setFocusable(false);
        jButton_minus.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButton_minus.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar1.add(jButton_minus);

        jButton1.setText("Modify");
        jButton1.setEnabled(false);
        jButton1.setFocusable(false);
        jButton1.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButton1.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar1.add(jButton1);

        jButton_details.setText("Details");
        jButton_details.setFocusable(false);
        jButton_details.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButton_details.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jButton_details.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton_detailsActionPerformed(evt);
            }
        });
        jToolBar1.add(jButton_details);

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jToolBar1, javax.swing.GroupLayout.DEFAULT_SIZE, 226, Short.MAX_VALUE)
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addComponent(jToolBar1, javax.swing.GroupLayout.PREFERRED_SIZE, 25, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jSplitPane1.setBottomComponent(jPanel1);

        jScrollPane1.setViewportView(jTree1);

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 226, Short.MAX_VALUE)
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 200, Short.MAX_VALUE)
        );

        jSplitPane1.setLeftComponent(jPanel2);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jSplitPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 226, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jSplitPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 227, Short.MAX_VALUE)
        );
    }// </editor-fold>//GEN-END:initComponents

    private void jButton_plusActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton_plusActionPerformed
        cff = new DefineCaseframeForm();
        cff.setUnselectedSlots(Slot.getSlots());
        cff.setVisible(true);
    }//GEN-LAST:event_jButton_plusActionPerformed

    private void jButton_detailsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton_detailsActionPerformed
        if(jTree1.getSelectionPath() == null || jTree1.getSelectionPath().getLastPathComponent() == treeModel.getRoot()) return;
        Caseframe selectedCaseframe = (Caseframe)((DefaultMutableTreeNode)jTree1.getSelectionPath().getLastPathComponent()).getUserObject();

        List<Slot> selected = selectedCaseframe.getSlots();
        List<Slot> unselected = new ArrayList<Slot>();
        for(Slot s : Slot.getSlots()){
            if(!selected.contains(s)) unselected.add(s);
        }

        cff = new DefineCaseframeForm();
        cff.setUnselectedSlots(unselected);
        cff.setSelectedSlots(selected);
        cff.setCfName(selectedCaseframe.getName());
        cff.setMode(DefineCaseframeForm.Mode.Edit);
        cff.setVisible(true);
    }//GEN-LAST:event_jButton_detailsActionPerformed


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButton1;
    private javax.swing.JButton jButton_details;
    private javax.swing.JButton jButton_minus;
    private javax.swing.JButton jButton_plus;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JToolBar jToolBar1;
    private javax.swing.JTree jTree1;
    // End of variables declaration//GEN-END:variables

    public void ctUpdate(List<Context> c, Boolean clear) {
    }

    public void stUpdate(Collection<SemanticType> v, Boolean clear) {
    }

    public void cfUpdate(Collection<Caseframe> cfc, boolean clear) {
    	if(clear){
	        rootNode = new DefaultMutableTreeNode("Caseframes");
	        treeModel = new SortedTreeModel(rootNode);
    		
	        for(Caseframe c : cfc){
	            DefaultMutableTreeNode n = new DefaultMutableTreeNode(c);
	            treeModel.insertNodeInto(n, rootNode);
	        }
	        jTree1.expandRow(0); //Expand the root node.
	        
	        jTree1.setModel(treeModel);
	        jTree1.repaint();
    	}
    	else{
    		for(Caseframe c : cfc){
	            DefaultMutableTreeNode n = new DefaultMutableTreeNode(c);
	            treeModel.insertNodeInto(n, rootNode);
	        }
    		jTree1.expandRow(0);
    		jTree1.repaint();
    	}
    }

    public void slotUpdate(Collection<Slot> slots, Boolean clear) {
    	if(clear && cff!=null){
    		cff.clearSlots();
    	}
        if(cff!=null) cff.addSlot(slots);
    }

    public void ctCurrent(Context c) {
    }

	public void termUpdate(Collection<Term> term, Boolean clear) {
	}

    @Override
    public void channelUpdate(Map<String, Set<Channel>> chs, Channel.ChannelType ichannel, Boolean clear) {

    }

    class SortedTreeModel extends DefaultTreeModel{
		private static final long serialVersionUID = 1L;

		public SortedTreeModel(TreeNode root) {
			super(root);
		}
		
		public void insertNodeInto(MutableTreeNode newChild, MutableTreeNode parent){
			for (int i = 0; i < parent.getChildCount(); i++){
				if(parent.getChildAt(i).toString().toLowerCase().compareTo(newChild.toString().toLowerCase()) >= 1){
					insertNodeInto(newChild, parent, i);
					return;
				}
			}
			insertNodeInto(newChild, parent, parent.getChildCount());
		}
		
	}

}
