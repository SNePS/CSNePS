/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/*
 * Wishlist:
 * -Add "With forward inference" checkbox.
 *
 */


/*
 * AddToGraph.java
 *
 * Created on Apr 17, 2010, 3:04:00 PM
 */

package csneps.gui;

import csneps.gui.business.Caseframe;
import csneps.gui.business.SemanticType;
import csneps.gui.business.Slot;
import csneps.gui.util.SortedComboBoxModel;

import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Comparator;

import javax.swing.AbstractAction;
import javax.swing.DefaultCellEditor;
import javax.swing.table.DefaultTableModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JPopupMenu;
import javax.swing.JTable;

/**
 *
 * @author dan
 */
public class QBEBasePanel extends javax.swing.JFrame {

    public enum DialogType{ ADD, FIND };

    DefaultTableModel table_model = new DefaultTableModel();
    SortedComboBoxModel cf_model = new SortedComboBoxModel(new Comparator<Caseframe>() {
		public int compare(Caseframe o1, Caseframe o2) {
			return o1.compareTo(o2);
		}});
    DefaultComboBoxModel slot_model = new DefaultComboBoxModel();

    JPopupMenu popup;

    int row = 0;

    int depth = 1;

    private static int totalVarCounter = 0;

    private static String customVarList = "";

    protected boolean fwdInference = false;

    public boolean ignoreMinSlotCount = false;

    /** Creates new form AddToGraph */
    public QBEBasePanel() {
        initComponents();


        jTable1.putClientProperty("terminateEditOnFocusLost", Boolean.TRUE);
        jTable1.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseReleased(MouseEvent e){
                if (e.getButton() == MouseEvent.BUTTON3){
                    row = jTable1.rowAtPoint(e.getPoint());

                    popup = new JPopupMenu();
                    popup.add(new AbstractAction("Add Frame As Filler") {
                        public void actionPerformed(ActionEvent e) {
                            String s = fillWithFrame();
                            if (s != null) {
                                jTable1.getModel().setValueAt(s, row, 1);
                            }
                        }
                    });

                    //Get the slot of the row the mouse is over.
                    Slot slot = (Slot)jTable1.getValueAt(row, 0);
                    int slotCt = 0;
                    //Find out how many of these are in the table.
                    for(int i = 0; i < jTable1.getRowCount(); i++){
                        if(jTable1.getValueAt(i, 0).toString().equals(slot.toString())) slotCt++;
                    }
                    if (slotCt > slot.getMin()) {
                        popup.add(new AbstractAction("Remove Slot Instance") {
                            public void actionPerformed(ActionEvent e) {
                                table_model.removeRow(row);
                            }
                        });
                    }

                    popup.show(e.getComponent(), e.getX(), e.getY());
                }
            }
        });

        for(Caseframe cf : Caseframe.getCaseframes()){
            cf_model.addElement(cf);
        }
        jComboBox_cf.setModel(cf_model);

        jComboBox_slot.setModel(slot_model);

        //This should set up the initial table. Null is fine to pass as the event
        //since we don't actually use it in the handler.
        jComboBox_cfActionPerformed(null);

        //jComboBox_atype.setEnabled(false);

    }

    static protected int getTotalVarCount(){
        return totalVarCounter;
    }

    static protected int incrementTotalVarCountBy(int i){
        return totalVarCounter+=i;
    }

    static protected String getCustomVars(){
        return customVarList;
    }

    static protected String addCustomVar(String newvar){
        return customVarList += " " + newvar;
    }

    static protected void resetState(){
        totalVarCounter = 0;
        customVarList = "";
    }

    protected String performOK(boolean execute){
        return "";
    }

    protected JButton getJBOK(){
        return jButton_OK;
    }

    protected JButton getJBCancel(){
        return jButton_cancel;
    }

    protected JTable getTable(){
        return jTable1;
    }

    protected Caseframe getSelectedCF(){
        return ((Caseframe)jComboBox_cf.getSelectedItem());
    }

    protected void hideAType(){
        jLabel_atype.setVisible(false);
        jComboBox_atype.setVisible(false);
        //jCheckBox_asserted.setVisible(false);
        this.setSize(this.getWidth(), this.getHeight()-30);
    }

    protected void showAType(){
        jLabel_atype.setVisible(true);
        jComboBox_atype.setVisible(true);
    }

    protected void hideCheckbox(){
        jCheckBox_asserted.setVisible(false);
    }

    protected void hideFwdInference(){
        jCheckBox_fwinference.setVisible(false);
    }

    protected JCheckBox getCheckBox(){
        return jCheckBox_asserted;
    }

    protected void hideOKCancel(){
        jButton_OK.setVisible(false);
        jButton_cancel.setVisible(false);
        this.setSize(this.getWidth(), this.getHeight()-40);
    }

    protected void showOKCancel(){
        jButton_OK.setVisible(true);
        jButton_cancel.setVisible(true);
    }

    protected String fillWithFrame(){
        //FrameSlotDialog fsd = new FrameSlotDialog(this, new QBEPanel());
        //fsd.setVisible(true);

        //System.err.println(fsd.getResult());
        //return fsd.getResult();
        return "";
    }

    protected void dialogMode(){
        hideAType();
        hideCheckbox();
        hideFwdInference();
        showOKCancel();
    }

    protected JComboBox getJCBType(){
        return jComboBox_atype;
    }

    protected JCheckBox getJCBAsserted(){
        return jCheckBox_asserted;
    }

    protected JComboBox getJCBCF(){
        return jComboBox_cf;
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jLabel1 = new javax.swing.JLabel();
        jComboBox_cf = new javax.swing.JComboBox();
        jButton_cancel = new javax.swing.JButton();
        jButton_OK = new javax.swing.JButton();
        jPanel1 = new javax.swing.JPanel();
        jLabel2 = new javax.swing.JLabel();
        jComboBox_slot = new javax.swing.JComboBox();
        jButton_add = new javax.swing.JButton();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTable1 = new JTableRE();
        jLabel_atype = new javax.swing.JLabel();
        jComboBox_atype = new javax.swing.JComboBox();
        jCheckBox_asserted = new javax.swing.JCheckBox();
        jCheckBox_fwinference = new javax.swing.JCheckBox();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Add Frame Instance");

        jLabel1.setText("Select Caseframe:");

        jComboBox_cf.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        jComboBox_cf.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jComboBox_cfActionPerformed(evt);
            }
        });

        jButton_cancel.setText("Cancel");
        jButton_cancel.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton_cancelActionPerformed(evt);
            }
        });

        jButton_OK.setText("OK");

        jPanel1.setBorder(javax.swing.BorderFactory.createTitledBorder("Slot Fillers"));

        jLabel2.setText("Add slot instance:");

        jComboBox_slot.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));

        jButton_add.setText("Add");
        jButton_add.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton_addActionPerformed(evt);
            }
        });

        jTable1.setModel(new CaseframeTableModel(
            new String [] {"Slot", "Filler"}, 2));
    jTable1.setRowSelectionAllowed(false);
    jScrollPane1.setViewportView(jTable1);

    javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
    jPanel1.setLayout(jPanel1Layout);
    jPanel1Layout.setHorizontalGroup(
        jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(jPanel1Layout.createSequentialGroup()
            .addContainerGap()
            .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 393, Short.MAX_VALUE)
                .addGroup(jPanel1Layout.createSequentialGroup()
                    .addComponent(jLabel2)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(jComboBox_slot, 0, 186, Short.MAX_VALUE)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(jButton_add)))
            .addContainerGap())
    );
    jPanel1Layout.setVerticalGroup(
        jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(jPanel1Layout.createSequentialGroup()
            .addContainerGap()
            .addComponent(jScrollPane1, javax.swing.GroupLayout.PREFERRED_SIZE, 187, javax.swing.GroupLayout.PREFERRED_SIZE)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
            .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel2)
                .addComponent(jComboBox_slot, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addComponent(jButton_add))
            .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );

    jLabel_atype.setText("Wrap With:");

    jComboBox_atype.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Nothing", "(every ...)", "(some ...)" }));

    jCheckBox_asserted.setSelected(true);
    jCheckBox_asserted.setText("Asserted");
    jCheckBox_asserted.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            jCheckBox_assertedActionPerformed(evt);
        }
    });

    jCheckBox_fwinference.setText("With forward inference");
    jCheckBox_fwinference.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            jCheckBox_fwinferenceActionPerformed(evt);
        }
    });

    javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
    getContentPane().setLayout(layout);
    layout.setHorizontalGroup(
        layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
            .addContainerGap()
            .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                .addComponent(jPanel1, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addGroup(layout.createSequentialGroup()
                    .addComponent(jCheckBox_asserted)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(jCheckBox_fwinference)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jButton_OK, javax.swing.GroupLayout.PREFERRED_SIZE, 60, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(jButton_cancel))
                .addGroup(javax.swing.GroupLayout.Alignment.LEADING, layout.createSequentialGroup()
                    .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addComponent(jLabel1)
                        .addComponent(jLabel_atype))
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addComponent(jComboBox_atype, 0, 286, Short.MAX_VALUE)
                        .addComponent(jComboBox_cf, 0, 286, Short.MAX_VALUE))))
            .addContainerGap())
    );
    layout.setVerticalGroup(
        layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(layout.createSequentialGroup()
            .addContainerGap()
            .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel1)
                .addComponent(jComboBox_cf, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jComboBox_atype, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addComponent(jLabel_atype))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
            .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
            .addGap(18, 18, 18)
            .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jButton_cancel)
                .addComponent(jButton_OK)
                .addComponent(jCheckBox_asserted)
                .addComponent(jCheckBox_fwinference))
            .addContainerGap())
    );

    pack();
    }// </editor-fold>//GEN-END:initComponents

    private void jButton_cancelActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton_cancelActionPerformed
        this.dispose();
    }//GEN-LAST:event_jButton_cancelActionPerformed

    private void jButton_addActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton_addActionPerformed
        if(jComboBox_slot.getSelectedItem() != null){
            //Find the last row which has the slot name we want to add.
            int end = 0;
            for(int i = 0; i < table_model.getRowCount(); i++){
                Object o = table_model.getValueAt(i, 0);
                if((Slot)o == jComboBox_slot.getSelectedItem())
                    end = i;
            }
            table_model.insertRow(++end, new Object[] { jComboBox_slot.getSelectedItem(), null } );
        }
            
            
    }//GEN-LAST:event_jButton_addActionPerformed

    private void jComboBox_cfActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jComboBox_cfActionPerformed
        //When we change the cf we need to populate the table with the min
        //slots needed for the cf. If max != min we need to add to the slots
        //combobox as well. If the cf is not a proposition, grey out the
        //"asserted" checkbox.

        Caseframe cf = (Caseframe)jComboBox_cf.getSelectedItem();
        if(cf == null) return; //This is really more for debug purposes.
        ArrayList<Slot> slots = cf.getSlots();

        if(!cf.getType().getName().equals("Propositional")){
            jCheckBox_asserted.setSelected(false);
            jCheckBox_asserted.setEnabled(false);
        }
        else if (cf.getType().hasAncestor(SemanticType.getSemanticType("Propositional"))){ //Check to make sure it isnt a child of Proposition
        	jCheckBox_asserted.setEnabled(true);
        }

        table_model = new CaseframeTableModel(new String[] {"Slot Name", "Filler"}, 0);
        slot_model = new DefaultComboBoxModel();

            for(Slot s : slots){

                int m = (ignoreMinSlotCount ? 1 : s.getMin().intValue());
                while(m>0){
                    table_model.addRow(new Object[] {s, null});
                    m--;
                }
                if (GUI2.DEBUG) System.err.println("Slot min: " + s.getMin() + " max: " + s.getMax());
                if(s.getMax() == null || s.getMin() < s.getMax()){
                    slot_model.addElement(s);
                }
            }
        jTable1.setModel(table_model);
        
        if(!cf.getFSymbols().isEmpty()){
            RowEditorModel rm = new RowEditorModel();
            jTable1.setRowEditorModel(rm);
            JComboBox cb = new JComboBox(cf.getFSymbols().toArray());
            DefaultCellEditor ed = new DefaultCellEditor(cb);
            rm.addEditorForRow(0, ed);
        }

        //table_model.addRow(new Object[] {"test", null});
        
        jComboBox_slot.setModel(slot_model);
        
    }//GEN-LAST:event_jComboBox_cfActionPerformed

    private void jCheckBox_fwinferenceActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jCheckBox_fwinferenceActionPerformed
        if(jCheckBox_fwinference.isSelected()){
            fwdInference = true;
        }
        else fwdInference = false;
    }//GEN-LAST:event_jCheckBox_fwinferenceActionPerformed

    private void jCheckBox_assertedActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jCheckBox_assertedActionPerformed
        if(!jCheckBox_asserted.isSelected()){
            if(jCheckBox_fwinference.isSelected()){
                jCheckBox_fwinference.setSelected(false);
            }
            jCheckBox_fwinference.setEnabled(false);
        }
        else jCheckBox_fwinference.setEnabled(true);
    }//GEN-LAST:event_jCheckBox_assertedActionPerformed

    /**
    * @param args the command line arguments
    */
    public static void main(String args[]) {
        java.awt.EventQueue.invokeLater(new Runnable() {
            public void run() {
                new QBEBasePanel().setVisible(true);
            }
        });
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButton_OK;
    private javax.swing.JButton jButton_add;
    private javax.swing.JButton jButton_cancel;
    private javax.swing.JCheckBox jCheckBox_asserted;
    private javax.swing.JCheckBox jCheckBox_fwinference;
    private javax.swing.JComboBox jComboBox_atype;
    private javax.swing.JComboBox jComboBox_cf;
    private javax.swing.JComboBox jComboBox_slot;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel_atype;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JScrollPane jScrollPane1;
    private JTableRE jTable1;
    // End of variables declaration//GEN-END:variables

}
