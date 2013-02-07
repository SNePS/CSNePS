/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/*
 * SlotForm.java
 *
 * Created on Mar 28, 2010, 6:51:03 PM
 */

package edu.buffalo.cse.sneps3.gui;

import edu.buffalo.cse.sneps3.gui.business.FnInterop;
import edu.buffalo.cse.sneps3.gui.business.SemanticType;

import javax.swing.DefaultComboBoxModel;

/**
 *
 * @author dan
 */
public class SlotForm extends javax.swing.JFrame {

    /** Creates new form SlotForm */
    public SlotForm() {
        initComponents();

        DefaultComboBoxModel m = new DefaultComboBoxModel();
        for(SemanticType t : SemanticType.getSemanticTypes()){
            m.addElement(t);
        }
        jComboBox_type.setModel(m);
    }

    private void initComponents() {

        jLabel1 = new javax.swing.JLabel();
        jTextField_name = new javax.swing.JTextField();
        jLabel2 = new javax.swing.JLabel();
        jComboBox_type = new javax.swing.JComboBox();
        jTextField_min = new javax.swing.JTextField();
        jLabel3 = new javax.swing.JLabel();
        jComboBox_pos = new javax.swing.JComboBox();
        jLabel4 = new javax.swing.JLabel();
        jComboBox_neg = new javax.swing.JComboBox();
        jLabel5 = new javax.swing.JLabel();
        jButton_create = new javax.swing.JButton();
        jButton1 = new javax.swing.JButton();
        jLabel6 = new javax.swing.JLabel();
        jTextField_max = new javax.swing.JTextField();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Define Slot");

        jLabel1.setText("Slot Name");

        jLabel2.setText("Semantic Type: ");

        jComboBox_type.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));

        jTextField_min.setText("1");

        jLabel3.setText("Min");

        jComboBox_pos.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "none", "reduce", "expand" }));
        jComboBox_pos.setSelectedIndex(1);

        jLabel4.setText("Posadjust");

        jComboBox_neg.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "none", "reduce", "expand" }));
        jComboBox_neg.setSelectedIndex(2);

        jLabel5.setText("Negadjust");

        jButton_create.setText("Create");
        jButton_create.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton_createActionPerformed(evt);
            }
        });

        jButton1.setText("Cancel");
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });

        jLabel6.setText("Max");

        jTextField_max.setText("nil");
        jTextField_max.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jTextField_maxActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                            .addComponent(jLabel2, javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel1, javax.swing.GroupLayout.Alignment.LEADING))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jTextField_name, javax.swing.GroupLayout.DEFAULT_SIZE, 282, Short.MAX_VALUE)
                            .addComponent(jComboBox_type, 0, 282, Short.MAX_VALUE)))
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jLabel4)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jComboBox_pos, javax.swing.GroupLayout.PREFERRED_SIZE, 99, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jLabel5)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jComboBox_neg, 0, 121, Short.MAX_VALUE))
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jLabel3)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextField_min, javax.swing.GroupLayout.PREFERRED_SIZE, 50, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabel6)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextField_max, javax.swing.GroupLayout.PREFERRED_SIZE, 50, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 48, Short.MAX_VALUE)
                        .addComponent(jButton_create)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButton1)))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel1)
                    .addComponent(jTextField_name, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel2)
                    .addComponent(jComboBox_type, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel4)
                    .addComponent(jComboBox_pos, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel5)
                    .addComponent(jComboBox_neg, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel3)
                    .addComponent(jTextField_min, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButton_create)
                    .addComponent(jButton1)
                    .addComponent(jLabel6)
                    .addComponent(jTextField_max, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        pack();
    }

    private void jButton_createActionPerformed(java.awt.event.ActionEvent evt) {
    	Integer min = (jTextField_min.getText().equals("nil") ? null : Integer.parseInt(jTextField_min.getText()));
    	Integer max = (jTextField_max.getText().equals("nil") ? null : Integer.parseInt(jTextField_max.getText()));
    	
    	FnInterop.defineSlot(jTextField_name.getText(), (SemanticType)jComboBox_type.getSelectedItem(), min, max, jComboBox_pos.getSelectedItem().toString(), jComboBox_neg.getSelectedItem().toString());
    	
        /*GUI2.getInstance().makeLispCall("(defineSlot " + jTextField_name.getText() + " :type " + jComboBox_type.getSelectedItem().toString()
                + (jTextField_min.getText().equals("") ? "" : " :min " + jTextField_min.getText())
                + (jTextField_max.getText().equals("") ? "" : " :max " + jTextField_max.getText())
                + " :posadjust " + jComboBox_pos.getSelectedItem().toString()
                + " :negadjust " + jComboBox_neg.getSelectedItem().toString() + ")");*/
        this.setVisible(false);
    }

    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
        this.setVisible(false);
        this.dispose();
    }//GEN-LAST:event_jButton1ActionPerformed

    private void jTextField_maxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jTextField_maxActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_jTextField_maxActionPerformed

    /**
    * @param args the command line arguments
    */
    public static void main(String args[]) {
        java.awt.EventQueue.invokeLater(new Runnable() {
            public void run() {
                new SlotForm().setVisible(true);
            }
        });
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButton1;
    private javax.swing.JButton jButton_create;
    private javax.swing.JComboBox jComboBox_neg;
    private javax.swing.JComboBox jComboBox_pos;
    private javax.swing.JComboBox jComboBox_type;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JTextField jTextField_max;
    private javax.swing.JTextField jTextField_min;
    private javax.swing.JTextField jTextField_name;
    // End of variables declaration//GEN-END:variables

}
