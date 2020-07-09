/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/*
 * SemanticTypeForm.java
 *
 * Created on Mar 28, 2010, 8:43:27 PM
 */

package csneps.gui;

import java.util.ArrayList;
import java.util.Collections;

import csneps.gui.business.FnInterop;
import csneps.gui.business.SemanticType;

import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListModel;
import javax.swing.JOptionPane;

/**
 *
 * @author dan
 */
public class SemanticTypeForm extends javax.swing.JFrame {

	private static final long serialVersionUID = -5293011594569596363L;
	
	DefaultListModel<SemanticType> lm;
    DefaultComboBoxModel<SemanticType> cbm;

    /** Creates new form SemanticTypeForm */
    public SemanticTypeForm() {
    	lm = new DefaultListModel<SemanticType>();
    	
    	cbm = new DefaultComboBoxModel<SemanticType>();
    	for(SemanticType t : SemanticType.getSemanticTypes()){
            cbm.addElement(t);
        }
    	
        initComponents();
    }

    private void initComponents() {

        jTextField_name = new javax.swing.JTextField();
        jLabel1 = new javax.swing.JLabel();
        jButton_create = new javax.swing.JButton();
        jButton_cancel = new javax.swing.JButton();
        jPanel1 = new javax.swing.JPanel();
        jComboBox_type = new javax.swing.JComboBox<SemanticType>();
        jButton_add = new javax.swing.JButton();
        jScrollPane1 = new javax.swing.JScrollPane();
        jList1 = new javax.swing.JList<SemanticType>();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Define Semantic Type");

        jLabel1.setText("Type Name:");

        jButton_create.setText("Create");
        jButton_create.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton_createActionPerformed(evt);
            }
        });

        jButton_cancel.setText("Cancel");
        jButton_cancel.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton_cancelActionPerformed(evt);
            }
        });

        jPanel1.setBorder(javax.swing.BorderFactory.createTitledBorder("Parent Types"));

        jComboBox_type.setModel(cbm);
        jList1.setModel(lm);

        jButton_add.setText("Add");
        jButton_add.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton_addActionPerformed(evt);
            }
        });

        jScrollPane1.setViewportView(jList1);

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 226, Short.MAX_VALUE)
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(jComboBox_type, javax.swing.GroupLayout.PREFERRED_SIZE, 147, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(18, 18, 18)
                        .addComponent(jButton_add)))
                .addContainerGap())
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel1Layout.createSequentialGroup()
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 158, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jComboBox_type, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButton_add))
                .addContainerGap())
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap(103, Short.MAX_VALUE)
                .addComponent(jButton_create)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButton_cancel)
                .addContainerGap())
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jLabel1)
                        .addGap(24, 24, 24)
                        .addComponent(jTextField_name, javax.swing.GroupLayout.DEFAULT_SIZE, 153, Short.MAX_VALUE)))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel1)
                    .addComponent(jTextField_name, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButton_create)
                    .addComponent(jButton_cancel))
                .addContainerGap())
        );

        pack();
    }

    private void jButton_createActionPerformed(java.awt.event.ActionEvent evt) {
        if(jTextField_name.getText().trim().equals("")){
            JOptionPane.showMessageDialog(this, "You must enter a name for the semantic type.");
            return;
        }

        String parents = "";
        for(int i = 0; i < lm.getSize(); i++){
            parents += lm.get(i).toString() + " ";
        }

        if(parents.equals("")){
            JOptionPane.showMessageDialog(this, "You must select at least one parent type.");
            return;
        }
        
        FnInterop.defineType(jTextField_name.getText(), (ArrayList<SemanticType>)Collections.list(lm.elements()));
        
        this.setVisible(false);
        this.dispose();
    }

    private void jButton_cancelActionPerformed(java.awt.event.ActionEvent evt) {
        this.setVisible(false);
        this.dispose();
    }

    private void jButton_addActionPerformed(java.awt.event.ActionEvent evt) {
        if(!lm.contains(jComboBox_type.getSelectedItem()))
            lm.addElement((SemanticType)jComboBox_type.getSelectedItem());
    }

    /**
    * @param args the command line arguments
    */
    public static void main(String args[]) {
        java.awt.EventQueue.invokeLater(new Runnable() {
            public void run() {
                new SemanticTypeForm().setVisible(true);
            }
        });
    }

    private javax.swing.JButton jButton_add;
    private javax.swing.JButton jButton_cancel;
    private javax.swing.JButton jButton_create;
    private javax.swing.JComboBox<SemanticType> jComboBox_type;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JList<SemanticType> jList1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTextField jTextField_name;
}
