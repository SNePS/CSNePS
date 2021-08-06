package csneps.gui;

import csneps.gui.business.FnInterop;
import csneps.gui.business.Slot;
import csneps.gui.business.Term;

import javax.swing.*;

/**
 *
 * @author dan
 */
public class AdoptRuleForm extends javax.swing.JFrame {

	private static final long serialVersionUID = -5293011594569596363L;

	DefaultListModel<String> lm;

    /** Creates new form SemanticTypeForm */
    public AdoptRuleForm() {
    	lm = new DefaultListModel<String>();

    	for(Term t : Term.getTerms()){
    	    if (t.getSyntacticType().equals("CARule") && !t.isAsserted()){
    	        lm.addElement(t.getDownCableset().get(Slot.getSlot("rulename")).toArray()[0].toString());
            }
        }

        initComponents();
    }

    private void initComponents() {
        jButton_adopt = new javax.swing.JButton();
        jButton_cancel = new javax.swing.JButton();
        jPanel1 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        jList1 = new javax.swing.JList<String>();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Adopt Rule");

        jButton_adopt.setText("Adopt");
        jButton_adopt.addActionListener(evt -> jButton_adoptActionPerformed(evt));

        jButton_cancel.setText("Cancel");
        jButton_cancel.addActionListener(evt -> jButton_cancelActionPerformed(evt));

        jPanel1.setBorder(javax.swing.BorderFactory.createTitledBorder("Unadopted Rules"));

        jList1.setModel(lm);

        jScrollPane1.setViewportView(jList1);

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 225, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel1Layout.createSequentialGroup()
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 500, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addContainerGap())
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap(103, Short.MAX_VALUE)
                .addComponent(jButton_adopt)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButton_cancel)
                .addContainerGap())
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButton_adopt)
                    .addComponent(jButton_cancel))
                .addContainerGap())
        );

        pack();
    }

    private void jButton_adoptActionPerformed(java.awt.event.ActionEvent evt) {

        for(int i : jList1.getSelectedIndices()){
            // This is super ugly. Should fix someday.
            FnInterop.adoptRule((Term)Term.getTerm(lm.getElementAt(i)).getUpCableset().get(Slot.getSlot("rulename")).toArray()[0]);
        }

        if(jList1.getSelectedIndices().length == 0){
            JOptionPane.showMessageDialog(this, "You must select at least one rule to adopt.");
            return;
        }

        this.setVisible(false);
        this.dispose();
    }

    private void jButton_cancelActionPerformed(java.awt.event.ActionEvent evt) {
        this.setVisible(false);
        this.dispose();
    }

    /**
    * @param args the command line arguments
    */
    public static void main(String args[]) {
        java.awt.EventQueue.invokeLater(new Runnable() {
            public void run() {
                new AdoptRuleForm().setVisible(true);
            }
        });
    }

    private javax.swing.JButton jButton_cancel;
    private javax.swing.JButton jButton_adopt;
    private javax.swing.JList<String> jList1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JScrollPane jScrollPane1;
}
