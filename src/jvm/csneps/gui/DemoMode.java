/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/*
 * DemoMode.java
 *
 * Created on May 13, 2010, 12:57:16 PM
 */

package csneps.gui;

import java.awt.Dimension;
import java.awt.Toolkit;
import javax.swing.text.Element;
import javax.swing.text.Utilities;

import csneps.gui.util.ClojureTools;

/**
 *
 * @author dan
 */
public class DemoMode extends javax.swing.JFrame {

	private static final long serialVersionUID = 6945617409033295743L;

	GUI2 parent;

    private int currOffset = 0;
    private int currRow = 0;
    
    private String buffer;

    /** Creates new form DemoMode */
    public DemoMode() {
        initComponents();
        // Get the default toolkit
        Toolkit toolkit = Toolkit.getDefaultToolkit();

        // Get the current screen size
        Dimension scrnsize = toolkit.getScreenSize();

        this.setLocation(((int)scrnsize.getWidth()-this.getWidth())/2, this.getY());
    }

    public void setupDemo(String s, GUI2 p){
        jTextArea1.setText(s);
        this.jTextArea1.setCaretPosition(0);
        parent = p;
        buffer = "";
        executeNextRow();
    }
    
    public void executeNextRow(){
        Element elem = Utilities.getParagraphElement(jTextArea1, currOffset);
        int start = elem.getStartOffset();
        int end = elem.getEndOffset();
        jTextArea1.select(start, end);
        currOffset = jTextArea1.getCaretPosition();

        if (jTextArea1.getSelectedText() != null)
        	buffer += jTextArea1.getSelectedText();
        
        if (!buffer.equals("") && ClojureTools.matchingParens(buffer)){
        	GUI2.getInstance().clojureEval(buffer);
        	buffer = "";
        }
        
        currRow++;
        if(jTextArea1.getLineCount() == currRow){
            jButton_next.setEnabled(false);
            jButton_runToEnd.setEnabled(false);
        }
    }
    
    public boolean isFinished(){
    	return !jButton_next.isEnabled();
    }
    
    private void initComponents() {

        jScrollPane1 = new javax.swing.JScrollPane();
        jTextArea1 = new javax.swing.JTextArea();
        jButton_next = new javax.swing.JButton();
        jButton_runToEnd = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);

        jTextArea1.setColumns(20);
        jTextArea1.setRows(5);
        jScrollPane1.setViewportView(jTextArea1);

        jButton_next.setText("Next");
        jButton_next.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });

        jButton_runToEnd.setText("Run To End");
        jButton_runToEnd.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton2ActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 768, Short.MAX_VALUE)
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jButton_next)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButton_runToEnd)))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 144, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButton_next)
                    .addComponent(jButton_runToEnd)))
        );

        pack();
    }

    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {
        executeNextRow();
    }

    private void jButton2ActionPerformed(java.awt.event.ActionEvent evt) {
        while(currRow < jTextArea1.getLineCount()){
            executeNextRow();
        }
    }

    /**
    * @param args the command line arguments
    */
    public static void main(String args[]) {
        java.awt.EventQueue.invokeLater(new Runnable() {
            public void run() {
                new DemoMode().setVisible(true);
            }
        });
    }

    private javax.swing.JButton jButton_next;
    private javax.swing.JButton jButton_runToEnd;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTextArea jTextArea1;

}
