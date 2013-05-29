/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/*
 * DemoMode.java
 *
 * Created on May 13, 2010, 12:57:16 PM
 */

package edu.buffalo.cse.sneps3.gui;

import java.awt.Dimension;
import java.awt.Toolkit;
import javax.swing.text.Element;
import javax.swing.text.Utilities;

import clojure.lang.RT;

/**
 *
 * @author dan
 */
public class DemoMode extends javax.swing.JFrame {

    GUI2 parent;

    int currOffset = 0;
    int currRow = 0;

    /** Creates new form DemoMode */
    public DemoMode() {
        initComponents();
        // Get the default toolkit
	Toolkit toolkit = Toolkit.getDefaultToolkit();

	// Get the current screen size
	Dimension scrnsize = toolkit.getScreenSize();

        //this.setLocation(((int)scrnsize.getWidth()-this.getWidth())/2, (int)scrnsize.getHeight()-this.getHeight()-25);
        this.setLocation(((int)scrnsize.getWidth()-this.getWidth())/2, this.getY());

        /*if(GUI2.DEBUG){
            this.jTextArea1.setText("Hi\ntest\nthree\nfour\nfive\nthere\nwoot\nagain\ntest\nthree\nfour");
            this.jTextArea1.setCaretPosition(0);
            executeNextRow();
        }*/
    }

    public void setupDemo(String s, GUI2 p){
        jTextArea1.setText(s);
        this.jTextArea1.setCaretPosition(0);
        parent = p;
        executeNextRow();
    }

    public void executeNextRow(){
        Element elem = Utilities.getParagraphElement(jTextArea1, currOffset);
        int start = elem.getStartOffset();
        int end = elem.getEndOffset();
        jTextArea1.select(start, end);
        currOffset = jTextArea1.getCaretPosition();

        GUI2.getInstance().clojureEval(jTextArea1.getSelectedText());
        
        //if(parent!=null) parent.makeLispCall(jTextArea1.getSelectedText());
        currRow++;
        if(jTextArea1.getLineCount() == currRow){
            jButton1.setEnabled(false);
            jButton2.setEnabled(false);
        }
    }
    
    private void initComponents() {

        jScrollPane1 = new javax.swing.JScrollPane();
        jTextArea1 = new javax.swing.JTextArea();
        jButton1 = new javax.swing.JButton();
        jButton2 = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);

        jTextArea1.setColumns(20);
        jTextArea1.setRows(5);
        jScrollPane1.setViewportView(jTextArea1);

        jButton1.setText("Next");
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });

        jButton2.setText("Run To End");
        jButton2.addActionListener(new java.awt.event.ActionListener() {
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
                        .addComponent(jButton1)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButton2)))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 144, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButton1)
                    .addComponent(jButton2)))
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

    private javax.swing.JButton jButton1;
    private javax.swing.JButton jButton2;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTextArea jTextArea1;

}