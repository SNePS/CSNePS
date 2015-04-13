/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/*
 * NodeFind.java
 *
 * Created on Nov 11, 2011, 12:47:53 PM
 */

package edu.buffalo.cse.sneps3.gui;

import java.util.HashSet;
import java.util.Set;
import java.util.StringTokenizer;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import edu.buffalo.cse.sneps3.gui.business.Slot;
import edu.buffalo.cse.sneps3.gui.business.Term;

/**
 *
 * @author dan
 */
public class NodeFind extends javax.swing.JPanel {

    /** Creates new form NodeFind */
    public NodeFind() {
        initComponents();
    }

    protected Set<Term> getContextTerms(int depth, Set<Term> collected){
    	Set<Term> added = new HashSet<Term>();
    	
    	for (Term term : collected){
    		if (term.getUpCablesetTerms() != null) 
    			added.addAll(term.getUpCablesetTerms());
    	}
    	
    	collected.addAll(added);
    	added = new HashSet<Term>();
    	
    	for (Term term : collected){
    		if (term.getDownCableset() != null){
    			Set<Slot> slots = term.getDownCableset().keySet();
    			for (Slot slot : slots){
    				added.addAll(term.getDownCableset().get(slot));
    			}
    		}
    	}
    	
    	collected.addAll(added);

    	if (depth == 1) return collected;
    	
    	return getContextTerms(depth - 1, collected);
    }
    
    protected boolean performOK(){
        Set<Term> nns = new HashSet<Term>();
        StringTokenizer st = new StringTokenizer(jTextField1.getText());
        String multiword = "";
        while(st.hasMoreTokens()){
            String s = st.nextToken();
            if(s.startsWith("\"")){
                multiword += s;
            }
            else if(!multiword.equals("")){
                multiword += " " + s;
                if(multiword.endsWith("\"")){
                	Term t = Term.getTerm(multiword.substring(1, multiword.length()-1));
                	if(t != null)
                		nns.add(t);
                	else{
                		JOptionPane.showMessageDialog(this, "Node with the label: " + multiword.substring(1, multiword.length()-1) + " was not found.");
                		return false;
                	}
                    //nns.add(multiword.substring(1, multiword.length()-1));
                    multiword = "";
                }
            }
            else if(Term.getTerm(s) != null) 
            	nns.add(Term.getTerm(s));
            else {
            	JOptionPane.showMessageDialog(this, "Node with the label: " + s + " was not found.");
            	return false;
            }
        }

        if(!multiword.equals("")){
            JOptionPane.showMessageDialog(this, "The multi-word term name: " + multiword + " is un-terminated.");
            return false;
        }

        String cd = jTextField_contextDegree.getText().trim();
        int cdint = 0;
        try {
        	cdint = Integer.parseInt(cd);
        } catch (Exception e){
        	JOptionPane.showMessageDialog(this, cd + " is not an integer.");
        	return false;
        }
        
        if (cdint > 0){
        	nns.addAll(getContextTerms(cdint, nns));
        }
        
        if(GUI2.DEBUG) System.out.println("Node Find: " + nns);
        
        if(GUI2.getInstance().getGraphPanel().isShowingAll()){
            GUI2.getInstance().getGraphPanel().displayOnlyTermSet(nns);
        }
        else GUI2.getInstance().getGraphPanel().displayTermSet(nns);
        return true;
    }

    private void initComponents() {

    	this.setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
    	
        jLabel1 = new javax.swing.JLabel();
        jTextField1 = new javax.swing.JTextField();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTextArea1 = new javax.swing.JTextArea();

        jLabel1.setText("Show only node(s) with name(s):");

        jScrollPane1.setHorizontalScrollBarPolicy(javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        jScrollPane1.setVerticalScrollBarPolicy(javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER);

        jTextArea1.setBackground(javax.swing.UIManager.getDefaults().getColor("Panel.background"));
        jTextArea1.setColumns(20);
        jTextArea1.setEditable(false);
        jTextArea1.setLineWrap(true);
        jTextArea1.setRows(2);
        jTextArea1.setText("You may enter a single node name or a space \nseparated list of node names.");
        jScrollPane1.setViewportView(jTextArea1);

        JPanel nodeSelectPane = new JPanel();
        
        nodeSelectPane.setLayout(new BoxLayout(nodeSelectPane, BoxLayout.Y_AXIS));
        nodeSelectPane.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
        nodeSelectPane.add(Box.createHorizontalGlue());
        nodeSelectPane.add(jScrollPane1);
        nodeSelectPane.add(jTextField1);
        nodeSelectPane.add(jLabel1);

        JPanel contextSelectPane = new JPanel();
        
        jLabel_beginContextLabel = new JLabel();
        jLabel_endContextLabel = new JLabel();
        jTextField_contextDegree = new JTextField();
        
        jLabel_beginContextLabel.setText("Display context depth: ");
        jTextField_contextDegree.setText("0");
        jTextField_contextDegree.setColumns(3);
        jLabel_endContextLabel.setText("relations");
        
        contextSelectPane.setLayout(new BoxLayout(contextSelectPane, BoxLayout.X_AXIS));
        contextSelectPane.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
        contextSelectPane.add(Box.createHorizontalGlue());
        contextSelectPane.add(jLabel_beginContextLabel);
        contextSelectPane.add(jTextField_contextDegree);
        contextSelectPane.add(jLabel_endContextLabel);
        
        
        this.add(nodeSelectPane);
        this.add(contextSelectPane);
        this.add(Box.createVerticalStrut(240));
    }
    
    private javax.swing.JLabel jLabel1;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTextArea jTextArea1;
    private javax.swing.JTextField jTextField1;
    
    // Show context. 
    private JLabel jLabel_beginContextLabel;
    private JLabel jLabel_endContextLabel;
    private JTextField jTextField_contextDegree;
}
