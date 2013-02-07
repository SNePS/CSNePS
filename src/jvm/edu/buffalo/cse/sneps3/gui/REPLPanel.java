/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/*
 * REPLPanel.java
 *
 * Created on Nov 2, 2009, 9:21:05 PM
 */

package edu.buffalo.cse.sneps3.gui;

import java.awt.event.KeyEvent;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;

import clojure.tools.nrepl.Connection.Response;
import edu.buffalo.cse.sneps3.gui.business.repl.REPLView;

//import com.franz.jlinker.*;
import java.awt.BorderLayout;
import java.util.Stack;


/**
 *
 * @author Dan Schlegel
 */
public class REPLPanel extends javax.swing.JPanel {

	private JTextArea output;

	private REPLView replView;

	private Stack<Character> parens;
	private String inputText = "";
	
	//public REPLFrame parent;
	/** Creates new form REPLPanel */
	public REPLPanel() {
		initComponents();
		this.setLayout(new BorderLayout());
		add(jSplitPane1, BorderLayout.CENTER);
		output = jTextArea1;
		jSplitPane1.setResizeWeight(1.0);
		jSplitPane1.setDividerSize(0);
		jSplitPane1.setDividerLocation(this.getHeight() - 15);
		
		parens = new Stack<Character>();
		//testParenMatcher();
	}

	public void connect(){
		replView = new REPLView(this, GUI2.getInstance().cljconn);
	}

	public void makeClojureCall(String text){
		appendText("=> " + text + "\n");
		replView.eval(text);
	}
	
	private boolean parenMatchError(Character c, int offset, boolean silent){
		if(!silent) appendText("Unexpected character: " + c + " at offset: " + offset + '\n');
		if(!silent) System.out.println("Unexpected character: " + c + " at offset: " + offset);
		parens = new Stack<Character>();
		return false;
	}
	
	public boolean parensMatch(String s){
		return parensMatch(s, false);
	}
	
	public boolean parensMatch(String s, boolean silent){
		boolean ignoring = false;
		for(int i = 0; i < s.length(); i++){
			char c = s.charAt(i);
			if (c == '\"' && (i > 0 && s.charAt(i-1) == '\\')){
				ignoring = !ignoring;
				if(parens.peek().equals('\"'))
					parens.pop();
				else parens.push('\"');
				continue;
			}
			if(ignoring) continue;
			if (c == '(' || c == '[' || c == '{'){ 
				parens.push(c);
				continue;
			}
			if (c == ')'){
				if(parens.peek().equals('(')){
					parens.pop();
					continue;
				}
				return parenMatchError(c, i, silent);
			}
			if (c == ']'){
				if(parens.peek().equals('[')){
					parens.pop();
					continue;
				}
				return parenMatchError(c, i, silent);
			}
			if (c == '}'){
				if(parens.peek().equals('{')){
					parens.pop();
					continue;
				}
				return parenMatchError(c, i, silent);
			}
		}
		if(parens.isEmpty()) return true;
		if(!silent) appendText("Unmatched delimiter: " + parens.peek() + '\n');
		if(!silent) System.out.println("Unmatched delimiter: " + parens.peek());
		parens = new Stack<Character>();
		return false;
	}
	
	public void testParenMatcher(){
		System.out.println("===================== Testing Matcher =====================");
		System.out.println(parensMatch("(abc 123 \"111\")"));
		System.out.println(parensMatch("(Isa (xyz) (orange))"));
		System.out.println(parensMatch("(Isa (xyz)"));
	}
	
	

	public void appendText(String strText) {
		final String appendText = strText;
		// The text must be appended in a separate thread.
		java.awt.EventQueue.invokeLater( new Runnable()
		{
			public void run()
			{
				jTextArea1.append(appendText);
			}
		});
	}

	public JTextArea getLogComponent(){
		return output;
	}

	private void initComponents() {

		jSplitPane1 = new javax.swing.JSplitPane();
		jTextField1 = new javax.swing.JTextField();
		jScrollPane1 = new javax.swing.JScrollPane();
		jTextArea1 = new javax.swing.JTextArea();

		jSplitPane1.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);

		jTextField1.addKeyListener(new java.awt.event.KeyAdapter() {
			public void keyPressed(java.awt.event.KeyEvent evt) {
				jTextField1KeyPressed(evt);
			}
		});
		jSplitPane1.setBottomComponent(jTextField1);

		jTextArea1.setColumns(20);
		jTextArea1.setRows(5);
		jScrollPane1.setViewportView(jTextArea1);

		jSplitPane1.setLeftComponent(jScrollPane1);
	}

	private void jTextField1KeyPressed(java.awt.event.KeyEvent evt) {
		if(evt.getKeyCode() == KeyEvent.VK_ENTER){
			String text = jTextField1.getText();

			//appendText("=> " + text + "\n");
			makeClojureCall(text);
			//replView.eval(text);

			jTextField1.setText("");
		}

	}


	// Variables declaration - do not modify
	private javax.swing.JScrollPane jScrollPane1;
	private javax.swing.JSplitPane jSplitPane1;
	private javax.swing.JTextArea jTextArea1;
	private javax.swing.JTextField jTextField1;
	// End of variables declaration

}

