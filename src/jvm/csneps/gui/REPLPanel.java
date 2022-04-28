/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/*
 * REPLPanel.java
 *
 * Created on Nov 2, 2009, 9:21:05 PM
 */

package csneps.gui;

import java.awt.event.KeyEvent;
import javax.swing.*;

import csneps.gui.business.repl.REPLView;

//import com.franz.jlinker.*;
import java.awt.BorderLayout;
import java.util.ArrayList;
import java.util.Stack;


/**
 *
 * @author Dan Schlegel
 */
public class REPLPanel extends javax.swing.JPanel {

	// GUI Components
	private JScrollPane jScrollPane1;
	private JSplitPane jSplitPane1;
	private JTextArea jTextArea_output;
	private JTextField jTextField_input;


	private REPLView replView;
	private Stack<Character> parens;
	private ArrayList<String> history;
	
	private int historyLoc = -1;
	
	/** Creates new form REPLPanel */
	public REPLPanel() {
		history = new ArrayList<>();
		
		initComponents();
		this.setLayout(new BorderLayout());
		add(jSplitPane1, BorderLayout.CENTER);
		jTextArea_output.setEditable(false);
		
		jSplitPane1.setResizeWeight(1.0);
		jSplitPane1.setDividerSize(0);
		jSplitPane1.setDividerLocation(this.getHeight() - 15);
		
		parens = new Stack<>();
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
		parens = new Stack<>();
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
		java.awt.EventQueue.invokeLater(() -> jTextArea_output.append(appendText));
	}

	public JTextArea getLogComponent(){
		return jTextArea_output;
	}

	private void initComponents() {

		jSplitPane1 = new javax.swing.JSplitPane();
		jTextField_input = new javax.swing.JTextField();
		jScrollPane1 = new javax.swing.JScrollPane();
		jTextArea_output = new javax.swing.JTextArea();

		jSplitPane1.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);

		jTextField_input.addKeyListener(new java.awt.event.KeyAdapter() {
			public void keyPressed(java.awt.event.KeyEvent evt) {
				jTextField1KeyPressed(evt);
			}
		});
		jSplitPane1.setBottomComponent(jTextField_input);

		jTextArea_output.setColumns(20);
		jTextArea_output.setRows(5);
		jScrollPane1.setViewportView(jTextArea_output);

		jSplitPane1.setLeftComponent(jScrollPane1);
	}

	private void jTextField1KeyPressed(java.awt.event.KeyEvent evt) {
		if(evt.getKeyCode() == KeyEvent.VK_ENTER){
			String text = jTextField_input.getText();

			makeClojureCall(text);
			history.add(text);

			jTextField_input.setText("");
		}
		else if (evt.getKeyCode() == KeyEvent.VK_UP){
			if (history.size() == 0) return;
			if (historyLoc == -1) historyLoc = history.size()-1;
			else if (historyLoc > 0) historyLoc--;
			
			jTextField_input.setText(history.get(historyLoc));
 		}
		else if (evt.getKeyCode() == KeyEvent.VK_DOWN){
			if (historyLoc == -1) return;
			if (historyLoc == history.size()-1){
				jTextField_input.setText("");
				historyLoc++;
				return;
			}
			else if (historyLoc > history.size()-1){
				return;
			}
			else jTextField_input.setText(history.get(++historyLoc));
 		}
		else historyLoc = -1;

	}
}

