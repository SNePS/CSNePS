/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.buffalo.cse.sneps3.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.HashSet;
import javax.swing.DefaultCellEditor;
import javax.swing.JComboBox;
import javax.swing.JOptionPane;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;

import edu.buffalo.cse.sneps3.gui.business.Caseframe;
import edu.buffalo.cse.sneps3.gui.business.FnInterop;
import edu.buffalo.cse.sneps3.gui.business.Slot;

/**
 *
 * @author dan
 */
public class AddToKBPanel extends QBEBasePanel{

    public AddToKBPanel(){
        super();
        showAType();

        getJBOK().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                performOK(true);
            }
        });

        this.getRootPane().setDefaultButton(getJBOK());

        /*DefaultTableModel dtm = (DefaultTableModel) getTable().getModel();
        dtm.addColumn("Term Wrapper");
        TableColumn wrapperCol = getTable().getColumnModel().getColumn(2);
        JComboBox jComboBox_wrap = new JComboBox();
        jComboBox_wrap.addItem("None");
        jComboBox_wrap.addItem("(every ...)");
        jComboBox_wrap.addItem("(some ...)");
        wrapperCol.setCellEditor(new DefaultCellEditor(jComboBox_wrap));

        dtm.addColumn("Var");
        dtm.addColumn("DepVar");*/

    }

    @Override
    protected String fillWithFrame(){
        FrameSlotDialog fsd = new FrameSlotDialog(this, new AddToKBPanel());
        fsd.showAType();
        fsd.showAsserted();
        fsd.setVisible(true);
        //Not yet implemented.
        hideFwdInference();

        //System.err.println(fsd.getResult());
        return fsd.getResult();
    }


    private String getVar(){
        int rowCount = getTable().getModel().getRowCount();
        int filled = rowCount;
        HashSet<String> vars = new HashSet<String>();
        for (int i = 0; i < getTable().getModel().getRowCount(); i++) {
            String val = (String) getTable().getModel().getValueAt(i, 1);
            if (val == null || val.trim().equals("")) {
                filled--;
            }
            else if(val.trim().startsWith("?")) vars.add(val.trim());
        }


        //Be sure we don't have empty slot fillers when asserting a ground term.
        if(getJCBType().getSelectedItem().equals("Nothing")){
            if(filled-vars.size() < rowCount){
                JOptionPane.showMessageDialog(this, "You may not have empty slots or variables when defining ground terms.");
                return "";
            }
        }
        else{
            if(filled-vars.size() < (rowCount-1)){
                JOptionPane.showMessageDialog(this, "You may not have more than one empty slot or variable when defining non-ground terms. Hint: You may mean to say (every (Isa ?<var> Entity)).");
                return "";
            }
            if(filled-vars.size() == rowCount){
                JOptionPane.showMessageDialog(this, "You must have an empty slot or variable when defining non-ground terms.");
                return "";
            }
        }
        if(vars.isEmpty()) return null;
        else return (String)vars.toArray()[0];
    }
    
    /*@Override
    public String performOK(boolean execute){
        //Variable setup
        Frame f;
        Caseframe cf = (Caseframe)getJCBCF().getSelectedItem();
        ArrayList<Slot> slots = new ArrayList<Slot>();
        ArrayList<Object> fillers = new ArrayList<Object>();
        String fsym = null;
        if(cf.fsymbols != null) fsym =  table_model.getValueAt(0, 1).toString();


        if(getJCBType().getSelectedItem().equals("Nothing")){
            if(fsym == null)
                f = new Frame(cf, slots, fillers);
            else
                f = new Frame(cf, fsym, slots, fillers);
        }
        else{
            if(fsym == null)
                f = new Frame()
        }

        if(execute){

        }
        else{
            return f.toString();
        }
    }*/

    private boolean hasInputErrors(int rowCount, int filled, HashSet<String> vars){
        if(getJCBType().getSelectedItem().equals("Nothing")){
            if(filled-vars.size() < rowCount){
                JOptionPane.showMessageDialog(this, "You may not have empty slots or variables when defining ground terms.");
                return true;
            }
        }
        else{
            if(filled-vars.size() < (rowCount-1)){
                JOptionPane.showMessageDialog(this, "You may not have more than one empty slot or variable when defining non-ground terms. Hint: You may mean to say (every (Isa ?<var> Entity)).");
                return true;
            }
            if(filled-vars.size() == rowCount){
                JOptionPane.showMessageDialog(this, "You must have an empty slot or variable when defining non-ground terms.");
                return true;
            }
        }
        return false;
    }
    
    private String buildSlotString(Slot s){
    	ArrayList<String> slotvals = new ArrayList<String>();
    	for(int i = 0; i < table_model.getRowCount(); i++){
            if(s.getName().equals(table_model.getValueAt(i, 0).toString())){
            	String cellValue = (String)getTable().getModel().getValueAt(i, 1);
            	slotvals.add(cellValue);
            }
    	}
    	if(slotvals.size() == 1){
    		return slotvals.get(0);
    	}
    	String setof = "(setof";
    	for(String str : slotvals) setof += " " + str;
    	return setof + ")";
    }
    
    private String buildTermString(){
    	String termString = "(";

        Caseframe cf = (Caseframe)getJCBCF().getSelectedItem();
        ArrayList<Slot> slots = cf.getSlots();

        // Function Symbol
        if(!cf.getFSymbols().isEmpty()) termString += table_model.getValueAt(0, 1).toString();
        else termString += cf.toString();

        // Add the rest of the slots.
        for (Slot s : slots){
        	termString += " " + buildSlotString(s);
        }
        
        if(getJCBType().getSelectedItem().equals("(some ...)")){
        	
        }
        if(getJCBType().getSelectedItem().equals("(every ...)")){
        	
        }
        
        
        /*if (slots.size() > 1){ //form is like (assert '(Isa (setof a c) (setof b d)))
            for(Slot s : slots){
                if(!cf.getFSymbols().isEmpty() && s == slots.get(0)) continue;
                //call += " (setof ";     //Can't be here if we have a var!
                String inter = "";
                boolean varSeen = false;
                for(int i = 0; i < table_model.getRowCount(); i++){
                    if(s.getName().equals(table_model.getValueAt(i, 0).toString())){
                        if(varSeen){
                            JOptionPane.showMessageDialog(this, "You may not have a set where any element is a variable.");
                            return "";
                        }
                        String cv = (String)getTable().getModel().getValueAt(i, 1);
                        if(cv == null) cv="";
                        if(cv.equals("") && getJCBType().getSelectedItem().equals("(some ...)")){
                            inter += " w" + varnum + " ";
                            varSeen = true;
                        }
                        if(cv.equals("") && getJCBType().getSelectedItem().equals("(every ...)")){
                            inter += " w" + varnum + " ";
                            varSeen = true;
                        }
                        //if(!jComboBox_atype.getSelectedItem().equals("Definite") && cv.equals("")){
                        //    call += "w" + varcount++ + " ";
                        //}
                        else inter += " " + table_model.getValueAt(i, 1);
                    }
                }
                if(varSeen){
                    call += inter;
                }
                else call += " (setof" + inter + ")";
            }
            if(!execute) call += "))";
            else if(getJCBAsserted().isSelected()) call += "))";
            else call += ") '" + cf.getType() + ")";
        }
        else{ //form is like (assert '(and a b c))
            for(Slot s : slots){
                if(!cf.getFSymbols().isEmpty() && s == slots.get(0)) continue;
                call += " ";
                for(int i = 0; i < table_model.getRowCount(); i++){
                    if(s.getName().equals(table_model.getValueAt(i, 0).toString())){
                        String cv = (String)getTable().getModel().getValueAt(i, 1);
                        if(cv == null) cv="";
                        if(cv.equals("") && getJCBType().getSelectedItem().equals("(some ...)")){
                            call += " w" + varnum + " ";
                        }
                        if(cv.equals("") && getJCBType().getSelectedItem().equals("(every ...)")){
                            call += " w" + varnum + " ";
                        }
                        else call += table_model.getValueAt(i, 1) + " ";
                    }
                }
                call += ")";
            }
            if(getJCBAsserted().isSelected()) call += "))";
            else call += " '" + cf.getType() + ")";
        }
        if(!getJCBType().getSelectedItem().equals("Nothing")) call += ")";*/

    	
    	return termString + ")";
    }
    
    public String performOK(boolean baselevel){
        int rowCount = getTable().getModel().getRowCount();
        int filled = rowCount;
        HashSet<String> vars = new HashSet<String>();
        for (int i = 0; i < getTable().getModel().getRowCount(); i++) {
            String val = (String) getTable().getModel().getValueAt(i, 1);
            if (val == null || val.trim().equals("")) {
                filled--;
            }
            else if(val.trim().startsWith("?")) vars.add(val.trim());
        }
        
        if(hasInputErrors(rowCount, filled, vars)) return "";
        
        String termString = buildTermString();
        
        this.dispose();
        
        if(baselevel){
        	if(getJCBAsserted().isSelected() && !fwdInference)
        		FnInterop.assertTerm(buildTermString());
        	else if (getJCBAsserted().isSelected() && fwdInference)
        		FnInterop.assertETerm(buildTermString());
        	else FnInterop.defineTerm(buildTermString());
        }
        
        return termString;	
    }
    
    
/*    public String performOK(boolean execute){
                //Stop editing so we don't lose data
        //jTable1.getCellEditor().stopCellEditing();

        //Build our lisp call:

        int rowCount = getTable().getModel().getRowCount();
        int filled = rowCount;
        HashSet<String> vars = new HashSet<String>();
        for (int i = 0; i < getTable().getModel().getRowCount(); i++) {
            String val = (String) getTable().getModel().getValueAt(i, 1);
            if (val == null || val.trim().equals("")) {
                filled--;
            }
            else if(val.trim().startsWith("?")) vars.add(val.trim());
        }
        
        
        //Be sure we don't have empty slot fillers when asserting a ground term.
        if(getJCBType().getSelectedItem().equals("Nothing")){
            if(filled-vars.size() < rowCount){
                JOptionPane.showMessageDialog(this, "You may not have empty slots or variables when defining ground terms.");
                return "";
            }
        }
        else{
            if(filled-vars.size() < (rowCount-1)){
                JOptionPane.showMessageDialog(this, "You may not have more than one empty slot or variable when defining non-ground terms. Hint: You may mean to say (every (Isa ?<var> Entity)).");
                return "";
            }
            if(filled-vars.size() == rowCount){
                JOptionPane.showMessageDialog(this, "You must have an empty slot or variable when defining non-ground terms.");
                return "";
            }
        }

        String call = "";

        if(execute){
            if(getJCBAsserted().isSelected()) call = "(assert";
            else call = "(defineTerm";
            
            if(fwdInference) call += "!";

            call += " `(";
        }
        else{
            if(getJCBAsserted().isSelected()) call = " ,(assert";
            else call = " ,(defineTerm";
            call += " `(";
        }

        int varnum = 0;

        //Figure out the variable situation.
        if(getJCBType().getSelectedItem().equals("(every ...)")){
            call += "every ";
            if(!vars.isEmpty()) call += vars.toArray()[0] + " (";
            else{
                varnum = QBEBasePanel.getTotalVarCount();
                QBEBasePanel.incrementTotalVarCountBy(1);
                call += "w" + varnum + " (";
            }
        }
        else if(getJCBType().getSelectedItem().equals("(some ...)")){
            call += "some ";

            String depVar = JOptionPane.showInputDialog(this, "Enter the variable name which " +
                    (vars.isEmpty() ? "w" + varnum : vars.toArray()[0]) + " depends on.");
            if(depVar == null) return "";

            if(!vars.isEmpty()) call += vars.toArray()[0] + "(" + depVar + ") (";
            else{
                varnum = QBEBasePanel.getTotalVarCount();
                QBEBasePanel.incrementTotalVarCountBy(1);
                call += "w" + varnum + "(" + depVar + ") (";
            }
        }


        /*int varcount = 0;
        if(!jComboBox_atype.getSelectedItem().equals("Definite")){
            if(jComboBox_atype.getSelectedItem().equals("Indefinite")) call += " some ";
            else call += " every ";
            for(int i = 0; i < table_model.getRowCount(); i++){
                String cv = "";
                if(getTable().getModel().getValueAt(i, 1) instanceof JComboBox){
                    cv = (String)((JComboBox)getTable().getModel().getValueAt(i, 1)).getSelectedItem();
                }
                else cv = (String)getTable().getModel().getValueAt(i, 1);
                if(cv == null) cv="";
                if (cv.equals("")){
                    call += "w" + varcount++ + " ";
                }
            }
            call += "(";
        }*/

/*        Caseframe cf = (Caseframe)getJCBCF().getSelectedItem();
        ArrayList<Slot> slots = cf.getSlots();

        if(!cf.getFSymbols().isEmpty()) call += table_model.getValueAt(0, 1).toString();
        else call += cf.toString();

        if (slots.size() > 1){ //form is like (assert '(Isa (setof a c) (setof b d)))
            for(Slot s : slots){
                if(!cf.getFSymbols().isEmpty() && s == slots.get(0)) continue;
                //call += " (setof ";     //Can't be here if we have a var!
                String inter = "";
                boolean varSeen = false;
                for(int i = 0; i < table_model.getRowCount(); i++){
                    if(s.getName().equals(table_model.getValueAt(i, 0).toString())){
                        if(varSeen){
                            JOptionPane.showMessageDialog(this, "You may not have a set where any element is a variable.");
                            return "";
                        }
                        String cv = (String)getTable().getModel().getValueAt(i, 1);
                        if(cv == null) cv="";
                        if(cv.equals("") && getJCBType().getSelectedItem().equals("(some ...)")){
                            inter += " w" + varnum + " ";
                            varSeen = true;
                        }
                        if(cv.equals("") && getJCBType().getSelectedItem().equals("(every ...)")){
                            inter += " w" + varnum + " ";
                            varSeen = true;
                        }
                        //if(!jComboBox_atype.getSelectedItem().equals("Definite") && cv.equals("")){
                        //    call += "w" + varcount++ + " ";
                        //}
                        else inter += " " + table_model.getValueAt(i, 1);
                    }
                }
                if(varSeen){
                    call += inter;
                }
                else call += " (setof" + inter + ")";
            }
            if(!execute) call += "))";
            else if(getJCBAsserted().isSelected()) call += "))";
            else call += ") '" + cf.getType() + ")";
        }
        else{ //form is like (assert '(and a b c))
            for(Slot s : slots){
                if(!cf.getFSymbols().isEmpty() && s == slots.get(0)) continue;
                call += " ";
                for(int i = 0; i < table_model.getRowCount(); i++){
                    if(s.getName().equals(table_model.getValueAt(i, 0).toString())){
                        String cv = (String)getTable().getModel().getValueAt(i, 1);
                        if(cv == null) cv="";
                        if(cv.equals("") && getJCBType().getSelectedItem().equals("(some ...)")){
                            call += " w" + varnum + " ";
                        }
                        if(cv.equals("") && getJCBType().getSelectedItem().equals("(every ...)")){
                            call += " w" + varnum + " ";
                        }
                        else call += table_model.getValueAt(i, 1) + " ";
                    }
                }
                call += ")";
            }
            if(getJCBAsserted().isSelected()) call += "))";
            else call += " '" + cf.getType() + ")";
        }
        if(!getJCBType().getSelectedItem().equals("Nothing")) call += ")";

        System.err.println(call + " " + execute);

        this.dispose();

        if(execute){
            GUI2.getInstance().makeLispCall(call);
            QBEBasePanel.resetState();
        }

        return call;
    }*/

}
