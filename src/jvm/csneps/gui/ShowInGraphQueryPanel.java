/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package csneps.gui;

import csneps.gui.business.FnInterop;
import csneps.gui.business.Slot;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.HashMap;
import javax.swing.JCheckBox;

/**
 *
 * @author Daniel R. Schlegel
 */
public class ShowInGraphQueryPanel extends QBEBasePanel{

    boolean withInference = false;


    public ShowInGraphQueryPanel(){
        super();
        this.setTitle("Show In Graph");
        this.hideAType();
        this.hideOKCancel();
        this.hideFwdInference();
        this.getRootPane().setDefaultButton(getJBOK());
        this.ignoreMinSlotCount = true;
        JCheckBox inf = super.getCheckBox();
        inf.setText("With Inference");
        inf.setSelected(false);
        inf.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if(((JCheckBox)e.getSource()).isSelected()) withInference = true;
                else withInference = false;
            }
        });

        getJBOK().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                performOK(true);
            }
        });
 
        

        //super.getJBOK().removeActionListener(super.getJBOK().getActionListeners()[0]);
        //super.getJBOK().addActionListener(new java.awt.event.ActionListener() {
        //    public void actionPerformed(java.awt.event.ActionEvent evt) {
        //        jButton_OKActionPerformed(evt);
        //    }
        //});
        //super.getJBCancel().removeActionListener(super.getJBCancel().getActionListeners()[0]);
    }

    @Override
    protected String fillWithFrame(){
        FrameSlotDialog fsd = new FrameSlotDialog(this, new ShowInGraphQueryPanel());
        fsd.setVisible(true);

        //System.err.println(fsd.getResult());
        return fsd.getResult();
    }

    private ArrayList<Integer> findInTable(Object o){
        ArrayList<Integer> ret = new ArrayList<Integer>();
        for(int i = 0; i < table_model.getRowCount(); i++){
            if(!getSelectedCF().getFSymbols().isEmpty() && i == 0) continue;
            if(getTable().getModel().getValueAt(i, 0) == o)
                ret.add(i);
        }
        return ret;
    }

    private HashMap<Slot, ArrayList<Integer>> slotToTablePositions(){
        HashMap<Slot, ArrayList<Integer>> ret = new HashMap<Slot, ArrayList<Integer>>();
        ArrayList<Slot> slots = getSelectedCF().getSlots();
        for(Slot s : slots){
            ret.put(s, findInTable(s));
        }
        return ret;
    }


    @Override
    protected String performOK(boolean execute) {
        if(getTable().getCellEditor()!=null) getTable().getCellEditor().stopCellEditing();

        String fn = (withInference ? "ask" : "find");
        
        //Get the count of rows in the table
        int rowCount = table_model.getRowCount();
        String varString = "";
        String match = "";
        //String regex = "\\b[x-z]\\d*\\b";
        //Pattern p = Pattern.compile(regex);

        if(execute){
            //Each row gets a variable automatically, even if we don't use it.
            for(int i = 0; i < (getTotalVarCount() + rowCount); i++){
                varString+="?w"+i+" ";
            }
        }
       
        HashMap<Slot, ArrayList<Integer>> hm = slotToTablePositions();
        for(Slot s : getSelectedCF().getSlots()){
            ArrayList<Integer> al = hm.get(s);
            for(int i = 0; i < al.size(); i++){
                Integer row = al.get(i);
                if(!getSelectedCF().getFSymbols().isEmpty() && row == 0) continue;
                String cv = (String)getTable().getModel().getValueAt(row, 1);
                if(cv==null) cv="";
                //Trim and add to CV.
                if(i == 0 && al.size() > 1) match += "(setof ";
                if(cv.trim().equals("")) match+= (withInference? "?w"+(getTotalVarCount()+row) : "?w"+(getTotalVarCount()+row)+" ");
                else if(cv.trim().startsWith("?")){
                    String trimmed = cv.trim();
                    match += trimmed + " ";
                    addCustomVar(trimmed);
                }
                else match+=cv + " ";
            }
            if(al.size() > 1) match += ") ";
        }

        /*for(int i = 0; i < rowCount; i++){
            if(getSelectedCF().fsymbols != null && i == 0) continue;
            String cv = (String)getTable().getModel().getValueAt(i, 1);
            if(cv==null) cv="";
            //Regex to add additional variables if necessary.
            //Matcher m = p.matcher(cv);
            //int count = 0;
            //while(m.find()){
            //    count++;
            //    varString+= cv.substring(m.start(), m.end())+ " ";
            //}
            //Trim and add to CV.
            if(cv.trim().equals("")) match+= (withInference? "?w"+(QBEBasePanel.getTotalVarCount()+i) : "?w"+(QBEBasePanel.getTotalVarCount()+i)+" ");
            else if(cv.trim().startsWith("?")){
                String trimmed = cv.trim();
                match += trimmed + " ";
                QBEBasePanel.addCustomVar(trimmed);
            }
            else match+=cv + " ";
        }*/

        if(execute){
            if(!getSelectedCF().getFSymbols().isEmpty()){
            	GUI2.getInstance().getGraphPanel().displayTermSet(FnInterop.find("(" + table_model.getValueAt(0, 1).toString() + " " + match + ")"));
            	
            	
                //GUI2.getInstance().makeLispCall("(graph-find '(" + fn + " '(" + table_model.getValueAt(0, 1).toString() + " " + match + ") " + (withInference ? "" : "'(" + varString + " " + QBEBasePanel.getCustomVars()) + ")"
                //   + "))");
            }
            else GUI2.getInstance().getGraphPanel().displayTermSet(FnInterop.find("(" + getSelectedCF() + " " + match + ")"));
            	//GUI2.getInstance().makeLispCall("(graph-find '(" + fn + " '(" + getSelectedCF() + " " + match + ") " + (withInference ? "" : "'(" + varString + " " + QBEBasePanel.getCustomVars()) + ")"
                    //+ "))");
            resetState();
            return "";
        }
        else{
            incrementTotalVarCountBy(rowCount);
            if(!getSelectedCF().getFSymbols().isEmpty()){
                return "(" + table_model.getValueAt(0, 1).toString() + " " + match + ")";
            }
            else return "(" + getSelectedCF() + " " + match + ")";
        }
    }

    public static void main(String args[]) {
        java.awt.EventQueue.invokeLater(new Runnable() {
            public void run() {
                new ShowInGraphQueryPanel().setVisible(true);
            }
        });
    }
}
