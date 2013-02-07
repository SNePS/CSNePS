/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.buffalo.cse.sneps3.gui;

import edu.buffalo.cse.sneps3.gui.QBEBasePanel.DialogType;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;

/**
 *
 * @author dan
 */
public class FrameSlotDialog extends JDialog{

    String result;
    QBEBasePanel qbe;
    JButton ok;
    JButton cancel;

    public FrameSlotDialog(Frame frame, QBEBasePanel content){
        super(frame, true);
        setTitle("Fill Slot");

        JButton close = new JButton("Close");
        close.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                dispose();
            }
        });

        qbe = content;
        qbe.dialogMode();

        ok = qbe.getJBOK();
        cancel = qbe.getJBCancel();

        //ok.removeActionListener(ok.getActionListeners()[0]);
        if (ok.getActionListeners().length > 0) {
            ok.removeActionListener(ok.getActionListeners()[0]);
        }
        ok.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                buildResult();
                if(!result.equals("")) dispose();
            }
        });

        if (cancel.getActionListeners().length > 0) {
            cancel.removeActionListener(ok.getActionListeners()[0]);
        }
        cancel.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                dispose();
            }
        });

        setSize(400, 428);

        add(qbe.getContentPane());

        //add(close);
    }

    protected void showAType(){
        qbe.showAType();
    }

    protected void showAsserted(){
        qbe.getJCBAsserted().setVisible(true);
    }

    private void buildResult(){

        result = qbe.performOK(false);


        //This is an atg build, not find... hmm.
/*        result = "(";
        int varcount = QBEPanel.getTotalVarCount();
        if(!atg.getJCBType().getSelectedItem().equals("Definite")){
            if(atg.getJCBType().getSelectedItem().equals("Indefinite")) result += " some ";
            else result += " every ";
            for(int i = 0; i < atg.getTable().getModel().getRowCount(); i++){
                String cv = "";
                if(atg.getTable().getModel().getValueAt(i, 1) instanceof JComboBox){
                    cv = (String)((JComboBox)atg.getTable().getModel().getValueAt(i, 1)).getSelectedItem();
                }
                else cv = (String)atg.getTable().getModel().getValueAt(i, 1);
                if(cv == null) cv="";
                if (cv.equals("")){
                    result += "w" + varcount++ + " ";
                }
            }
            result += "(";
        }

        varcount = QBEPanel.getTotalVarCount();
        Caseframe cf = (Caseframe)atg.getJCBCF().getSelectedItem();
        ArrayList<Slot> slots = cf.slots;

        if(cf.fsymbols != null) result += atg.getTable().getModel().getValueAt(0, 1).toString();
        else result += cf.toString();

        if (slots.size() > 1){ //form is like (assert '(Isa (setof a c) (setof b d)))
            for(Slot s : slots){
                if(cf.fsymbols != null && s == slots.get(0)) continue;
                result += " (setof ";
                for(int i = 0; i < atg.getTable().getModel().getRowCount(); i++){
                    if(s.name.equals(atg.getTable().getModel().getValueAt(i, 0).toString())){
                        String cv = (String)atg.getTable().getModel().getValueAt(i, 1);
                        if(cv == null) cv="";
                        if(!atg.getJCBType().getSelectedItem().equals("Definite") && cv.equals("")){
                            result += "w" + varcount++ + " ";
                        }
                        else result += atg.getTable().getModel().getValueAt(i, 1) + " ";
                    }
                }
                result += ")";
            }
            if(atg.getJCBAsserted().isSelected()) result += ")";
            else result += ") '" + cf.type + ")";
        }
        else{ //form is like (assert '(and a b c))
            for(Slot s : slots){
                if(cf.fsymbols != null && s == slots.get(0)) continue;
                result += " ";
                for(int i = 0; i < atg.getTable().getModel().getRowCount(); i++){
                    if(s.name.equals(atg.getTable().getModel().getValueAt(i, 0).toString())){
                        String cv = (String)atg.getTable().getModel().getValueAt(i, 1);
                        if(cv == null) cv="";
                        if(!atg.getJCBType().getSelectedItem().equals("Definite") && cv.equals("")){
                            result += "w" + varcount++ + " ";
                        }
                        else result += atg.getTable().getModel().getValueAt(i, 1) + " ";
                    }
                }
                result += ")";
            }
            if(atg.getJCBAsserted().isSelected()) {}//result += ")";
            else result += " '" + cf.type + ")";
        }
        if(!atg.getJCBType().getSelectedItem().equals("Definite")) result += ")";*/
    }

    public String getResult(){
        return result;
    }

}
