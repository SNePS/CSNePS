/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.buffalo.cse.sneps3.gui;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;

import edu.buffalo.cse.sneps3.gui.business.Caseframe;
import edu.buffalo.cse.sneps3.gui.business.Slot;

/**
 *
 * @author dan
 *
 * A frame is an instance of a caseframe.
 */
public class Frame {

    public enum Quantification {None, Arbitrary, Indefinite};
    public static Map<Quantification, String> quantstr = new HashMap<Quantification, String>() {
        {
            put(Quantification.None, "");
            put(Quantification.Arbitrary, "every");
            put(Quantification.Indefinite, "some");
        };
    };

    private Caseframe cf;
    private ArrayList<PairLR<Slot,Object>> sf;
    private Quantification quant;
    private String varName;
    private String depVar;
    private String fsym;

    public Frame(Caseframe cf, String fsym, ArrayList<Slot> slots, ArrayList<Object> fillers, Quantification quant, String varName, String depVar){
        this(cf, slots, fillers, quant, varName, depVar);
        this.fsym = fsym;
    }

    public Frame(Caseframe cf, ArrayList<Slot> slots, ArrayList<Object> fillers, Quantification quant, String varName, String depVar){
        this(cf, slots, fillers);
        this.quant = quant;
        this.varName = varName;
        this.depVar = depVar;
    }

    public Frame(Caseframe cf, String fsym, ArrayList<Slot> slots, ArrayList<Object> fillers){
        this(cf, slots, fillers);
        this.fsym = fsym;
    }

    public Frame(Caseframe cf, ArrayList<Slot> slots, ArrayList<Object> fillers){
        this.cf = cf;
        sf = new ArrayList<PairLR<Slot,Object>>();
        for(int i = 0; i < slots.size(); i++){
            sf.add(new PairLR<Slot, Object>(slots.get(i), fillers.get(i)));
        }
        this.quant = Quantification.None;
    }

    public Quantification getQuantification(){
        return quant;
    }

    public String getVariable(){
        return varName;
    }

    public String getDepVar(){
        return depVar;
    }

    private ArrayList<PairLR<Slot, Object>> getAll(Slot s){
        ArrayList<PairLR<Slot,Object>> ss = new ArrayList<PairLR<Slot,Object>>();
        for(PairLR<Slot, Object> p : sf){
            if(p.getLeft() == s) ss.add(p);
        }
        return ss;
    }

    private ArrayList<PairLR<Slot, Object>> getVarShare(Frame f, ArrayList<PairLR<Slot, Object>> slot){
        ArrayList<PairLR<Slot, Object>> out = new ArrayList<PairLR<Slot, Object>>();
        for(PairLR<Slot, Object> p : slot)
            if(p.getRight() instanceof Frame)
                if(((Frame)p.getRight()).getVariable().equals(f.getVariable()))
                    //if(((Frame)p.getRight()).getDepVar() == f.getDepVar() || ((Frame)p.getRight()).getDepVar().equals(f.getDepVar()))
                    if(((Frame) p.getRight()).getDepVar() == null ? f.getDepVar() == null : ((Frame) p.getRight()).getDepVar().equals(f.getDepVar()))
                        out.add(p);
        return out;
    }

    private String buildVarString(Frame f){
        if(f.quant == Quantification.None) return "";
        else if(f.quant == Quantification.Arbitrary) return "(every " + f.getVariable();
        else return "(some " + f.getVariable() + "(" + f.getDepVar() + ") ";
    }

    private String buildFillers(ArrayList<PairLR<Slot, Object>> slot, boolean setof){
        String output = "";
        //If we aren't doing setof, it could be (and a b c)
        if(!setof || slot.size()==1){
            for(PairLR<Slot, Object> p : slot){
                Object o = p.getRight();
                if(o instanceof Frame)
                    output += " " + ((Frame)o).toString();
                else output += " " + o.toString();
            }
        }
        //For each item in the array look through the remaining items for ones which share a 
        //variable.
        else{
            ArrayList<PairLR<Slot, Object>> done = new ArrayList<PairLR<Slot, Object>>();
            output += " (setof ";
            for(int i = 0; i < slot.size(); i++){
                Object o = slot.get(i).getRight();
                if(done.contains(o)) continue;
                if(o instanceof Frame){
                    Frame f = (Frame)o;
                    Quantification q = f.getQuantification();
                    if(q != Quantification.None){
                        ArrayList<PairLR<Slot, Object>> shared = getVarShare(f, slot);
                        done.addAll(shared);
                        output += buildVarString(f);
                        for(PairLR<Slot, Object> p : shared)
                            output += ((Frame)p.getRight()).buildNoQuantString();
                        output += ")";
                    }
                    else output += f.toString();
                }
                else output += o.toString();
            }
            output += ")";
        }
        return output;
    }

    protected String buildNoQuantString(){
        String out = cf.getName();
        for(Slot s : cf.getSlots()){
            if(cf.getSlots().size() > 1)
                out += buildFillers(getAll(s), true);
            else out += buildFillers(getAll(s), false);
        }


        return "";
    }

    @Override
    public String toString(){
        //First sort by slot name. This sort puts them in the order of the cf, but grouped.
        /*Collections.sort(sf, new Comparator<PairLR>(){

            public int compare(PairLR o1, PairLR o2) {
                if(o1.getLeft().toString().equals(o2.getLeft().toString())) return 0;
                if(cf.slots.indexOf(o1) < cf.slots.indexOf(o2)) return -1;
                else return 1;


                //return o1.getLeft().toString().compareTo(o2.getLeft().toString());
            }
        });*/

        return buildVarString(this) + buildNoQuantString() + ")";
    }
}
