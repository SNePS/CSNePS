/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/*
 * RDockPanel.java
 *
 * Created on Jan 15, 2010, 9:32:34 PM
 */

package edu.buffalo.cse.sneps3.gui;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.GridLayout;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.util.ArrayList;
import javax.swing.JButton;
import javax.swing.JPanel;

import org.jdesktop.swingx.JXMultiSplitPane;
import org.jdesktop.swingx.MultiSplitLayout;

import edu.buffalo.cse.sneps3.gui.dataaccess.Model;

/**
 *
 * @author dan
 */
public class RDockPanel extends javax.swing.JPanel implements ComponentListener{

    Model model;
    GUI2 parent;

    RDockComponent semantic; 
    SemanticTypesPanel stPanel = new SemanticTypesPanel();

    RDockComponent cf;
    CaseframesPanel cfPanel;

    RDockComponent context;
    ContextsPanel ctPanel;

    int componentCt = 0;
    ArrayList<RDockComponent> components = new ArrayList<RDockComponent>();

    JXMultiSplitPane mp = new JXMultiSplitPane();


    /** Creates new form RDockPanel */
    public RDockPanel() {
        initComponents();
        //this.setSize(250, 400);
        //this.setLayout(new GridLayout(2,1));

        this.removeAll();
        //this.setLayout(new BorderLayout());
        //this.setSize(this.getPreferredSize());
        this.add(new JButton("Hewrwerq kerjawe;rj;werj;werweri"));
    }
    
    public void setModel(Model m){
        model = m;
        //initRest2();
//        String layoutDef =
//    "(COLUMN top bottom)";
//MultiSplitLayout.Node modelRoot = MultiSplitLayout.parseModel(layoutDef);
//
//MultiSplitPane multiSplitPane = new MultiSplitPane();
//multiSplitPane.getMultiSplitLayout().setModel(modelRoot);
//multiSplitPane.add(new JButton("Left Column"), "top");
//multiSplitPane.add(new JButton("Bottom Row"), "bottom");
//this.add(multiSplitPane, BorderLayout.CENTER);
    }

    public void setParent(GUI2 p){
        parent = p;
    }

    public void initRest(){
        //Semantic Types
        semantic = new RDockComponent();
        //RelationFrame rl = new RelationFrame(null, null, 5, 5, 5, 5);
        semantic.setComponentName("Semantic Types");
        semantic.setLocation(0, 0);
        semantic.setSize(100, 200);
        semantic.setComponent(stPanel);
        //stPanel.setParent(parent);
        this.add(semantic);
        model.registerView(stPanel);


        //Caseframes
        //cfPanel = new CaseframesPanel(model, parent);
        cf = new RDockComponent();
        cf.setComponentName("Caseframes");
        cf.setLocation(0, semantic.getHeight());
        cf.setSize(100,200);
        cf.setComponent(cfPanel);
        this.add(cf);


        //Contexts
        ctPanel = new ContextsPanel();
        context = new RDockComponent();
        context.setComponentName("Contexts");
        context.setLocation(0, semantic.getHeight()+cf.getHeight());
        context.setSize(100,200);
        context.setComponent(ctPanel);
        //ctPanel.setParent(parent);
        this.add(context);
        model.registerView(ctPanel);

    }

    public void initRest2(){
                //Semantic Types
        semantic = new RDockComponent();
        //RelationFrame rl = new RelationFrame(null, null, 5, 5, 5, 5);
        semantic.setComponentName("Semantic Types");
        //semantic.setLocation(0, 0);
        semantic.setSize(100, 200);
        semantic.setComponent(stPanel);
        //stPanel.setParent(parent);
        this.addRDockComponent(semantic);
        model.registerView(stPanel);


        //Caseframes
        //cfPanel = new CaseframesPanel(model, parent);
        cf = new RDockComponent();
        cf.setComponentName("Caseframes");
        //cf.setLocation(0, semantic.getHeight());
        cf.setSize(100,200);
        cf.setComponent(cfPanel);
        this.addRDockComponent(cf);


        //Contexts
        ctPanel = new ContextsPanel();
        context = new RDockComponent();
        context.setComponentName("Contexts");
        //context.setLocation(0, semantic.getHeight()+cf.getHeight());
        context.setSize(100,200);
        context.setComponent(ctPanel);
        //ctPanel.setParent(parent);
        this.addRDockComponent(context);
        model.registerView(ctPanel);
    }

    //public void addComponent(JPanel p){
    //    p.addComponentListener((ComponentListener) this);
    //}

    public void addRDockComponent(RDockComponent c){
        components.add(c);
        String layoutDef = "(COLUMN ";
        for(int i = 0 ; i < components.size(); i++) layoutDef += "c"+i+" ";
        layoutDef += ")";
        MultiSplitLayout.Node modelRoot = MultiSplitLayout.parseModel(layoutDef);
        mp = new JXMultiSplitPane();
        mp.getMultiSplitLayout().setModel(modelRoot);
        for(int i = 0 ; i < components.size(); i++){
            mp.add(components.get(i), "c"+i);
        }
        this.removeAll();
        this.add(mp, BorderLayout.CENTER);
    }


    //@Override
    //public Component add(Component comp){
    //   super.add(comp);
    //   if(comp instanceof RDockComponent){
    //       System.out.println(this.getWidth() + " " + this.getHeight());
    //        comp.setSize(this.getWidth(), comp.getHeight());
    //        comp.addComponentListener(this);
    //    }
    //    return comp;
    //}


    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 231, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 300, Short.MAX_VALUE)
        );
    }// </editor-fold>//GEN-END:initComponents

    public void componentResized(ComponentEvent arg0) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public void componentMoved(ComponentEvent arg0) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public void componentShown(ComponentEvent arg0) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public void componentHidden(ComponentEvent arg0) {
        throw new UnsupportedOperationException("Not supported yet.");
    }


    // Variables declaration - do not modify//GEN-BEGIN:variables
    // End of variables declaration//GEN-END:variables

}
