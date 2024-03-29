/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package csneps.gui.graph;

import csneps.gui.business.Context;
import csneps.gui.business.FnInterop;
import csneps.gui.CaseframeBasedShowHideDialog;
import csneps.gui.GUI2;
import csneps.gui.business.Term;
import edu.uci.ics.jung.algorithms.layout.GraphElementAccessor;
import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;
import javax.swing.AbstractAction;
import javax.swing.JPopupMenu;

/**
 *
 * @author dan
 */
public class SnepsModalGraphMouse<V, E> extends DefaultModalGraphMouse<V, E> implements MouseListener, MouseMotionListener {

    JPopupMenu popup = new JPopupMenu();

    public SnepsModalGraphMouse() {
        super();
    }

    
    
    
    /**
     * Add support for right clicking verticies to expand them.
     * @param e
     */
    @Override
    public void mousePressed(MouseEvent e) {
        super.mousePressed(e);
        GUI2.getInstance().getGraphPanel().highlightedNodes.clear();
        if (e.getButton() != MouseEvent.BUTTON3) {
            popup.setVisible(false);
        }
        if (e.getButton() == MouseEvent.BUTTON3) {
            final VisualizationViewer<V, E> vv = (VisualizationViewer<V,E>) e.getSource();
            GraphElementAccessor<V, E> pickSupport = vv.getPickSupport();
            Layout<V, E> layout = vv.getGraphLayout();
            Point2D loc = e.getPoint();
            V vertex = pickSupport.getVertex(layout, loc.getX(), loc.getY());
            final TermNode node = (TermNode) vertex;
            if (vertex == null) {
                popup.setVisible(false);
                return;
            } else {
                popup.setVisible(false);
                popup.removeAll();



                int dncsvis = node.getDownCablesetVisibleCount();
                int upcsvis = node.getUpCablesetVisibleCount();
                Set<ITermNode<IEdge>> pickedNodes = GUI2.getInstance().getGraphPanel().getVV().getPickedVertexState().getPicked();

                Term term = node.getTerm();

                if(GUI2.DEBUG)
                    System.out.println("UpCSVis: " + upcsvis + " DnCSVis " + dncsvis);

                if (upcsvis < node.getInEdges().size()) {
                    popup.add(new AbstractAction("Show All In Edges (" + (node.getInEdges().size() - upcsvis) + " edges)") {

                        public void actionPerformed(ActionEvent e) {
                        	GUI2.getInstance().getGraph().showInEdges(node);
                        	vv.repaint();
                            //JungGraphPanel.instance.showUpCableset(node);
                        }
                    });

                    popup.add(new AbstractAction("Show In Edges By Relation") {

                        public void actionPerformed(ActionEvent e) {
                        	CaseframeBasedShowHideDialog cd =
                        			new CaseframeBasedShowHideDialog(GUI2.getInstance(), new ArrayList<String>(GUI2.getInstance().getGraph().getInHiddenFSymbols(node)));

                            cd.setHelpText("   Select the relations you        wish to show in the graph.");
                            cd.setVisible(true);

                            for (String fsym : cd.getResult()) {
                            	GUI2.getInstance().getGraph().showInEdges(node, fsym);
                            }

                            vv.repaint();
                        }
                    });

                    /*JMenu submenu = new JMenu("Show In Relations");
                    final HashMap<Caseframe, ArrayList<Edge>> hm = JungGraphPanel.instance.getHiddenUpCablesetCfs(node);
                    for(final Caseframe cf : hm.keySet()){
                    submenu.add(new AbstractAction(cf.toString()) {
                    public void actionPerformed(ActionEvent e) {
                    for(Edge je : hm.get(cf))
                    JungGraphPanel.instance.showNode(je.from);
                    }
                    });
                    }
                    popup.add(submenu);*/
                }
                if (dncsvis < node.getOutEdges().size()) {
                    popup.add(new AbstractAction("Show All Out Edges (" + (node.getOutEdges().size() - dncsvis) + " edges)") {

                        public void actionPerformed(ActionEvent e) {
                        	GUI2.getInstance().getGraph().showOutEdges(node);
                        	vv.repaint();
                        }
                    });
                }

                if (upcsvis > 0) {
                    popup.add(new AbstractAction("Hide All In Edges (" + upcsvis + " edges)") {

                        public void actionPerformed(ActionEvent e) {
                        	GUI2.getInstance().getGraph().hideInEdges(node);
                        	vv.repaint();
                        }
                    });



                    popup.add(new AbstractAction("Hide In Edges By Relation") {

                        public void actionPerformed(ActionEvent e) {
                        	CaseframeBasedShowHideDialog cd =
                        			new CaseframeBasedShowHideDialog(GUI2.getInstance(), new ArrayList<String>(GUI2.getInstance().getGraph().getInShownFSymbols(node)));

                            cd.setHelpText("   Select the relations you       wish to hide from the graph.");
                            cd.setVisible(true);

                            for (String fsym : cd.getResult()) {
                            	GUI2.getInstance().getGraph().hideInEdges(node, fsym);
                            }

                            vv.repaint();
                        }
                    });

                }
                if (dncsvis > 0 && !node.getTerm().isMolecular()) {
                    popup.add(new AbstractAction("Hide All Out Edges (" + dncsvis + " edges)") {

                        public void actionPerformed(ActionEvent e) {
                        	GUI2.getInstance().getGraph().hideOutEdges(node);
                            //JungGraphPanel.instance.hideDownCableset(node);
                        	vv.repaint();
                        }
                    });
                }

                if(pickedNodes.contains(node) && pickedNodes.size() > 1){
                    popup.add(new AbstractAction("Hide Selected Nodes") {
                        public void actionPerformed(ActionEvent e) {
                            for (ITermNode pnode : pickedNodes ) {
                                GUI2.getInstance().getGraph().hideVertex(pnode);
                            }
                            vv.repaint();
                        }
                    });
                }
                else{
                    popup.add(new AbstractAction("Hide Node") {
                        public void actionPerformed(ActionEvent e) {
                            GUI2.getInstance().getGraph().hideVertex(node);
                            vv.repaint();
                        }
                    });
                }

                if (!term.isAsserted()){
                    // Propositions and things that can be lowered in type to propositions can be asserterd.
                    if(term.getSyntacticType().equals("Proposition")
                            || term.getSyntacticType().equals("Propositional")
                            || term.getSyntacticType().equals("Entity")) {
                        popup.add(new AbstractAction("Assert") {
                            public void actionPerformed(ActionEvent e) {
                                FnInterop.addToContext(node.getTerm(), Context.getCurrentContext());
                                GUI2.getInstance().getGraphPanel().getVV().repaint();
                            }
                        });
                    }

                    // CARules can be adopted.
                    if(term.getSyntacticType().equals("CARule")){
                        popup.add(new AbstractAction("Adopt") {
                            public void actionPerformed(ActionEvent e) {
                                FnInterop.adoptRule(term);
                                GUI2.getInstance().getGraphPanel().getVV().repaint();
                            }
                        });
                    }
                }

                if(node.getTerm().isAsserted()) {
                    // Propositions and things that can be lowered in type to propositions can be unasserterd.
                    if (term.getSyntacticType().equals("Proposition")
                            || term.getSyntacticType().equals("Propositional")
                            || term.getSyntacticType().equals("Entity")) {
                        popup.add(new AbstractAction("Unassert") {
                            public void actionPerformed(ActionEvent e) {
                                FnInterop.unassertTerm(node.getTerm());
                                GUI2.getInstance().getGraphPanel().getVV().repaint();
                            }
                        });
                    }

                    // CARules can be unadopted.
                    if(term.getSyntacticType().equals("CARule")){
                        popup.add(new AbstractAction("Unadopt") {
                            public void actionPerformed(ActionEvent e) {
                                FnInterop.unadoptRule(term);
                                GUI2.getInstance().getGraphPanel().getVV().repaint();
                            }
                        });
                    }
                }

                popup.show(vv, e.getX(), e.getY());
            }
        }
    }

    /**
     * Add support for mouse over popups.
     * @param e
     */
    @Override
    public void mouseMoved(MouseEvent e) {
        super.mouseMoved(e);
        VisualizationViewer<V, E> vv = (VisualizationViewer<V, E>) e.getSource();
        GraphElementAccessor<V, E> pickSupport = vv.getPickSupport();
        Layout<V, E> layout = vv.getGraphLayout();
        Point2D loc = e.getPoint();
        V vertex = pickSupport.getVertex(layout, loc.getX(), loc.getY());
        final ITermNode node = (ITermNode) vertex;
        if (vertex == null) {
            if (!GUI2.getInstance().getGraphPanel().getShowNewTerms()) {
                GUI2.getInstance().getGraphPanel().setStatusbarText("New assertions are not shown in the graph.");
            } else {
                GUI2.getInstance().getGraphPanel().setStatusbarText("");
            }
            return;
        }
        String sbtext = "   ";

        if(node.getTerm().isMolecular())
            sbtext += node.getTerm().toString();
        else 
        	sbtext += node.toString();
        
        if(!node.getInEdges().isEmpty()){
            sbtext += " is in relation" + (node.getInEdges().size() == 1 ? "" : "s") + ": " + new HashSet(node.getRelationsPartOf()); //Use a HashSet to eliminate duplicates.
        }
        GUI2.getInstance().getGraphPanel().setStatusbarText(sbtext);
    }
}
