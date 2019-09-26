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

import edu.buffalo.cse.sneps3.gui.business.Slot;
import edu.buffalo.cse.sneps3.gui.business.Term;

import javax.swing.*;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;

import edu.buffalo.cse.sneps3.gui.graph.IEdge;
import edu.buffalo.cse.sneps3.gui.graph.ITermNode;
import edu.buffalo.cse.sneps3.gui.graph.SnepsGraph;
import edu.buffalo.cse.sneps3.gui.graph.algorithms.UndirectedDijkstraDistance;
import edu.buffalo.cse.sneps3.gui.graph.algorithms.UndirectedDijkstraShortestPath;
import edu.buffalo.cse.sneps3.gui.graph.algorithms.UndirectedUnweightedShortestPath;
import edu.uci.ics.jung.algorithms.shortestpath.DijkstraDistance;
import edu.uci.ics.jung.algorithms.shortestpath.Distance;
import edu.uci.ics.jung.algorithms.shortestpath.DistanceStatistics;

/**
 *
 * @author dan
 */
public class NodeFindPath extends JPanel {

    private JLabel jLabel_node1;
    private JLabel jLabel_node2;
    private javax.swing.JTextArea jTextArea1;
    private JTextField jTextField_node1;
    private JTextField jTextField_node2;

    // Show context.
    private JLabel jLabel_beginContextLabel;
    private JLabel jLabel_endContextLabel;
    private JTextField jTextField_contextDegree;

    public NodeFindPath() {
        initComponents();
    }

    protected Set<Term> getContextTerms(int depth, Set<Term> collected){
    	Set<Term> added = new HashSet<>();

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
        Term term1 = Term.getTerm(jTextField_node1.getText());
        Term term2 = Term.getTerm(jTextField_node2.getText());
        if (term1 == null){
            JOptionPane.showMessageDialog(this, "Node with the label: " + jTextField_node1.getText() + " was not found.");
        }
        if (term2 == null){
            JOptionPane.showMessageDialog(this, "Node with the label: " + jTextField_node2.getText() + " was not found.");
        }


        // TODO: Make path searching undirected.
        // TODO: Allow searching notes not currently visible.

        SnepsGraph<ITermNode<IEdge>, IEdge> dsg = GUI2.getInstance().getGraph();
        //UndirectedUnweightedShortestPath sp = new UndirectedUnweightedShortestPath(GUI2.getInstance().getGraph());

        UndirectedDijkstraDistance dd = new UndirectedDijkstraDistance(GUI2.getInstance().getGraph());
        System.out.println("Distance: " + dd.getDistance(dsg.getVertex(jTextField_node1.getText()), dsg.getVertex(jTextField_node2.getText())));

        //System.out.println("AVG: " + DistanceStatistics.diameter(dsg));
        UndirectedDijkstraShortestPath dsp = new UndirectedDijkstraShortestPath(dsg);
        List<IEdge> path = dsp.getPath(dsg.getVertex(jTextField_node1.getText()), dsg.getVertex(jTextField_node2.getText()));


        Set<Term> nns = new HashSet<Term>();

        for (IEdge e : path){
            nns.add(e.getFrom().getTerm());
            nns.add(e.getTo().getTerm());
        }

        if(GUI2.DEBUG) System.out.println("Node Find: " + nns);

        if(GUI2.getInstance().getGraphPanel().isShowingAll()){
            GUI2.getInstance().getGraphPanel().displayOnlyTermSet(nns);
        }
        else GUI2.getInstance().getGraphPanel().displayTermSet(nns);
        return true;
    }

    private void initComponents() {

        GroupLayout layout = new GroupLayout(this);

        jLabel_node1 = new JLabel();
        jLabel_node2 = new JLabel();
        jTextField_node1 = new JTextField();
        jTextField_node2 = new JTextField();
        jTextArea1 = new javax.swing.JTextArea();

        jLabel_node1.setText("Node 1:");
        jLabel_node2.setText("Node 2:");

        jTextArea1.setBackground(javax.swing.UIManager.getDefaults().getColor("Panel.background"));
        jTextArea1.setColumns(20);
        jTextArea1.setEditable(false);
        jTextArea1.setLineWrap(true);
        jTextArea1.setRows(2);
        jTextArea1.setText("You may enter a single node name or a space \nseparated list of node names.");

        layout.setHorizontalGroup(layout.createSequentialGroup()
                .addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING)
                        .addComponent(jLabel_node1)
                        .addComponent(jLabel_node2))
                .addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING)
                        .addComponent(jTextField_node1)
                        .addComponent(jTextField_node2)));

        layout.setVerticalGroup(layout.createSequentialGroup()
                .addGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                        .addComponent(jLabel_node1)
                        .addComponent(jTextField_node1))
                .addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING)
                        .addComponent(jLabel_node2)
                        .addComponent(jTextField_node2)));

        this.setLayout(layout);
    }
}
