/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/*
 * NodeFind.java
 *
 * Created on Nov 11, 2011, 12:47:53 PM
 */

package csneps.gui;

import csneps.gui.business.Slot;
import csneps.gui.graph.IEdge;
import csneps.gui.graph.ITermNode;
import csneps.gui.graph.SnepsGraph;
import csneps.gui.graph.algorithms.UndirectedDijkstraDistance;
import csneps.gui.graph.algorithms.UndirectedDijkstraShortestPath;
import csneps.gui.business.Term;

import javax.swing.*;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static javax.swing.LayoutStyle.ComponentPlacement.RELATED;

/**
 *
 * @author dan
 */
public class NodeFindPath extends JPanel {

    private JLabel jLabel_node1;
    private JLabel jLabel_node2;
    private javax.swing.JTextArea jTextArea_Directions;
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
        SnepsGraph<ITermNode<IEdge>, IEdge> dsg = GUI2.getInstance().getGraph();

        // Verify node existence.
        ITermNode<IEdge> node1 = dsg.getVertex(jTextField_node1.getText());
        ITermNode<IEdge> node2 = dsg.getVertex(jTextField_node2.getText());

        if (node1 == null){
            JOptionPane.showMessageDialog(this, "Node with the label: " + jTextField_node1.getText() + " was not found.");
        }
        if (node2 == null){
            JOptionPane.showMessageDialog(this, "Node with the label: " + jTextField_node2.getText() + " was not found.");
        }

        if(GUI2.DEBUG && node1 != null && node2 != null){
            UndirectedDijkstraDistance dd = new UndirectedDijkstraDistance(GUI2.getInstance().getGraph());
            System.out.println("Distance: " + dd.getDistance(node1, node2));
        }

        // Compute and show shortest path.
        if (node1 != null && node2 != null){
            UndirectedDijkstraShortestPath dsp = new UndirectedDijkstraShortestPath(dsg);
            List<IEdge> path = dsp.getPath(node1, node2);

            Set<Term> nns = new HashSet<Term>();

            for (IEdge e : path){
                nns.add(e.getFrom().getTerm());
                nns.add(e.getTo().getTerm());
            }

            if(GUI2.DEBUG) System.out.println("Path : " + nns);

            if(GUI2.getInstance().getGraphPanel().isShowingAll()){
                GUI2.getInstance().getGraphPanel().displayOnlyTermSet(nns);
            }
            else GUI2.getInstance().getGraphPanel().displayTermSet(nns);
            return true;
        }
        return false;
    }

    private void initComponents() {

        GroupLayout layout = new GroupLayout(this);

        jLabel_node1 = new JLabel();
        jLabel_node2 = new JLabel();
        jTextField_node1 = new JTextField();
        jTextField_node2 = new JTextField();
        jTextArea_Directions = new javax.swing.JTextArea();

        jLabel_node1.setText("Node 1:");
        jLabel_node2.setText("Node 2:");

        jTextArea_Directions.setBackground(javax.swing.UIManager.getDefaults().getColor("Panel.background"));
        jTextArea_Directions.setColumns(20);
        jTextArea_Directions.setEditable(false);
        jTextArea_Directions.setLineWrap(true);
        //jTextArea1.setRows(2);
        jTextArea_Directions.setText("Enter the nodes to find the path between.");

        layout.setHorizontalGroup(layout.createParallelGroup()
                        .addComponent(jTextArea_Directions)
                        .addGroup(layout.createSequentialGroup()
                            .addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING)
                                .addComponent(jLabel_node1)
                                .addComponent(jLabel_node2))
                            .addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING)
                                .addComponent(jTextField_node1)
                                .addComponent(jTextField_node2))));

        layout.setVerticalGroup(layout.createSequentialGroup()
                .addComponent(jTextArea_Directions)
                .addGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                        .addComponent(jLabel_node1)
                        .addComponent(jTextField_node1))
                .addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING)
                        .addComponent(jLabel_node2)
                        .addComponent(jTextField_node2))
                        .addPreferredGap(RELATED,
                                260, Short.MAX_VALUE)
                );

        //layout.linkSize(jTextField_node1, jTextField_node2);
        layout.setAutoCreateGaps(true);
        layout.setAutoCreateContainerGaps(true);

        this.setLayout(layout);
    }
}
