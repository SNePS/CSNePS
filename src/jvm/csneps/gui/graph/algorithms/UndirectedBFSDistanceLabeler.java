/*
 * This code adapted from JUNG, which is published under the BSD license.
 */

/**
 * Labels each node in the graph according to the BFS distance from the start node(s). If nodes are
 * unreachable, then they are assigned a distance of -1. All nodes traversed at step k are marked as
 * predecessors of their successors traversed at step k+1.
 *
 * <p>Running time is: O(m)
 *
 * @author Scott White
 */

package csneps.gui.graph.algorithms;

import csneps.gui.graph.IEdge;
import csneps.gui.graph.ITermNode;
import edu.uci.ics.jung.graph.Graph;

import java.util.*;

public class UndirectedBFSDistanceLabeler {
    private Map<ITermNode<IEdge>, Integer> distanceDecorator = new HashMap<ITermNode<IEdge>, Integer>();
    private List<ITermNode<IEdge>> mCurrentList;
    private Set<ITermNode<IEdge>> mUnvisitedNodes;
    private List<ITermNode<IEdge>> mNodesInOrderVisited;
    private Map<ITermNode<IEdge>, HashSet<ITermNode<IEdge>>> mPredecessorMap;

    /**
     * Creates a new BFS labeler for the specified graph and root set The distances are stored in the
     * corresponding Node objects and are of type MutableInteger
     */
    public UndirectedBFSDistanceLabeler() {
        mPredecessorMap = new HashMap<ITermNode<IEdge>, HashSet<ITermNode<IEdge>>>();
    }

    /**
     * Returns the list of nodes visited in order of traversal
     *
     * @return the list of nodes
     */
    public List<ITermNode<IEdge>> getNodesInOrderVisited() {
        return mNodesInOrderVisited;
    }

    /**
     * Returns the set of all nodes that were not visited
     *
     * @return the list of unvisited nodes
     */
    public Set<ITermNode<IEdge>> getUnvisitedNodes() {
        return mUnvisitedNodes;
    }

    /**
     * Given a node, returns the shortest distance from any node in the root set to v
     *
     * @param g the graph in which the distances are to be measured
     * @param v the node whose distance is to be retrieved
     * @return the shortest distance from any node in the root set to v
     */
    public int getDistance(Graph<ITermNode<IEdge>, IEdge> g, ITermNode<IEdge> v) {
        //Preconditions.checkArgument(
        //        g.getVertices().contains(v), "Node %s is not contained in the graph %s", v, g);

        return distanceDecorator.get(v);
    }

    /**
     * Returns set of predecessors of the given node
     *
     * @param v the node whose predecessors are to be retrieved
     * @return the set of predecessors
     */
    public Set<ITermNode<IEdge>> getPredecessors(ITermNode<IEdge> v) {
        return mPredecessorMap.get(v);
    }

    protected void initialize(Collection<ITermNode<IEdge>> vertices, Set<ITermNode<IEdge>> rootSet){
        mNodesInOrderVisited = new ArrayList<ITermNode<IEdge>>();
        mUnvisitedNodes = new HashSet<ITermNode<IEdge>>();
        for (ITermNode<IEdge> currentNode : vertices) {
            mUnvisitedNodes.add(currentNode);
            mPredecessorMap.put(currentNode, new HashSet<ITermNode<IEdge>>());
        }

        mCurrentList = new ArrayList<ITermNode<IEdge>>();
        for (ITermNode<IEdge> v : rootSet) {
            distanceDecorator.put(v, 0);
            mCurrentList.add(v);
            mUnvisitedNodes.remove(v);
            mNodesInOrderVisited.add(v);
        }
    }

    protected void initialize(Graph<ITermNode<IEdge>, IEdge> g, Set<ITermNode<IEdge>> rootSet) {
        initialize(g.getVertices(), rootSet);
    }

    private void addPredecessor(ITermNode<IEdge> predecessor, ITermNode<IEdge> successor) {
        HashSet<ITermNode<IEdge>> predecessors = mPredecessorMap.get(successor);
        predecessors.add(predecessor);
    }

    /**
     * Computes the distances of all the node from the starting root nodes. If there is more than one
     * root node the minimum distance from each root node is used as the designated distance to a
     * given node. Also keeps track of the predecessors of each node traversed as well as the order of
     * nodes traversed.
     *
     * @param graph the graph to label
     * @param rootSet the set of starting nodes to traverse from
     */
    public void labelDistances(Graph<ITermNode<IEdge>, IEdge> graph, Set<ITermNode<IEdge>> rootSet) {

        initialize(graph, rootSet);

        int distance = 1;
        while (true) {
            List<ITermNode<IEdge>> newList = new ArrayList<ITermNode<IEdge>>();
            for (ITermNode<IEdge> currentNode : mCurrentList) {
                for (IEdge next : currentNode.getInEdges()) {
                    visitNewNode(currentNode, next.getFrom(), distance, newList);
                }

                for (IEdge next : currentNode.getOutEdges()) {
                    visitNewNode(currentNode, next.getTo(), distance, newList);
                }
            }
            if (newList.size() == 0) {
                break;
            }
            mCurrentList = newList;
            distance++;
        }

        for (ITermNode<IEdge> v : mUnvisitedNodes) {
            distanceDecorator.put(v, -1);
        }
    }

    /**
     * Computes the distances of all the node from the specified root node. Also keeps track of the
     * predecessors of each node traversed as well as the order of nodes traversed.
     *
     * @param graph the graph to label
     * @param root the single starting node to traverse from
     */
    public void labelDistances(Graph<ITermNode<IEdge>, IEdge> graph, ITermNode<IEdge> root) {
        labelDistances(graph, Collections.singleton(root));
    }

    private void visitNewNode(ITermNode<IEdge> predecessor, ITermNode<IEdge> neighbor, int distance, List<ITermNode<IEdge>> newList) {
        if (mUnvisitedNodes.contains(neighbor)) {
            distanceDecorator.put(neighbor, distance);
            newList.add(neighbor);
            mNodesInOrderVisited.add(neighbor);
            mUnvisitedNodes.remove(neighbor);
        }
        int predecessorDistance = distanceDecorator.get(predecessor);
        int successorDistance = distanceDecorator.get(neighbor);
        if (predecessorDistance < successorDistance) {
            addPredecessor(predecessor, neighbor);
        }
    }

    /**
     * Must be called after {@code labelDistances} in order to contain valid data.
     *
     * @return a map from nodes to minimum distances from the original source(s)
     */
    public Map<ITermNode<IEdge>, Integer> getDistanceDecorator() {
        return distanceDecorator;
    }
}
