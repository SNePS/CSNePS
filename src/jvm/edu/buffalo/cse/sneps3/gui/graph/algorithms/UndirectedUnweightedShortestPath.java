/*
 * This code adapted from JUNG, which is published under the BSD license.
 */

package edu.buffalo.cse.sneps3.gui.graph.algorithms;

import edu.buffalo.cse.sneps3.gui.graph.IEdge;
import edu.buffalo.cse.sneps3.gui.graph.ITermNode;
import edu.uci.ics.jung.algorithms.shortestpath.Distance;
import edu.uci.ics.jung.graph.Graph;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class UndirectedUnweightedShortestPath implements Distance<ITermNode<IEdge>> {
    /**
     * Computes the shortest path distances for graphs whose edges are not weighted (using BFS).
     *
     * @author Scott White
     */
// TODO: refactor to make this (much!) more efficient

        private Map<ITermNode<IEdge>, Map<ITermNode<IEdge>, Number>> mDistanceMap;
        private Map<ITermNode<IEdge>, Map<ITermNode<IEdge>, ITermNode<IEdge>>> mPredecessorMap;
        private Graph<ITermNode<IEdge>, IEdge> mGraph;
        private Map<ITermNode<IEdge>, Integer> distances = new HashMap<ITermNode<IEdge>, Integer>();

        /**
         * Constructs and initializes algorithm
         *
         * @param g the graph
         */
        public UndirectedUnweightedShortestPath(Graph<ITermNode<IEdge>, IEdge> g) {
            mDistanceMap = new HashMap<>();
            mPredecessorMap = new HashMap<>();
            mGraph = g;
        }

        /** @see edu.uci.ics.jung.algorithms.shortestpath.Distance#getDistance(Object, Object) */
        public Number getDistance(ITermNode<IEdge> source, ITermNode<IEdge> target) {
            Map<ITermNode<IEdge>, Number> sourceSPMap = getDistanceMap(source);
            return sourceSPMap.get(target);
        }

        /** @see edu.uci.ics.jung.algorithms.shortestpath.Distance#getDistanceMap(Object) */
        public Map<ITermNode<IEdge>, Number> getDistanceMap(ITermNode<IEdge> source) {
            Map<ITermNode<IEdge>, Number> sourceSPMap = mDistanceMap.get(source);
            if (sourceSPMap == null) {
                computeShortestPathsFromSource(source);
                sourceSPMap = mDistanceMap.get(source);
            }
            return sourceSPMap;
        }

        /** @see edu.uci.ics.jung.algorithms.shortestpath.ShortestPath#getIncomingEdgeMap(Object) */
        public Map<ITermNode<IEdge>, ITermNode<IEdge>> getIncomingEdgeMap(ITermNode<IEdge> source) {
            Map<ITermNode<IEdge>, ITermNode<IEdge>> sourceIEMap = mPredecessorMap.get(source);
            if (sourceIEMap == null) {
                computeShortestPathsFromSource(source);
                sourceIEMap = mPredecessorMap.get(source);
            }
            return sourceIEMap;
        }

        /**
         * Computes the shortest path distances from a given node to all other nodes.
         *
         * @param source the source node
         */
        private void computeShortestPathsFromSource(ITermNode<IEdge> source) {
            UndirectedBFSDistanceLabeler labeler = new UndirectedBFSDistanceLabeler();
            labeler.labelDistances(mGraph, source);
            distances = labeler.getDistanceDecorator();
            Map<ITermNode<IEdge>, Number> currentSourceSPMap = new HashMap<ITermNode<IEdge>, Number>();
            Map<ITermNode<IEdge>, ITermNode<IEdge>> currentSourcePredMap = new HashMap<ITermNode<IEdge>, ITermNode<IEdge>>();

            for (ITermNode<IEdge> node : mGraph.getVertices()) {

                Integer distanceVal = distances.get(node);
                // BFSDistanceLabeler uses -1 to indicate unreachable nodes;
                // don't bother to store unreachable nodes
                if (distanceVal != null && distanceVal.intValue() >= 0) {
                    currentSourceSPMap.put(node, distanceVal);
                    int minDistance = distanceVal.intValue();

                    for (IEdge neighbor : node.getInEdges()) {
                        if (neighbor.getFrom().equals(node)) {
                            continue;
                        }

                        Integer predDistance = distances.get(neighbor.getFrom());
                        if (predDistance < minDistance && predDistance >= 0) {
                            minDistance = predDistance.intValue();
                            currentSourcePredMap.put(node, neighbor.getFrom());
                        }
                    }

                    for (IEdge neighbor : node.getOutEdges()) {
                        if (neighbor.getTo().equals(node)) {
                            continue;
                        }

                        Integer predDistance = distances.get(neighbor.getTo());
                        if (predDistance < minDistance && predDistance >= 0) {
                            minDistance = predDistance.intValue();
                            currentSourcePredMap.put(node, neighbor.getTo());
                        }
                    }
                }
            }
            mDistanceMap.put(source, currentSourceSPMap);
            mPredecessorMap.put(source, currentSourcePredMap);
        }

        /**
         * Clears all stored distances for this instance. Should be called whenever the graph is modified
         * (edge weights changed or edges added/removed). If the user knows that some currently calculated
         * distances are unaffected by a change, <code>reset(V)</code> may be appropriate instead.
         *
         */
        public void reset() {
            mDistanceMap.clear();
            mPredecessorMap.clear();
        }

        /**
         * Clears all stored distances for the specified source node <code>source</code>. Should be called
         * whenever the stored distances from this node are invalidated by changes to the graph.
         *
         * @see #reset()
         * @param v the node for which distances should be cleared
         */
        public void reset(ITermNode<IEdge> v) {
            mDistanceMap.remove(v);
            mPredecessorMap.remove(v);
        }
    }
