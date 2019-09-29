/*
 * This code adapted from JUNG, which is published under the BSD license.
 */

package edu.buffalo.cse.sneps3.gui.graph.algorithms;

import com.google.common.base.Function;
import com.google.common.base.Functions;
import edu.buffalo.cse.sneps3.gui.graph.IEdge;
import edu.buffalo.cse.sneps3.gui.graph.ITermNode;
import edu.buffalo.cse.sneps3.gui.graph.SnepsGraph;
import edu.uci.ics.jung.algorithms.shortestpath.Distance;
import edu.uci.ics.jung.algorithms.util.BasicMapEntry;
import edu.uci.ics.jung.algorithms.util.MapBinaryHeap;

import java.util.*;

public class UndirectedDijkstraDistance implements Distance<ITermNode<IEdge>> {
    protected SnepsGraph<ITermNode<IEdge>, IEdge> g;
    protected Function<? super IEdge, ? extends Number> nev;
    protected Map<ITermNode<IEdge>, UndirectedDijkstraDistance.SourceData> sourceMap;
    protected boolean cached;
    protected double max_distance;
    protected int max_targets;

    public UndirectedDijkstraDistance(SnepsGraph<ITermNode<IEdge>, IEdge> g, Function<? super IEdge, ? extends Number> nev, boolean cached) {
        this.g = g;
        this.nev = nev;
        this.sourceMap = new HashMap();
        this.cached = cached;
        this.max_distance = 1.0D / 0.0;
        this.max_targets = 2147483647;
    }

    public UndirectedDijkstraDistance(SnepsGraph<ITermNode<IEdge>, IEdge> g, Function<? super IEdge, ? extends Number> nev) {
        this(g, nev, true);
    }

    public UndirectedDijkstraDistance(SnepsGraph<ITermNode<IEdge>, IEdge> g) {
        this(g, Functions.constant(1), true);
    }

    public UndirectedDijkstraDistance(SnepsGraph<ITermNode<IEdge>, IEdge> g, boolean cached) {
        this(g, Functions.constant(1), cached);
    }

    protected LinkedHashMap<ITermNode<IEdge>, Number> singleSourceShortestPath(ITermNode<IEdge> source, Collection<ITermNode<IEdge>> targets, int numDests) {
        UndirectedDijkstraDistance.SourceData sd = this.getSourceData(source);
        Set<ITermNode<IEdge>> to_get = new HashSet();
        if (targets != null) {
            to_get.addAll(targets);
            Set<ITermNode<IEdge>> existing_dists = sd.distances.keySet();
            Iterator<ITermNode<IEdge>> var7 = targets.iterator();

            while(var7.hasNext()) {
                ITermNode<IEdge> o = var7.next();
                if (existing_dists.contains(o)) {
                    to_get.remove(o);
                }
            }
        }

        if (!sd.reached_max && (targets == null || !to_get.isEmpty()) && sd.distances.size() < numDests) {
            while(!sd.unknownVertices.isEmpty() && (sd.distances.size() < numDests || !to_get.isEmpty())) {
                Map.Entry<ITermNode<IEdge>, Number> p = sd.getNextVertex();
                ITermNode<IEdge> v = p.getKey();
                double v_dist = ((Number)p.getValue()).doubleValue();
                to_get.remove(v);
                if (v_dist > this.max_distance) {
                    sd.restoreVertex(v, v_dist);
                    sd.reached_max = true;
                    break;
                }

                sd.dist_reached = v_dist;
                if (sd.distances.size() >= this.max_targets) {
                    sd.reached_max = true;
                    break;
                }

                Iterator<IEdge> var10 = this.getEdgesToCheck(v).iterator();

                while(var10.hasNext()) {
                    IEdge e = var10.next();
                    Iterator<ITermNode<IEdge>> var12 = this.g.getIncidentVertices(e).iterator();

                    while(var12.hasNext()) {
                        ITermNode<IEdge> w = var12.next();
                        if (!sd.distances.containsKey(w)) {
                            double edge_weight = ((Number)this.nev.apply(e)).doubleValue();
                            if (edge_weight < 0.0D) {
                                throw new IllegalArgumentException("Edges weights must be non-negative");
                            }

                            double new_dist = v_dist + edge_weight;
                            if (!sd.estimatedDistances.containsKey(w)) {
                                sd.createRecord(w, e, new_dist);
                            } else {
                                double w_dist = (Double)sd.estimatedDistances.get(w);
                                if (new_dist < w_dist) {
                                    sd.update(w, e, new_dist);
                                }
                            }
                        }
                    }
                }
            }

            return sd.distances;
        } else {
            return sd.distances;
        }
    }

    protected UndirectedDijkstraDistance.SourceData getSourceData(ITermNode<IEdge> source) {
        UndirectedDijkstraDistance.SourceData sd = this.sourceMap.get(source);
        if (sd == null) {
            sd = new UndirectedDijkstraDistance.SourceData(source);
        }

        return sd;
    }

    protected Collection<IEdge> getEdgesToCheck(ITermNode<IEdge> v) {
        ArrayList<IEdge> edges = new ArrayList<>();
        edges.addAll(v.getInEdges());
        edges.addAll(v.getOutEdges());
        return edges;
        //return this.g instanceof Graph ? ((Graph)this.g).getOutEdges(v) : this.g.getIncidentEdges(v);
    }

    public Number getDistance(ITermNode<IEdge> source, ITermNode<IEdge> target) {
        if (!this.g.containsVertex(target)) {
            throw new IllegalArgumentException("Specified target vertex " + target + " is not part of graph " + this.g);
        } else if (!this.g.containsVertex(source)) {
            throw new IllegalArgumentException("Specified source vertex " + source + " is not part of graph " + this.g);
        } else {
            Set<ITermNode<IEdge>> targets = new HashSet();
            targets.add(target);
            Map<ITermNode<IEdge>, Number> distanceMap = this.getDistanceMap(source, targets);
            return (Number)distanceMap.get(target);
        }
    }

    public Map<ITermNode<IEdge>, Number> getDistanceMap(ITermNode<IEdge> source, Collection<ITermNode<IEdge>> targets) {
        if (!this.g.containsVertex(source)) {
            throw new IllegalArgumentException("Specified source vertex " + source + " is not part of graph " + this.g);
        } else if (targets.size() > this.max_targets) {
            throw new IllegalArgumentException("size of target set exceeds maximum number of targets allowed: " + this.max_targets);
        } else {
            Map<ITermNode<IEdge>, Number> distanceMap = this.singleSourceShortestPath(source, targets, Math.min(this.g.getVertexCount(), this.max_targets));
            if (!this.cached) {
                this.reset(source);
            }

            return distanceMap;
        }
    }

    public Map<ITermNode<IEdge>, Number> getDistanceMap(ITermNode<IEdge> source) {
        return this.getDistanceMap(source, Math.min(this.g.getVertexCount(), this.max_targets));
    }

    public LinkedHashMap<ITermNode<IEdge>, Number> getDistanceMap(ITermNode<IEdge> source, int numDests) {
        if (!this.g.getShownAndHiddenVertices().contains(source)) {
            throw new IllegalArgumentException("Specified source vertex " + source + " is not part of graph " + this.g);
        } else if (numDests >= 1 && numDests <= this.g.getVertexCount()) {
            if (numDests > this.max_targets) {
                throw new IllegalArgumentException("numDests must be <= the maximum number of targets allowed: " + this.max_targets);
            } else {
                LinkedHashMap<ITermNode<IEdge>, Number> distanceMap = this.singleSourceShortestPath(source, (Collection)null, numDests);
                if (!this.cached) {
                    this.reset(source);
                }

                return distanceMap;
            }
        } else {
            throw new IllegalArgumentException("numDests must be >= 1 and <= g.numVertices()");
        }
    }

    public void setMaxDistance(double max_dist) {
        this.max_distance = max_dist;

        UndirectedDijkstraDistance.SourceData sd;
        for(Iterator<ITermNode<IEdge>> var3 = this.sourceMap.keySet().iterator(); var3.hasNext(); sd.reached_max = this.max_distance <= sd.dist_reached || sd.distances.size() >= this.max_targets) {
            ITermNode<IEdge> v = var3.next();
            sd = (UndirectedDijkstraDistance.SourceData)this.sourceMap.get(v);
        }

    }

    public void setMaxTargets(int max_targets) {
        this.max_targets = max_targets;

        UndirectedDijkstraDistance.SourceData sd;
        for(Iterator<ITermNode<IEdge>> var2 = this.sourceMap.keySet().iterator(); var2.hasNext(); sd.reached_max = this.max_distance <= sd.dist_reached || sd.distances.size() >= max_targets) {
            ITermNode<IEdge> v = var2.next();
            sd = (UndirectedDijkstraDistance.SourceData)this.sourceMap.get(v);
        }

    }

    public void reset() {
        this.sourceMap = new HashMap();
    }

    public void enableCaching(boolean enable) {
        this.cached = enable;
    }

    public void reset(ITermNode<IEdge> source) {
        this.sourceMap.put(source, null);
    }

    protected class SourceData {
        protected LinkedHashMap<ITermNode<IEdge>, Number> distances = new LinkedHashMap();
        protected Map<ITermNode<IEdge>, Number> estimatedDistances = new HashMap();
        protected MapBinaryHeap<ITermNode<IEdge>> unknownVertices;
        protected boolean reached_max = false;
        protected double dist_reached = 0.0D;

        protected SourceData(ITermNode<IEdge> source) {
            this.unknownVertices = new MapBinaryHeap(new UndirectedDijkstraDistance.VertexComparator(this.estimatedDistances));
            UndirectedDijkstraDistance.this.sourceMap.put(source, this);
            this.estimatedDistances.put(source, 0.0);
            this.unknownVertices.add(source);
            this.reached_max = false;
            this.dist_reached = 0.0D;
        }

        protected Map.Entry<ITermNode<IEdge>, Number> getNextVertex() {
            ITermNode<IEdge> v = this.unknownVertices.remove();
            Double dist = (Double)this.estimatedDistances.remove(v);
            this.distances.put(v, dist);
            return new BasicMapEntry(v, dist);
        }

        protected void update(ITermNode<IEdge> dest, IEdge tentative_edge, double new_dist) {
            this.estimatedDistances.put(dest, new_dist);
            this.unknownVertices.update(dest);
        }

        protected void createRecord(ITermNode<IEdge> w, IEdge e, double new_dist) {
            this.estimatedDistances.put(w, new_dist);
            this.unknownVertices.add(w);
        }

        protected void restoreVertex(ITermNode<IEdge> v, double dist) {
            this.estimatedDistances.put(v, dist);
            this.unknownVertices.add(v);
            this.distances.remove(v);
        }
    }

    protected static class VertexComparator implements Comparator<ITermNode<IEdge>> {
        private Map<ITermNode<IEdge>, Number> distances;

        protected VertexComparator(Map<ITermNode<IEdge>, Number> distances) {
            this.distances = distances;
        }

        public int compare(ITermNode<IEdge> o1, ITermNode<IEdge> o2) {
            return ((Double)this.distances.get(o1)).compareTo((Double)this.distances.get(o2));
        }
    }
}