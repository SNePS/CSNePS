/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.buffalo.cse.sneps3.gui.graph;

import com.google.common.base.Function;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.util.Context;
import edu.uci.ics.jung.visualization.util.ArrowFactory;
import java.awt.Shape;


public class ArrowShapeTransformer<C, S> implements Function<Context<Graph<ITermNode<IEdge>,IEdge>,IEdge>,Shape> {

    public Shape apply(Context<Graph<ITermNode<IEdge>, IEdge>, IEdge> arg0) {
        IEdge e = arg0.element;
        if(e instanceof CollapsedEdge){
            return ArrowFactory.getWedgeArrow(15, 15);
        }
        else return ArrowFactory.getNotchedArrow(8, 8, 5);
    }
}

/**
 *
 * @author dan
 */
/*public class ArrowTransformerTriangle<Context, Shape> implements Transformer {

    public Shape transform(Shape s) {
        return ArrowFactory.getWedgeArrow(10, 10);
    }

    //Transformer<Context<Graph<V,E>,E>,Shape>

}
*/