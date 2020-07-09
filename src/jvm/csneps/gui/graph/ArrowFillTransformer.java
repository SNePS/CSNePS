/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package csneps.gui.graph;

import com.google.common.base.Function;

import java.awt.Color;
import java.awt.Paint;


public class ArrowFillTransformer<C, S> implements Function<IEdge, Paint> {

    public Paint apply(IEdge arg0) {
        return Color.white;
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