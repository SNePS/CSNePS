/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package csneps.gui;

import java.util.Vector;

/**
 *
 * @author dan
 */
public class VectorTable<A, B> {
    Vector<A> aVect;
    Vector<Vector<B>> bVects;

    VectorTable(){

    }

    VectorTable(Vector<A> a, Vector<B> b){
        aVect = a;
        bVects.add(b);
    }

    public A getItemFromCol1(int row){
        return aVect.get(row);
    }

    public B getItemFromCol2(int row){
        return bVects.get(0).get(row);
    }

    public B getItemFromColGT1(int col, int row){
        return bVects.get(col-1).get(row);
    }
}
