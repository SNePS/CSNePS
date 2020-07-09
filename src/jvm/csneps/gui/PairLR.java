/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package csneps.gui;

/**
 *
 * @author dan
 */
public class PairLR<L,R> {

  private final L left;
  private final R right;

  public PairLR(L left, R right) {
    this.left = left;
    this.right = right;
  }

  public L getLeft() { return left; }
  public R getRight() { return right; }

  @Override
  public int hashCode() { return left.hashCode() ^ right.hashCode(); }

  @Override
  public boolean equals(Object o) {
    if (o == null) return false;
    if (!(o instanceof PairLR)) return false;
    PairLR pairo = (PairLR) o;
    return this.left.equals(pairo.getLeft()) &&
           this.right.equals(pairo.getRight());
  }

}