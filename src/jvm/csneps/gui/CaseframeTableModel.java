/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package csneps.gui;

import javax.swing.table.DefaultTableModel;

/**
 *
 * @author dan
 */
public class CaseframeTableModel extends DefaultTableModel {

    public CaseframeTableModel(Object[] columnNames, int rowCount) {
        super(columnNames, rowCount);
    }

    @Override
    public boolean isCellEditable(int row, int col) {
        if (col == 0) {
            return false;
        }
        return true;
    }
}

/*
 * jTable1.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null, null, null, null},
                {null, null, null, null, null}
            },
            new String [] {
                "Slot", "Filler", "Title 3", "Title 4", "Title 5"
            }
        ) {
            boolean[] canEdit = new boolean [] {
                false, true, true, true, true
            };

            public boolean isCellEditable(int rowIndex, int columnIndex) {
                return canEdit [columnIndex];
            }
        });
 */