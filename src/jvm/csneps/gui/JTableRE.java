//Based on http://www.javaworld.com/javaworld/javatips/jw-javatip102.html, modified by DSchlegel.

package csneps.gui;

import javax.swing.*;
import javax.swing.table.*;
import java.util.Vector;

public class JTableRE extends JTable {

    protected RowEditorModel rm;

    public JTableRE() {
        super();
        rm = null;
    }

    public JTableRE(TableModel tm) {
        super(tm);
        rm = null;
    }

    public JTableRE(TableModel tm, TableColumnModel cm) {
        super(tm, cm);
        rm = null;
    }

    public JTableRE(TableModel tm, TableColumnModel cm,
            ListSelectionModel sm) {
        super(tm, cm, sm);
        rm = null;
    }

    public JTableRE(int rows, int cols) {
        super(rows, cols);
        rm = null;
    }

    public JTableRE(final Vector rowData, final Vector columnNames) {
        super(rowData, columnNames);
        rm = null;
    }

    public JTableRE(final Object[][] rowData, final Object[] colNames) {
        super(rowData, colNames);
        rm = null;
    }

    // new constructor
    public JTableRE(TableModel tm, RowEditorModel rm) {
        super(tm, null, null);
        this.rm = rm;
    }

    public void setRowEditorModel(RowEditorModel rm) {
        this.rm = rm;
    }

    public RowEditorModel getRowEditorModel() {
        return rm;
    }

    public TableCellEditor getCellEditor(int row, int col) {
        TableCellEditor tmpEditor = null;
        if (rm != null) {
            tmpEditor = rm.getEditor(row);
        }
        if (tmpEditor != null) {
            return tmpEditor;
        }
        return super.getCellEditor(row, col);
    }
}
