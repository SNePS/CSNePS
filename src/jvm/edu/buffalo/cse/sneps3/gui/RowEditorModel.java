//Based on http://www.javaworld.com/javaworld/javatips/jw-javatip102.html, modified by DSchlegel.

package edu.buffalo.cse.sneps3.gui;

import javax.swing.table.*;
import java.util.*;

public class RowEditorModel {

    private HashMap data;

    public RowEditorModel() {
        data = new HashMap();
    }

    public void addEditorForRow(int row, TableCellEditor e) {
        data.put(row, e);
    }

    public void removeEditorForRow(int row) {
        data.remove(row);
    }

    public TableCellEditor getEditor(int row) {
        return (TableCellEditor) data.get(row);
    }
}
