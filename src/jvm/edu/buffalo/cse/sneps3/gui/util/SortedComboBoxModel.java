package edu.buffalo.cse.sneps3.gui.util;

import java.util.Collections;
import java.util.Comparator;
import java.util.Vector;

import javax.swing.AbstractListModel;
import javax.swing.MutableComboBoxModel;

public class SortedComboBoxModel extends AbstractListModel implements MutableComboBoxModel{
	private static final long serialVersionUID = -3806471690493405434L;
	
	@SuppressWarnings("rawtypes")
	Comparator comparator;
	@SuppressWarnings("rawtypes")
	Vector items;
	
	Object selected;
	
	@SuppressWarnings("rawtypes")
	public SortedComboBoxModel(Comparator c){
		super();
		comparator = c;
		items = new Vector();
	}
	
	@SuppressWarnings({ "rawtypes" })
	public SortedComboBoxModel(Comparator c, Vector items){
		super();
		comparator = c;
		this.items = sortItems((Vector)items.clone(), c);
	}
	
	@SuppressWarnings({ "unchecked", "rawtypes" })
	protected Vector sortItems(Vector v, Comparator c){
		Collections.sort(v, c);
		return v;
	}

	@Override
	public int getSize() {
		return items.size();
	}

	@Override
	public Object getElementAt(int index) {
		return items.get(index);
	}

	@Override
	public void setSelectedItem(Object anItem) {
		if(selected == null && anItem == null) return;
		if(selected != null && anItem.equals(selected)) return;
		if(anItem != null && !items.contains(anItem)) return;
		
		selected = anItem;
		fireContentsChanged(this, -1, -1);
	}

	@Override
	public Object getSelectedItem() {
		return selected;
	}

	@Override
	public void addElement(Object obj) {
		insertElementAt(obj, 0);
		if (items.size() == 1 && selected == null)
			 setSelectedItem(obj);
	}

	@Override
	public void removeElement(Object obj) {
		int idx = items.indexOf(obj);
		if(idx >= 0) removeElementAt(idx);
	}

	@SuppressWarnings("unchecked")
	@Override
	public void insertElementAt(Object obj, int index) {
		int size = items.size();
		int idx = 0;
		//Stay sorted.
		for (idx = 0; idx < size; idx++)
		{
			Object o = getElementAt( idx );
			if (comparator.compare(o, obj) > 0)
				break;
		}

		items.insertElementAt(obj, idx);
		fireIntervalAdded(this, index, index);
	}

	@Override
	public void removeElementAt(int index) {
		if(index < items.size()){
			int selected = items.indexOf(this.selected);
			if (selected == index) // choose a new selected item
			{
				 if (selected > 0) setSelectedItem(getElementAt(selected - 1));
				 else setSelectedItem(getElementAt(selected + 1));
			}
			items.remove(index);
			fireIntervalRemoved(this, index, index);
		}
	}
}
