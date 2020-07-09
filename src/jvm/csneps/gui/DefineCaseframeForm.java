/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/*
 * CaseFrameForm.java
 *
 * Created on Feb 19, 2010, 6:49:29 PM
 */

package csneps.gui;

import csneps.gui.business.FnInterop;
import csneps.gui.business.SemanticType;
import csneps.gui.business.Slot;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.swing.AbstractListModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListModel;

/**
 *
 * @author dan
 */
public class DefineCaseframeForm extends javax.swing.JFrame{

	private static final long serialVersionUID = 5203255870323043291L;

	ArrayList<Slot> slots;

    DefaultListModel<Slot> selectedModel;
    SortedListModel<Slot> unselectedModel;

    SlotForm slotForm;

    boolean fsymbols = false;

    public enum Mode {New, Edit};

    /** Creates new form CaseFrameForm */
    public DefineCaseframeForm() {
        initComponents();
        selectedModel = new DefaultListModel<Slot>();
        unselectedModel = new SortedListModel<Slot>();
        jList_selected.setModel(selectedModel);
        jList_unselected.setModel(unselectedModel);

        DefaultComboBoxModel<SemanticType> m = new DefaultComboBoxModel<SemanticType>();
        for(SemanticType t : SemanticType.getSemanticTypes()){
            m.addElement(t);
        }
        jComboBox_type.setModel(m);

        //unselectedModel.addElement("Test");
    }

    public void setMode(Mode m){
        if(m == Mode.Edit){
            jButton_create.setText("Modify");
            jButton_create.setEnabled(false);
            jButton_select.setEnabled(false);
            jButton_unselect.setEnabled(false);
            jList_selected.setEnabled(false);
            jList_unselected.setEnabled(false);
            jTextField_name.setEnabled(false);
            jButton_newslot.setEnabled(false);
        }
    }

    public void setUnselectedSlots(Collection<Slot> s){
        for(Slot slot : s){
            unselectedModel.add(slot);
        }
    }

    public void setSelectedSlots(Collection<Slot> s){
        for(Slot slot : s){
            selectedModel.addElement(slot);
        }
    }

    public void setCfName(String s){
        jTextField_name.setName(s);
    }

    //This happens when SNePS sends out a new Slot to us.
    public void addSlot(Collection<Slot> s){
        for(Slot slot : s){
        	System.out.println("Adding slot: " + slot);
            if(!unselectedModel.contains(slot) && !selectedModel.contains(slot)){
                unselectedModel.add(slot);
            }
        }
    }
    
    public void clearSlots(){
    	unselectedModel.clear();
    }

    private void initComponents() {

        jSplitPane1 = new javax.swing.JSplitPane();
        jPanel2 = new javax.swing.JPanel();
        jTextField_name = new javax.swing.JTextField();
        jLabel_semtype = new javax.swing.JLabel();
        jComboBox_type = new javax.swing.JComboBox<SemanticType>();
        jPanel_slots = new javax.swing.JPanel();
        jButton_newslot = new javax.swing.JButton();
        jButton_unselect = new javax.swing.JButton();
        jButton_select = new javax.swing.JButton();
        jSplitPane3 = new javax.swing.JSplitPane();
        jScrollPane_unselected = new javax.swing.JScrollPane();
        jList_unselected = new javax.swing.JList<Slot>();
        jScrollPane_selected = new javax.swing.JScrollPane();
        jList_selected = new javax.swing.JList<Slot>();
        jComboBox_head = new javax.swing.JComboBox<String>();
        jPanel_mainButtons = new javax.swing.JPanel();
        jButton_create = new javax.swing.JButton();
        jButton_cancel = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Define Caseframe");

        jSplitPane1.setDividerLocation(380);
        jSplitPane1.setDividerSize(0);
        jSplitPane1.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        jSplitPane1.setResizeWeight(1.0);

        jLabel_semtype.setText("Semantic Type: ");

        jPanel_slots.setBorder(javax.swing.BorderFactory.createTitledBorder("Slots"));

        jButton_newslot.setFont(new java.awt.Font("DejaVu Sans", 0, 10));
        jButton_newslot.setText("New Slot");
        jButton_newslot.setPreferredSize(new java.awt.Dimension(66, 15));
        jButton_newslot.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton_newslotActionPerformed(evt);
            }
        });

        jButton_unselect.setFont(new java.awt.Font("DejaVu Sans", 0, 10));
        jButton_unselect.setText("<");
        jButton_unselect.setPreferredSize(new java.awt.Dimension(66, 15));
        jButton_unselect.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton_unselectActionPerformed(evt);
            }
        });

        jButton_select.setFont(new java.awt.Font("DejaVu Sans", 0, 10));
        jButton_select.setText(">");
        jButton_select.setPreferredSize(new java.awt.Dimension(66, 15));
        jButton_select.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton_selectActionPerformed(evt);
            }
        });

        jSplitPane3.setDividerLocation(210);
        jSplitPane3.setDividerSize(5);
        jSplitPane3.setResizeWeight(0.5);

        jScrollPane_unselected.setViewportView(jList_unselected);

        jSplitPane3.setLeftComponent(jScrollPane_unselected);
        
        jScrollPane_selected.setViewportView(jList_selected);

        jSplitPane3.setRightComponent(jScrollPane_selected);

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel_slots);
        jPanel_slots.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addComponent(jSplitPane3, javax.swing.GroupLayout.DEFAULT_SIZE, 431, Short.MAX_VALUE)
                        .addContainerGap())
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel3Layout.createSequentialGroup()
                        .addComponent(jButton_newslot, javax.swing.GroupLayout.PREFERRED_SIZE, 80, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(75, 75, 75)
                        .addComponent(jButton_select, javax.swing.GroupLayout.PREFERRED_SIZE, 51, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jButton_unselect, javax.swing.GroupLayout.PREFERRED_SIZE, 51, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(174, 174, 174))))
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel3Layout.createSequentialGroup()
                .addComponent(jSplitPane3, javax.swing.GroupLayout.DEFAULT_SIZE, 219, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButton_newslot, javax.swing.GroupLayout.PREFERRED_SIZE, 20, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButton_select, javax.swing.GroupLayout.PREFERRED_SIZE, 20, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButton_unselect, javax.swing.GroupLayout.PREFERRED_SIZE, 20, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap())
        );

        jComboBox_head.setModel(new javax.swing.DefaultComboBoxModel<String>(new String[] { "Name", "Function Symbols" }));
        jComboBox_head.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jComboBox_headActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addComponent(jComboBox_head, javax.swing.GroupLayout.PREFERRED_SIZE, 163, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextField_name, javax.swing.GroupLayout.DEFAULT_SIZE, 296, Short.MAX_VALUE))
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addComponent(jLabel_semtype)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jComboBox_type, 0, 341, Short.MAX_VALUE))
                    .addComponent(jPanel_slots, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jComboBox_head, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jTextField_name, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel_semtype)
                    .addComponent(jComboBox_type, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel_slots, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(17, Short.MAX_VALUE))
        );

        jSplitPane1.setTopComponent(jPanel2);

        jButton_create.setText("Create");
        jButton_create.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton_createActionPerformed(evt);
            }
        });

        jButton_cancel.setText("Cancel");
        jButton_cancel.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel_mainButtons);
        jPanel_mainButtons.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel1Layout.createSequentialGroup()
                .addContainerGap(350, Short.MAX_VALUE)
                .addComponent(jButton_create)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButton_cancel)
                .addContainerGap())
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButton_cancel)
                    .addComponent(jButton_create))
                .addContainerGap())
        );

        jSplitPane1.setBottomComponent(jPanel_mainButtons);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jSplitPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 420, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jSplitPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 425, Short.MAX_VALUE)
        );

        pack();
    }
    
    
    
    private ArrayList<String> parsefsyms(String fsymstr){
    	ArrayList<String> fsyms = new ArrayList<String>();
    	String buffer = "";
    	boolean inlist = false;
    	int depth = 0;
    	
    	for(Character c : fsymstr.toCharArray()){
    		if(c.equals(' ') && !buffer.equals(("")) && !inlist){
    			fsyms.add(buffer);
    			buffer = "";
    		}
    		else if(c.equals('(')){
    			inlist = true;
    			++depth;
    			buffer += c;
    		}
    		else if(c.equals(')') && inlist){
    			buffer += c;
    			--depth;
    			if(depth == 0) inlist = false;
    		}
    		else buffer += c;
    	}
    	fsyms.add(buffer);
    	
    	return fsyms;
    }
    
    
    private void jButton_createActionPerformed(java.awt.event.ActionEvent evt) {
        this.setVisible(false);
        
        ArrayList<Slot> slots = new ArrayList<Slot>();
        for(int i = 0; i < jList_selected.getModel().getSize(); i++){
            slots.add((Slot)jList_selected.getModel().getElementAt(i));
        }
        if(fsymbols){
        	ArrayList<String> fsyms = parsefsyms(jTextField_name.getText());
        	FnInterop.defineCaseframe(jComboBox_type.getSelectedItem().toString(), slots, fsyms);
        	return;
        }
        FnInterop.defineCaseframe(jComboBox_type.getSelectedItem().toString(), jTextField_name.getText(), slots);
    }

    //Add an item from to the selected list from the unselected.
    private void jButton_selectActionPerformed(java.awt.event.ActionEvent evt) {
        int sel = jList_unselected.getSelectedIndex();
        if(sel < 0) return;
        selectedModel.addElement((Slot)unselectedModel.getElementAt(sel));
        unselectedModel.removeElement(unselectedModel.getElementAt(sel));
    }

    //Add the item back to the unselected list from the selected.
    private void jButton_unselectActionPerformed(java.awt.event.ActionEvent evt) {
        int sel = jList_selected.getSelectedIndex();
        if(sel < 0) return;
        unselectedModel.add(selectedModel.get(sel));
        selectedModel.removeElementAt(sel);
    }

    private void jButton_newslotActionPerformed(java.awt.event.ActionEvent evt) {
        slotForm = new SlotForm();
        slotForm.setVisible(true);
    }

    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {
        this.setVisible(false);
        this.dispose();
    }

    private void jComboBox_headActionPerformed(java.awt.event.ActionEvent evt) {
        if(jComboBox_head.getSelectedIndex() == 0){
            fsymbols = false;
        }
        else if (jComboBox_head.getSelectedIndex() == 1){
            fsymbols = true;
        }
    }

    /**
    * @param args the command line arguments
    */
    public static void main(String args[]) {
        java.awt.EventQueue.invokeLater(new Runnable() {
            public void run() {
                new DefineCaseframeForm().setVisible(true);
            }
        });
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButton_cancel;
    private javax.swing.JButton jButton_create;
    private javax.swing.JButton jButton_newslot;
    private javax.swing.JButton jButton_select;
    private javax.swing.JButton jButton_unselect;
    private javax.swing.JComboBox<String> jComboBox_head;
    private javax.swing.JComboBox<SemanticType> jComboBox_type;
    private javax.swing.JLabel jLabel_semtype;
    private javax.swing.JList<Slot> jList_selected;
    private javax.swing.JList<Slot> jList_unselected;
    private javax.swing.JPanel jPanel_mainButtons;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel_slots;
    private javax.swing.JScrollPane jScrollPane_unselected;
    private javax.swing.JScrollPane jScrollPane_selected;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JSplitPane jSplitPane3;
    private javax.swing.JTextField jTextField_name;
    // End of variables declaration//GEN-END:variables

}

class SortedListModel<E> extends AbstractListModel<E> {
	private static final long serialVersionUID = -6702970294830858491L;
	SortedSet<E> model;

	  public SortedListModel() {
	    model = new TreeSet<E>();
	  }

	  public int getSize() {
	    return model.size();
	  }

	  @SuppressWarnings("unchecked")
	  public E getElementAt(int index) {
	    return (E)model.toArray()[index];
	  }

	  public void add(E element) {
	    if (model.add(element)) {
	      fireContentsChanged(this, 0, getSize());
	  }
	}
	  public void addAll(E elements[]) {
	    Collection<E> c = Arrays.asList(elements);
	    model.addAll(c);
	    fireContentsChanged(this, 0, getSize());
	  }

	  public void clear() {
	    model.clear();
	    fireContentsChanged(this, 0, getSize());
	  }

	  public boolean contains(Object element) {
	    return model.contains(element);
	  }

	  public Object firstElement() {
	    return model.first();
	  }

	  public Iterator<E> iterator() {
	    return model.iterator();
	  }

	  public Object lastElement() {
	    return model.last();
	  }

	  public boolean removeElement(Object element) {
	    boolean removed = model.remove(element);
	    if (removed) {
	      fireContentsChanged(this, 0, getSize());
	    }
	    return removed;
	  }
	}
