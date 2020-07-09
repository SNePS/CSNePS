/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package csneps.gui;

import java.awt.event.ActionEvent;
import javax.swing.JDialog;
import java.awt.Frame;
import java.awt.event.ActionListener;

/**
 *
 * @author dan
 */
public class GlobalGraphFilterDialog extends JDialog{

    public GlobalGraphFilterDialog(Frame frame, final GlobalGraphFilter panel){
        super(frame, true);
        setTitle("Set Global Graph Filter");
        setSize(265, 400);

        panel.getOKButton().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                dispose();
                panel.perfomOK();
            }
        });

        panel.getCancelButton().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                dispose();
            }
        });

        add(panel);
    }

}
