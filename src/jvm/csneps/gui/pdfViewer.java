///*
// * To change this template, choose Tools | Templates
// * and open the template in the editor.
// */
//
//package csneps.gui;
//
///**
//* ===========================================
//* Java Pdf Extraction Decoding Access Library
//* ===========================================
//*
//* Project Info:  http://www.jpedal.org
//* (C) Copyright 1997-2010, IDRsolutions and Contributors.
//*
//* 	This file is part of JPedal
//*
//    This library is free software; you can redistribute it and/or
//    modify it under the terms of the GNU Lesser General Public
//    License as published by the Free Software Foundation; either
//    version 2.1 of the License, or (at your option) any later version.
//
//    This library is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//    Lesser General Public License for more details.
//
//    You should have received a copy of the GNU Lesser General Public
//    License along with this library; if not, write to the Free Software
//    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//
//*
//* ---------------
//* Viewer.java
//* ---------------
//*/
//
//import java.awt.BorderLayout;
//import java.awt.GridBagConstraints;
//import java.awt.GridBagLayout;
//import java.awt.event.ActionEvent;
//import java.awt.event.ActionListener;
//import java.awt.event.WindowEvent;
//import java.awt.event.WindowListener;
//
//import javax.swing.ImageIcon;
//import javax.swing.JButton;
//import javax.swing.JComponent;
//import javax.swing.JFrame;
//import javax.swing.JLabel;
//import javax.swing.JPanel;
//import javax.swing.JProgressBar;
//import javax.swing.JScrollPane;
//import javax.swing.JTextField;
//
//import org.jpedal.PdfDecoder;
//import org.jpedal.exception.PdfException;
//
//public class pdfViewer {
//
//	JFrame mainFrame = new JFrame("JavaME Viewer");
//	PdfDecoder pdfPane = new PdfDecoder(true);
//	JScrollPane scollPane = new JScrollPane(pdfPane);
//
//	//JButton open = new JButton("Open a new file");
//
//	JPanel navButtons = new JPanel(new GridBagLayout());
//	GridBagConstraints gbc = new GridBagConstraints();
//
//	JTextField page = new JTextField();
//	JLabel pageCount = new JLabel();
//	JButton next = new JButton(new ImageIcon(getClass().getResource("/org/jpedal/examples/simpleviewer/res/fforward.gif")));
//	JButton previous = new JButton(new ImageIcon(getClass().getResource("/org/jpedal/examples/simpleviewer/res/fback.gif")));
//
//	int currentPage = 1;
//
//	WindowListener wl = new WindowListener() {
//		public void windowOpened(WindowEvent arg0) {}
//		public void windowIconified(WindowEvent arg0) {}
//		public void windowDeiconified(WindowEvent arg0) {}
//		public void windowDeactivated(WindowEvent arg0) {}
//		public void windowClosing(WindowEvent arg0) {}
//		public void windowClosed(WindowEvent arg0) {System.exit(1);}
//		public void windowActivated(WindowEvent arg0) {}
//	};
//
//	public static void main(String[] args) {
//
//		/*MEViewer v = new MEViewer();
//		int result = v.init(args[0]);
//		if(result!=0){
//			if(result==-1)
//				System.err.println("An error occured setting up the ME viewer");
//			if(result==-2)
//				System.err.println("An error occured reading the first page");
//			System.exit(1);
//		}*/
//	}
//
//	public pdfViewer(String filename){
//            init(filename);
//	}
//
//	public int init(String fileName){
//
//		mainFrame.addWindowListener(wl);
//
//		/*open.addActionListener(new ActionListener() {
//			public void actionPerformed(ActionEvent arg0) {
//
//				JTextField opentxt = new JTextField("File Name:");
//				opentxt.setEditable(false);
//
//				final JTextField file = new JTextField(pdfPane.getFileName());
//
//				JButton openIt = new JButton("Open File");
//				openIt.addActionListener(new ActionListener() {
//					public void actionPerformed(ActionEvent arg0) {
//						try {
//							setPdfDisplay();
//							openFile(file.getText());
//
//						} catch (PdfException ex) {
//							ex.printStackTrace();
//						}
//					}
//				});
//
//				setDisplay(opentxt, file, openIt);
//			}
//		});*/
//
//		setupNavButtons();
//		setPdfDisplay();
//
//		mainFrame.setVisible(true);
//
//		try {
//
//
////			//open shorted version of pdfreference.
////			String filename="C:\\IDRsolutions\\PDFdata\\reference books\\PDF Reference, version 1-7_1-4.pdf";
////			filename = "C:\\PDFdata\\baseline_screens\\cropping\\Costena.pdf";
//
//			openFile(fileName);
//
//		} catch (PdfException e) {
//			e.printStackTrace();
//			return -1;
//		}
//
//		return 0;
//	}
//
//	private void setupNavButtons() {
//		next.addActionListener(new ActionListener() {
//			public void actionPerformed(ActionEvent arg0) {
//				if(currentPage<pdfPane.getPageCount()){
//					currentPage++;
//					drawPage();
//				}
//			}
//		});
//		previous.addActionListener(new ActionListener() {
//			public void actionPerformed(ActionEvent arg0) {
//				if(currentPage>1){
//					currentPage--;
//					drawPage();
//				}
//			}
//		});
//
//		gbc.fill=GridBagConstraints.BOTH;
//		gbc.gridwidth=1;
//		gbc.gridx=0;
//		gbc.gridy=0;
//		navButtons.add(previous,gbc);
//
//		gbc.gridx=1;
//		navButtons.add(page,gbc);
//
//		gbc.gridx=2;
//		navButtons.add(pageCount,gbc);
//
//		gbc.gridx=3;
//		navButtons.add(next,gbc);
//	}
//
//	private void setPdfDisplay(){
//		mainFrame.getContentPane().removeAll();
//		mainFrame.getContentPane().setLayout(new BorderLayout());
//		mainFrame.getContentPane().add(scollPane,BorderLayout.CENTER);
//		mainFrame.getContentPane().add(navButtons,BorderLayout.SOUTH);
//		//mainFrame.getContentPane().add(open,BorderLayout.NORTH);
//
//		mainFrame.pack();
//		mainFrame.repaint();
//	}
//
//	private void setDisplay(JComponent leftComp, JComponent centerComp,JComponent rightComp) {
//		mainFrame.getContentPane().removeAll();
//		mainFrame.getContentPane().add(leftComp,BorderLayout.WEST);
//		mainFrame.getContentPane().add(centerComp,BorderLayout.CENTER);
//		mainFrame.getContentPane().add(rightComp,BorderLayout.EAST);
//		mainFrame.getContentPane().add(navButtons,BorderLayout.SOUTH);
//
//		mainFrame.pack();
//		mainFrame.repaint();
//	}
//
//	public void openFile(String fileName) throws PdfException{
//		JProgressBar bar = new JProgressBar();
//        bar.setAlignmentX(JComponent.LEFT_ALIGNMENT);
//		bar.setIndeterminate(true);
//        gbc.gridx=4;
//        gbc.gridwidth=2;
//        navButtons.add(bar,gbc);
//
//        mainFrame.pack();
//        mainFrame.repaint();
//
//        //the opening needs to be done on a new thread to allow the user to see the scrolling progress bar
//		pdfPane.openPdfFile(fileName);
//		while(!pdfPane.isOpen()){
//			try {
//				Thread.sleep(100);
//			} catch (InterruptedException e) {
//				// TODO Auto-generated catch block
//				e.printStackTrace();
//			}
//		}
//
//		drawPage();
//		//up to here.
//
//		pageCount.setText(" / "+pdfPane.getPageCount());
//		navButtons.remove(bar);
//		mainFrame.pack();
//        mainFrame.repaint();
//	}
//
//	public void drawPage(){
//		try {
//			page.setText(""+currentPage);
//			pdfPane.decodePage(currentPage);
//
//			pdfPane.setPageParameters(1,1); //values scaling (1=100%). page number
//
//
//			//wait to ensure decoded
//			pdfPane.waitForDecodingToFinish();
//
//			scollPane.invalidate();
//			scollPane.validate();
//			scollPane.repaint();
//
//		} catch (Exception e) {
//			e.printStackTrace();
//		}
//	}
//}