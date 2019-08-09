
/*
 * GUI2.java
 *
 * Created on Jan 15, 2010, 9:25:04 PM
 */

package edu.buffalo.cse.sneps3.gui;

import edu.buffalo.cse.sneps3.gui.business.FnInterop;

import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Collection;

//Clojure
import clojure.lang.PersistentHashSet;
import clojure.lang.RT;
import clojure.tools.nrepl.*;

//Jung 2.0
import edu.buffalo.cse.sneps3.gui.dataaccess.Model;
import edu.buffalo.cse.sneps3.gui.graph.IEdge;
import edu.buffalo.cse.sneps3.gui.graph.ITermNode;
import edu.buffalo.cse.sneps3.gui.graph.SnepsGraph;
import edu.buffalo.cse.sneps3.gui.util.OSTools;
import edu.uci.ics.jung.visualization.Layer;
import edu.uci.ics.jung.algorithms.layout.util.Relaxer;

//Swing
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.KeyEventDispatcher;
import java.awt.KeyboardFocusManager;
import java.awt.RenderingHints;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import javax.swing.JFileChooser;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JToggleButton;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;

import org.freehep.graphicsbase.util.export.ExportDialog;

/**
 *
 * @author Daniel R. Schlegel
 */
public class GUI2 extends javax.swing.JFrame{
	private static final long serialVersionUID = 1L;
	public static final double javaVersion = Double.parseDouble(System.getProperty("java.specification.version"));

	public static final String version = "2018.01.03";
	
    public static final boolean DEBUG = false;

    //The public data model used throughout the app.
    public static Model model;

    private static GUI2 instance;

    Connection cljconn;

    CreateContextForm ccf;
    DefineCaseframeForm caseFrameForm;

    int lastReplSize = 0;

    //This is a way to stop weird collisions early in startup.
    boolean established = false;

    //These strings store the KB so we can save if if we want to.
    //String fullkb = "";
    //defs are slot/caseframe definitions.
    //String defs = "";
    //asserts are assertions since the last clearkb.
    //String asserts = "";

    //This variable contains what is to be sent using the makeLispCall function. 
    //If parens dont match up we store the call here until they do, this way we
    //don't send incomplete commands to Lisp and expect a response.
    String sendQueue = "";

    ArrayList<String> sendHistory = new ArrayList<String>();
    
    
    File currentDir = new File(".");

    ExportDialog export;
    boolean doingSave = false;
    
    private DemoMode demoMode;

    //static ArrayList<Caseframe> hide_cf_list = new ArrayList<Caseframe>();

	/** Creates new form GUI2 */
	public GUI2() {
		// Initialize the model.
		model = new Model();

		caseFrameForm = new DefineCaseframeForm();
		if (javaVersion < 1.9)
			export = new ExportDialog();

		initComponents();

		this.setTitle("CSNePS GUI Version " + version);

        //Use this to redirect output from the repl eventually...
        //Var.pushThreadBindings(RT.map(RT.OUT), <outstreamwriter>)
        
        this.setLayout(new BorderLayout());
        this.add(jToolBar1, BorderLayout.NORTH);
        this.add(jSplitPane1, BorderLayout.CENTER);

        //This will let us start maximized - maybe we want this some day!
        //this.setExtendedState(this.getExtendedState()|JFrame.MAXIMIZED_BOTH);

        //Set the contextModel to be the model for the context combo box
        //comboBox_context.setModel(contextModel);

        //Finally set myself visible
        this.setVisible(true);
        
        setInstance();

        KeyEventDispatcher screenshot = new KeyEventDispatcher() {

            public boolean dispatchKeyEvent(KeyEvent e) {

                //System.out.println(e.getKeyCode() + " " + e.isControlDown() + " " + e.isShiftDown());

                if(e.isControlDown() && e.isShiftDown() && e.getKeyCode() == 83){// && !OSTools.isMac()){
                    if(doingSave) return false;
                    doingSave = true;

                    Component parent = e.getComponent();
                    while(parent.getParent()!=null){
                        parent = parent.getParent();
                    }

                    //ExportDialog export = new ExportDialog();
                    if (javaVersion < 1.9)
                    		export.showExportDialog(parent, "Take Screenshot...", parent, "guiscr");
                    doingSave = false;
                    return true;
                }
                return false;
            }
        };

        KeyboardFocusManager.getCurrentKeyboardFocusManager().addKeyEventDispatcher(screenshot);

        jMenuItem12.setEnabled(false);

        //getGraphPanel().setStatusbarText("New assertions are not shown in the graph.");
    }
    
    public GUI2(int portnum){
    	this();
    	if (DEBUG) System.out.println("Port: " + portnum);
    	try {
    		cljconn = new Connection("nrepl://localhost:" + portnum);
    		replPanel1.appendText("; Connection to Clojure established.\n");
    		replPanel1.connect();
    		initializeModel();
    	}
    	catch(Exception e) {System.out.println("Error connecting to nrepl" + e.getMessage());}
    }
    
    public GUI2(int portnum, PersistentHashSet termset){
    	this();
    	if (DEBUG) System.out.println("Port: " + portnum + " Terms: " + termset);
    	try {
    		cljconn = new Connection("nrepl://localhost:" + portnum);
    		replPanel1.appendText("; Connection to Clojure established.\n");
    		replPanel1.connect();
    		initializeModel(termset);
    	}
    	catch(Exception e) {System.out.println("Error connecting to nrepl" + e.getMessage()); e.printStackTrace();}
    }

    public static Model getModel(){
    	return model;
    }
    

    private void setInstance(){
        instance = this;
    }

    public static GUI2 getInstance(){
        return instance;
    }

    public void setREPLVisibility(boolean vis){
        if(vis){
            splitPane_graphAndREPL.setDividerLocation(lastReplSize);
            splitPane_graphAndREPL.setDividerSize(6);
        }
        else{
            lastReplSize = splitPane_graphAndREPL.getDividerLocation();
            splitPane_graphAndREPL.setDividerLocation(this.getHeight());
            splitPane_graphAndREPL.setDividerSize(0);
        }
    }
    
    public void setPluginsVisibility(boolean vis){
        if(vis){
            jSplitPane1.setDividerLocation(this.getWidth() - 230);
        }
        else{
            //lastPluginsSize = pluginPanel1.getWidth();
            jSplitPane1.setDividerLocation(this.getWidth());
        }
    }

    /****************************************************
     * The following is code for interacting with Clojure
     ****************************************************/
    
	/*public Response sendToClojure(String s){
		return cljconn.send("op", "eval", "code", s);
	}*/
    
    public void clojureEval(String s){
    	replPanel1.makeClojureCall(s);
    }
	
	public void initializeModel(){
		model.setTypesRef(RT.var("csneps.core", "semantic-type-hierarchy"));
		model.initializeTypes();
		model.setSlotsRef(RT.var("csneps.core.relations", "SLOTS")); //Ref to Map of name -> Slot
		model.initializeSlots();
		model.setCaseframesRef(RT.var("csneps.core.caseframes", "CASEFRAMES")); //Ref to Hash set of Caseframe
		model.setFSymbolsRef(RT.var("csneps.core.caseframes", "FN2CF"));
		model.initializeCaseframes();
		model.setTermsRef(RT.var("csneps.core", "TERMS")); //Ref to Map of name -> Term.
		model.setCaseframeRef(RT.var("csneps.core", "term-caseframe-map"));
		model.setDownCablesetRef(RT.var("csneps.core", "down-cableset"));
		model.setRestrictionSetWRef(RT.var("csneps.core", "restriction-set"));
		model.setDependenciesRef(RT.var("csneps.core", "dependencies"));
		
		model.setContextsRef(RT.var("csneps.core.contexts", "CONTEXTS")); //Ref to Map of name -> Context
		model.setCurrentContextRef(RT.var("csneps.core.contexts", "*CurrentContext*"));
		model.initializeContexts();
		model.initializeTerms();
	}
	
	public void initializeModel(PersistentHashSet termset){
		model.setTypesRef(RT.var("csneps.core", "semantic-type-hierarchy"));
		model.initializeTypes();
		model.setSlotsRef(RT.var("csneps.core.relations", "SLOTS")); //Ref to Map of name -> Slot
		model.initializeSlots();
		model.setCaseframesRef(RT.var("csneps.core.caseframes", "CASEFRAMES")); //Ref to Hash set of Caseframe
		model.setFSymbolsRef(RT.var("csneps.core.caseframes", "FN2CF"));
		model.initializeCaseframes();
		model.setTermsRef(RT.var("csneps.core.sneps3", "TERMS")); //Ref to Map of name -> Term.
		model.setCaseframeRef(RT.var("csneps.core", "term-caseframe-map"));
		model.setDownCablesetRef(RT.var("csneps.core", "down-cableset"));
		model.setRestrictionSetWRef(RT.var("csneps.core", "restriction-set"));
		model.setDependenciesRef(RT.var("csneps.core", "dependencies"));
		
		model.setContextsRef(RT.var("csneps.core.contexts", "CONTEXTS")); //Ref to Map of name -> Context
		model.setCurrentContextRef(RT.var("csneps.core.contexts", "*CurrentContext*"));
		model.initializeContexts();
		model.initializeTerms(termset);
	}

    public void startup(){
        //Set the proper package for use:
        //rEPLPanel1.makeLispCall("(in-package :snuser)");
    }

    public boolean checkParenMatch(String s){
        char[] chars = s.toCharArray();
        int left = 0;
        int right = 0;
        boolean pipeignore = false;
        boolean stringignore = false;
        for(int i = 0; i < chars.length; i++){
            if(chars[i] == '|' && (i == 0 || chars[i-1] != '\\') && !stringignore)  pipeignore = !pipeignore;
            if(chars[i] == '\"' && (i == 0 || chars[i-1] != '\\') && !pipeignore) stringignore = !stringignore;
            if(!pipeignore && !stringignore){
                if(chars[i] == '(') left++;
                else if(chars[i] == ')') right++;
            }
        }
        if(left == right) return true;
        return false;
    }
    
/*    public void makeLispCall(String s){
        if(s.trim().equals("") || s.trim().charAt(0) == ';') return;
        sendQueue += s;
        if(checkParenMatch(sendQueue)){
            sendHistory.add(sendQueue.toString());
            rEPLPanel1.makeLispCall(sendQueue);
            fullkb += sendQueue + '\n';
            if(sendQueue.contains("defineCaseframe") || sendQueue.contains("defineSlot") || sendQueue.contains("defineType")){
                defs += sendQueue + '\n';
            }
            else{
                asserts += sendQueue + '\n';
            }
            sendQueue = "";
        }
    }*/
    
    //This is all just used by the jlinker thing because it isnt very good.
    //******JLinker recv methods.

/*    public void reinitialize(boolean complete){
        jungGraphPanel1.reinitialize();
        nodeName_node_map.clear();
        if(GUI2.DEBUG) System.out.println("NNNM: " + GUI2.getInstance().getNodeName_node_map());
        edges.clear();
        hide_cf_list.clear();
        model.clearContexts();
        if(complete){
            model.clearCaseframes();
            model.clearSlots();
        }

        //jungGraphPanel1.reinitialize();
        //nodeName_node_map.clear();
        //System.out.println("NNNM: " + GUI2.getInstance().getNodeName_node_map());
        //edges.clear();
        jTabbedPane1.setTitleAt(0, "Graph View");
        //For output
        asserts = "";
    }*/

    //public static ArrayList<Caseframe> getHideCFList(){
    //    return hide_cf_list;
    //}

    /*public void updateContexts(ArrayList arr){
        //Old - we shouldn't use this anymore!
        //model.recvUpdatedContexts(arr);
    }

    //Move to a type safe Vector as soon as possible.
    public void updateSemanticTypes(ArrayList arr){
        ArrayList<SemanticType> temp = new ArrayList<SemanticType>();
        for(int i = 0; i < arr.size(); i++){
            if(arr.get(i) instanceof SemanticType){
                temp.add((SemanticType)arr.get(i));
            }
        }
        model.recvUpdatedTypes(temp);
    }

    public void addCaseframe(Caseframe c){
        model.addNewCaseframe(c);
    }

    public void addSlot(Slot s){
        model.addNewSlot(s);
    }*/

//    public void addSemanticType(String name, ArrayList parents){
//        if(DEBUG) System.out.println("DEBUG: Received Type " + name + " parents " + parents);
//        ArrayList<SemanticType> stParents = new ArrayList<SemanticType>();
//        for(SemanticType s : model.types){
//            for(int i = 0; i < parents.size(); i++){
//                if(((String)parents.get(i)).equals(s.name)) stParents.add(s);
//            }
//        }
//        model.addNewType(new SemanticType(name, stParents));
//        if(DEBUG) System.out.println("Added Type: " + name + " parents " + stParents);
//    }

/*    public void addContext(String name, ArrayList parents){
        if(DEBUG) System.out.println("DEBUG: Received Context " + name + " parents " + parents);
        ArrayList<Context> ctParents = new ArrayList<Context>();
        for(Context c : model.contexts){
            for(int i = 0; i < parents.size(); i++){
                if(((String)parents.get(i)).equals(c.name)) ctParents.add(c);
            }
        }
        model.addNewContext(new Context(name, ctParents));
    }*/

    //public void setCurrentContext(String name){
    //	model.selectCurrentContext(Context.getContext(name));
    //}
    //******End JLinker recv methods.

    //The following methods are to hack around jlinker not supporting gerics in java.
    //DirectedSparseMultigraph<String, Edge> dsg = new DirectedSparseMultigraph<String, Edge>();
    //DirectedSparseMultigraph<TermNode, Edge> dsg = new DirectedSparseMultigraph<TermNode, Edge>();
    //public void createNewGraph(){
//        dsg = new DirectedSparseMultigraph<String, Edge>();
    //    dsg = new DirectedSparseMultigraph<TermNode, Edge>();
    //}

    //If we already have a graph, we may want to convert to a StaticLayout!

/*    public void addStringVertex(String s){
        //Make sure we haven't already added this vertex. We need to do this for
        //graph updates- and I'm not sure if its better to do here or in the lisp
        //end of things.
        for(String str : dsg.getVertices()){
            if(str.equals(s)) return;
        }
        dsg.addVertex(s);
    }*/

    public void setAddMode(boolean b){
        if(b){
            jungGraphPanel1.layout.lock(true);
            Relaxer relaxer = jungGraphPanel1.vv.getModel().getRelaxer();
            relaxer.pause();
        }
        else{
            jungGraphPanel1.layout.initialize();
            Relaxer relaxer = jungGraphPanel1.vv.getModel().getRelaxer();
            relaxer.resume();
            jungGraphPanel1.layout.lock(false);
        }
    }

/*    public void relayoutGraph(){
        jungGraphPanel1.displayGraph(jungGraphPanel1.getGraph());
    }

    public void disableGraphRefresh(){
        jCheckBoxMenuItem_autoRefresh.setSelected(false);
        makeLispCall("(setf sneps3::*auto-refresh-graph* nil)");
    }

    public void enableGraphRefresh(){
        jCheckBoxMenuItem_autoRefresh.setSelected(true);
        makeLispCall("(setf sneps3::*auto-refresh-graph* t)");
        makeLispCall("(sneps3::generate-graph)");
    }

    public void disableGraphRelayout(){
        jCheckBoxMenuItem_autoRelayout.setSelected(false);
        makeLispCall("(setf sneps3::*auto-relayout-graph* nil)");

    }

    public void enableGraphRelayout(){
        jCheckBoxMenuItem_autoRelayout.setSelected(true);
        makeLispCall("(setf sneps3::*auto-relayout-graph* t)");
    }*/
    //End hacky crap

    public Collection<ITermNode<IEdge>> getNodes(){
        return jungGraphPanel1.dsg.getVertices();
    }

    public SnepsGraph<ITermNode<IEdge>, IEdge> getGraph(){
        return jungGraphPanel1.dsg;
    }

    public void enableFindButton(){
        //jungGraphPanel1.exitFindMode.setVisible(true);
    }

/*    public void findGraphResults(ArrayList l){
        if(DEBUG) System.out.println("List received. Size: " + l.size());
        if(jungGraphPanel1.isShowingAll()){
            jungGraphPanel1.displayOnlyNodeSet(l);
        }
        else jungGraphPanel1.displayNodeSet(l);
        //jungGraphPanel1.displayFindGraph(l);
    }*/

//    public Map<String, TermNode> getNodeName_node_map(){
//        return nodeName_node_map;
//    }

    //public ArrayList<Edge> getEdge_list(){
    //    return edges;
    //}


    public void loadToKB(File f){
    	try {
    		setAddMode(true);
			clojure.lang.Compiler.loadFile(f.getAbsolutePath());
			setAddMode(false);
		} catch (Exception e) {
			e.printStackTrace();
		}
    }

    public void setScaleLevel(int i){
        jungGraphPanel1.getVV().getRenderContext().getMultiLayerTransformer().getTransformer(Layer.LAYOUT).setToIdentity();
        jungGraphPanel1.getVV().getRenderContext().getMultiLayerTransformer().getTransformer(Layer.VIEW).setToIdentity();
        jungGraphPanel1.scale_btn_click = i;
        if(i>=0)
            for(int j = 0; j < i; j++){
                jungGraphPanel1.scaler.scale(jungGraphPanel1.getVV(), jungGraphPanel1.scaleAmt, jungGraphPanel1.getVV().getCenter());
            }
        else
            for(int j = 0; j > i; j++){
                jungGraphPanel1.scaler.scale(jungGraphPanel1.getVV(), 1/jungGraphPanel1.scaleAmt, jungGraphPanel1.getVV().getCenter());
            }
    }


    public final JungGraphPanel getGraphPanel(){
        return jungGraphPanel1;
    }

    private void initComponents() {
        jToolBar1 = new javax.swing.JToolBar();
        jButton1 = new javax.swing.JButton();
        jToggleButton_repl = new javax.swing.JToggleButton();
        jToggleButton_plugins = new javax.swing.JToggleButton();
        jSplitPane1 = new javax.swing.JSplitPane();
        splitPane_graphAndREPL = new javax.swing.JSplitPane();
        jTabbedPane1 = new javax.swing.JTabbedPane();
        jungGraphPanel1 = new edu.buffalo.cse.sneps3.gui.JungGraphPanel();
        replPanel1 = new edu.buffalo.cse.sneps3.gui.REPLPanel();
        pluginPanel1 = new edu.buffalo.cse.sneps3.gui.PluginPanel();
        jMenuBar1 = new javax.swing.JMenuBar();
        jMenu1 = new javax.swing.JMenu();
        jMenu5 = new javax.swing.JMenu();
        jMenuItem1 = new javax.swing.JMenuItem();
        loadDemo = new javax.swing.JMenuItem();
        jMenu4 = new javax.swing.JMenu();
        saveCurrentKB = new javax.swing.JMenuItem();
        //saveKBasDemo = new javax.swing.JMenuItem();
        jMenuItem10 = new javax.swing.JMenuItem();
        jMenuItem3 = new javax.swing.JMenuItem();
        jMenu_globablFilter = new javax.swing.JMenu();
        jMenuItem9 = new javax.swing.JMenuItem();
        jMenuItem12 = new javax.swing.JMenuItem();
        jMenuItem_refreshGraph = new javax.swing.JMenuItem();
        //jCheckBoxMenuItem_autoRefresh = new javax.swing.JCheckBoxMenuItem();
        jMenuItem_relayout = new javax.swing.JMenuItem();
        jCheckBoxMenuItem_autoRelayout = new javax.swing.JCheckBoxMenuItem();
        jMenu7 = new javax.swing.JMenu();
        jMenuItem4 = new javax.swing.JMenuItem();
        jMenuItem5 = new javax.swing.JMenuItem();
        jMenuItem6 = new javax.swing.JMenuItem();
        jMenuItem7 = new javax.swing.JMenuItem();
        jMenuItem8 = new javax.swing.JMenuItem();
        jCheckBoxMenuItem_antialias = new javax.swing.JCheckBoxMenuItem();
        jMenu3 = new javax.swing.JMenu();
        jMenuItem2 = new javax.swing.JMenuItem();
        jMenuItem11 = new javax.swing.JMenuItem();
        jMenu6 = new javax.swing.JMenu();
        menuItem_sneps3manual = new javax.swing.JMenuItem();
        menuItem_guidocs = new javax.swing.JMenuItem();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        
        jToolBar1.setRollover(true);

        jButton1.setText("Add Frame Instance");
        jButton1.setFocusable(false);
        jButton1.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButton1.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });
        jToolBar1.add(jButton1);

        jToggleButton_repl.setSelected(true);
        jToggleButton_repl.setText("REPL");
        jToggleButton_repl.setFocusable(false);
        jToggleButton_repl.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jToggleButton_repl.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToggleButton_repl.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jToggleButton_replActionPerformed(evt);
            }
        });
        jToolBar1.add(jToggleButton_repl);
        
        jToggleButton_plugins.setSelected(true);
        jToggleButton_plugins.setText("Plugins");
        jToggleButton_plugins.setFocusable(false);
        jToggleButton_plugins.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jToggleButton_plugins.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToggleButton_plugins.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jToggleButton_pluginsActionPerformed(evt);
            }
        });
        jToolBar1.add(jToggleButton_plugins);

        jSplitPane1.setDividerLocation(850);
        jSplitPane1.setDividerSize(0);
        jSplitPane1.setResizeWeight(1.0);

        splitPane_graphAndREPL.setDividerLocation(451);
        splitPane_graphAndREPL.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        splitPane_graphAndREPL.setResizeWeight(1.0);

        jTabbedPane1.addTab("Graph View", jungGraphPanel1);

        splitPane_graphAndREPL.setTopComponent(jTabbedPane1);
        jTabbedPane1.getAccessibleContext().setAccessibleName("Graph View");

        splitPane_graphAndREPL.setRightComponent(replPanel1);

        jSplitPane1.setLeftComponent(splitPane_graphAndREPL);
        jSplitPane1.setRightComponent(pluginPanel1);

        jMenu1.setMnemonic('F');
        jMenu1.setText("File");

        jMenu5.setMnemonic('L');
        jMenu5.setText("Load");

        jMenuItem1.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_L, InputEvent.CTRL_DOWN_MASK));
        jMenuItem1.setMnemonic('L');
        jMenuItem1.setText("Load to KB");
        jMenuItem1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem1ActionPerformed(evt);
            }
        });
        jMenu5.add(jMenuItem1);

        loadDemo.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_D, InputEvent.ALT_DOWN_MASK));
        loadDemo.setMnemonic('D');
        loadDemo.setText("Demo");
        loadDemo.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                loadDemoActionPerformed(evt);
            }
        });
        jMenu5.add(loadDemo);

        jMenu1.add(jMenu5);

        jMenu4.setMnemonic('S');
        jMenu4.setText("Save");

        saveCurrentKB.setText("Current KB");
        saveCurrentKB.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                saveCurrentKBActionPerformed(evt);
            }
        });
        jMenu4.add(saveCurrentKB);

        /*saveKBasDemo.setText("KB as Demo");
        saveKBasDemo.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                saveKBasDemoActionPerformed(evt);
            }
        });
        jMenu4.add(saveKBasDemo);*/

        jMenuItem10.setText("Export Graph...");
        jMenuItem10.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem10ActionPerformed(evt);
            }
        });
        if (javaVersion < 1.9) 
        		jMenu4.add(jMenuItem10);

        jMenu1.add(jMenu4);

        jMenuItem3.setMnemonic('Q');
        jMenuItem3.setText("Quit");
        jMenuItem3.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem3ActionPerformed(evt);
            }
        });
        jMenu1.add(jMenuItem3);

        jMenuBar1.add(jMenu1);

        jMenu_globablFilter.setMnemonic('G');
        jMenu_globablFilter.setText("Graph");

        jMenuItem9.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F, InputEvent.CTRL_DOWN_MASK));
        jMenuItem9.setMnemonic('S');
        jMenuItem9.setText("Show In Graph");
        jMenuItem9.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem9ActionPerformed(evt);
            }
        });
        jMenu_globablFilter.add(jMenuItem9);

        jMenuItem12.setText("Globally Filter Graph by Caseframe");
        jMenuItem12.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem12ActionPerformed(evt);
            }
        });
        jMenu_globablFilter.add(jMenuItem12);

        jMenu_globablFilter.add(new JPopupMenu.Separator());

		jCheckBoxMenuItem_showOntologyTerms = new javax.swing.JCheckBoxMenuItem("Show Ontology Terms", false);
		jCheckBoxMenuItem_showOntologyTerms.addActionListener(e -> jungGraphPanel1.setShowOntologyTerms(jCheckBoxMenuItem_showOntologyTerms.isSelected()));
		jMenu_globablFilter.add(jCheckBoxMenuItem_showOntologyTerms);

		jCheckBoxMenuItem_showNewAssertions = new javax.swing.JCheckBoxMenuItem("Show New Assertions in Graph", true);
		jCheckBoxMenuItem_showNewAssertions.addActionListener(e -> {
			if (jCheckBoxMenuItem_showNewAssertions.isSelected()) {
				jungGraphPanel1.setShowNewTerms(true);
				getGraphPanel().setStatusbarText("");
			} else {
				jungGraphPanel1.setShowNewTerms(false);
				getGraphPanel().setStatusbarText("New assertions are not shown in the graph.");
			}
		});
        
        jMenu_globablFilter.add(jCheckBoxMenuItem_showNewAssertions);
        jMenu_globablFilter.add(new JPopupMenu.Separator());

        jMenuItem_refreshGraph.setMnemonic('F');
        jMenuItem_refreshGraph.setText("Refresh");
        jMenuItem_refreshGraph.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem_refreshGraphActionPerformed(evt);
            }
        });
        jMenu_globablFilter.add(jMenuItem_refreshGraph);

        /*jCheckBoxMenuItem_autoRefresh.setMnemonic('R');
        jCheckBoxMenuItem_autoRefresh.setSelected(true);
        jCheckBoxMenuItem_autoRefresh.setText("Auto Refresh");
        jCheckBoxMenuItem_autoRefresh.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jCheckBoxMenuItem_autoRefreshActionPerformed(evt);
            }
        });
        jMenu_globablFilter.add(jCheckBoxMenuItem_autoRefresh);*/

        jMenuItem_relayout.setMnemonic('L');
        jMenuItem_relayout.setText("Relayout");
        jMenuItem_relayout.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem_relayoutActionPerformed(evt);
            }
        });
        jMenu_globablFilter.add(jMenuItem_relayout);

        jCheckBoxMenuItem_autoRelayout.setMnemonic('E');
        jCheckBoxMenuItem_autoRelayout.setSelected(true);
        jCheckBoxMenuItem_autoRelayout.setText("Auto Relayout");
        jCheckBoxMenuItem_autoRelayout.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jCheckBoxMenuItem_autoRelayoutActionPerformed(evt);
            }
        });
        jMenu_globablFilter.add(jCheckBoxMenuItem_autoRelayout);

        jMenu7.setMnemonic('F');
        jMenu7.setText("Font Size");

        jMenuItem4.setText("12");
        jMenuItem4.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemFontSizeActionPerformed(evt);
            }
        });
        jMenu7.add(jMenuItem4);

        jMenuItem5.setText("14");
        jMenuItem5.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemFontSizeActionPerformed(evt);
            }
        });
        jMenu7.add(jMenuItem5);

        jMenuItem6.setText("16");
        jMenuItem6.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemFontSizeActionPerformed(evt);
            }
        });
        jMenu7.add(jMenuItem6);

        jMenuItem7.setText("18");
        jMenuItem7.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemFontSizeActionPerformed(evt);
            }
        });
        jMenu7.add(jMenuItem7);

        jMenuItem8.setText("20");
        jMenuItem8.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemFontSizeActionPerformed(evt);
            }
        });
        jMenu7.add(jMenuItem8);

        jMenu_globablFilter.add(jMenu7);

        jCheckBoxMenuItem_antialias.setSelected(true);
        jCheckBoxMenuItem_antialias.setText("Anti-Aliasing");
        jCheckBoxMenuItem_antialias.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jCheckBoxMenuItem_antialiasActionPerformed(evt);
            }
        });
        jMenu_globablFilter.add(jCheckBoxMenuItem_antialias);

        jMenuBar1.add(jMenu_globablFilter);

        jMenu3.setMnemonic('S');
        jMenu3.setText("SNePS");

        jMenuItem2.setMnemonic('C');
        jMenuItem2.setText("Clear Knowledge Base");
        jMenuItem2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem2ActionPerformed(evt);
            }
        });
        jMenu3.add(jMenuItem2);

        jMenuItem11.setMnemonic('L');
        jMenuItem11.setText("Clear Knowledge Base, Slots, and Caseframes");
        jMenuItem11.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem11ActionPerformed(evt);
            }
        });
        jMenu3.add(jMenuItem11);

        jMenuBar1.add(jMenu3);

        jMenu6.setMnemonic('H');
        jMenu6.setText("Help");

        menuItem_sneps3manual.setMnemonic('S');
        menuItem_sneps3manual.setText("SNePS 3 Manual");
        menuItem_sneps3manual.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                menuItem_sneps3manualActionPerformed(evt);
            }
        });
        jMenu6.add(menuItem_sneps3manual);

        menuItem_guidocs.setMnemonic('G');
        menuItem_guidocs.setText("GUI Manual");
        menuItem_guidocs.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                menuItem_guidocsActionPerformed(evt);
            }
        });
        jMenu6.add(menuItem_guidocs);

        jMenuBar1.add(jMenu6);

        setJMenuBar(jMenuBar1);
        
        this.setLayout(new BorderLayout());
        this.setSize(1100, 700);
        this.add(jToolBar1, BorderLayout.NORTH);
        this.add(jSplitPane1, BorderLayout.CENTER);
    }// </editor-fold>//GEN-END:initComponents


    private void jToggleButton_replActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jToggleButton_replActionPerformed
        if(((JToggleButton) evt.getSource()).isSelected()) setREPLVisibility(true);
        else setREPLVisibility(false);
    }//GEN-LAST:event_jToggleButton_replActionPerformed
    
    private void jToggleButton_pluginsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jToggleButton_replActionPerformed
        if(((JToggleButton) evt.getSource()).isSelected()) setPluginsVisibility(true);
        else setPluginsVisibility(false);
    }

    private void jMenuItem1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem1ActionPerformed
        JFileChooser chooser = new JFileChooser();
        chooser.setCurrentDirectory(currentDir);
        int returnVal = chooser.showOpenDialog(this);
        if(returnVal == JFileChooser.APPROVE_OPTION) {
            currentDir = chooser.getCurrentDirectory();
            loadToKB(chooser.getSelectedFile());
        } 
    }//GEN-LAST:event_jMenuItem1ActionPerformed

    private void jMenuItem3ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem3ActionPerformed
        System.exit(0);
    }//GEN-LAST:event_jMenuItem3ActionPerformed

    private void saveCurrentKBActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_saveCurrentKBActionPerformed
        JFileChooser chooser = new JFileChooser();
        chooser.setCurrentDirectory(currentDir);
        int returnVal = chooser.showSaveDialog(this);
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            currentDir = chooser.getCurrentDirectory();
            FnInterop.writeKBToTextFile(chooser.getSelectedFile().getAbsolutePath());
        }
    }

    private void jMenuItem2ActionPerformed(java.awt.event.ActionEvent evt) {
        //model.clearContexts();
        //asserts = "";
        FnInterop.clearkb(false);
    }

    /*private void saveKBasDemoActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_saveKBasDemoActionPerformed
        JFileChooser chooser = new JFileChooser();
        chooser.setCurrentDirectory(currentDir);
        int returnVal = chooser.showSaveDialog(this);
        if (returnVal == JFileChooser.APPROVE_OPTION) {
        	currentDir = chooser.getCurrentDirectory();
        	FnInterop.writeKBToTextFile(chooser.getSelectedFile().getAbsolutePath());
        	
        	
            FileWriter outFile = null;
            try {
                File f = chooser.getSelectedFile();
                outFile = new FileWriter(f);
                PrintWriter out = new PrintWriter(outFile);
                out.write(fullkb);
            } catch (IOException ex) {
                Logger.getLogger(GUI2.class.getName()).log(Level.SEVERE, null, ex);
            } finally {
                try {
                    outFile.close();
                } catch (IOException ex) {
                    Logger.getLogger(GUI2.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
        }
    }*///GEN-LAST:event_saveKBasDemoActionPerformed

    private void loadDemoActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_loadDemoActionPerformed
        try{
        	if (demoMode != null && !demoMode.isFinished()){
        		int dialogResult = JOptionPane.showConfirmDialog(null, 
        				"A demo is already running, would you like to load a new demo?", 
        				"Demo Already Running", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);
        		
        		if (dialogResult == JOptionPane.NO_OPTION) return;
        	}

            JFileChooser chooser = new JFileChooser();
            chooser.setCurrentDirectory(currentDir);
            int returnVal = chooser.showOpenDialog(this);
            if(returnVal == JFileChooser.APPROVE_OPTION) {
                currentDir = chooser.getCurrentDirectory();
                File f = chooser.getSelectedFile();
                jTabbedPane1.setTitleAt(0, "Graph View: " + f.getName());
                BufferedReader reader = new BufferedReader( new FileReader (f));
                String line  = null;
                StringBuilder stringBuilder = new StringBuilder();
                String ls = System.getProperty("line.separator");
                while( ( line = reader.readLine() ) != null ) {
                    stringBuilder.append( line );
                    stringBuilder.append( ls );
                }
                reader.close();
                
                if(demoMode != null) {
        			demoMode.setVisible(false);
        			demoMode.dispose();
        		}
                
                demoMode = new DemoMode();
                demoMode.setVisible(true);
                demoMode.setupDemo(stringBuilder.toString(), this);
            }
        }
        catch(Exception e){e.printStackTrace();}
    }//GEN-LAST:event_loadDemoActionPerformed

    private void jMenuItem_refreshGraphActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem_refreshGraphActionPerformed
    	//TODO: Decide what to do about this.
        //makeLispCall("(sneps3::generate-graph)");
    }//GEN-LAST:event_jMenuItem_refreshGraphActionPerformed

/*    private void jCheckBoxMenuItem_autoRefreshActionPerformed(java.awt.event.ActionEvent evt) {
        AbstractButton aButton = (AbstractButton) evt.getSource();
        boolean selected = aButton.getModel().isSelected();
        if(selected){
            makeLispCall("(setf sneps3::*auto-refresh-graph* t)");
            makeLispCall("(sneps3::generate-graph)");
        }
        else{
            makeLispCall("(setf sneps3::*auto-refresh-graph* nil)");
        }
    }*/

    private void jCheckBoxMenuItem_autoRelayoutActionPerformed(java.awt.event.ActionEvent evt) {
        /*AbstractButton aButton = (AbstractButton) evt.getSource();
        boolean selected = aButton.getModel().isSelected();
        if(selected){
            makeLispCall("(setf sneps3::*auto-relayout-graph* t)");
        }
        else{
            makeLispCall("(setf sneps3::*auto-relayout-graph* nil)");
        }*/
    	//TODO: Implement auto relayout as part of building the graph.
    }

    private void jMenuItem_relayoutActionPerformed(java.awt.event.ActionEvent evt) {
    	jungGraphPanel1.displayGraph(jungGraphPanel1.getGraph());
    }

    private void menuItem_sneps3manualActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_menuItem_sneps3manualActionPerformed
        pdfViewer manual = new pdfViewer("Docs/manual.pdf");
    }//GEN-LAST:event_menuItem_sneps3manualActionPerformed

    private void menuItem_guidocsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_menuItem_guidocsActionPerformed
        pdfViewer manual = new pdfViewer("Docs/SNePSGUIDocs.pdf");
    }//GEN-LAST:event_menuItem_guidocsActionPerformed

    private void jMenuItemFontSizeActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItemFontSizeActionPerformed
        JMenuItem m = (JMenuItem)evt.getSource();
        int s = Integer.parseInt(m.getText());
        jungGraphPanel1.setFontSize(s);
    }//GEN-LAST:event_jMenuItemFontSizeActionPerformed

    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
        AddToKBPanel ag = new AddToKBPanel();
        //ag.setParent(this);
        ag.setVisible(true);
}//GEN-LAST:event_jButton1ActionPerformed

    private void jMenuItem9ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem9ActionPerformed
        FindQuery3 f = new FindQuery3();
        f.setVisible(true);
    }//GEN-LAST:event_jMenuItem9ActionPerformed

    private void jMenuItem10ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem10ActionPerformed
        export.showExportDialog(this, "Export graph as...", jungGraphPanel1.getVV(), "propgraph");
        if(jCheckBoxMenuItem_antialias.isSelected()){
            jungGraphPanel1.getVV().getRenderingHints().put(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        }
    }//GEN-LAST:event_jMenuItem10ActionPerformed

    private void jCheckBoxMenuItem_antialiasActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jCheckBoxMenuItem_antialiasActionPerformed
        if(jCheckBoxMenuItem_antialias.isSelected()){
            jungGraphPanel1.getVV().getRenderingHints().put(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            jungGraphPanel1.getVV().repaint();
        }
        else{
            jungGraphPanel1.getVV().getRenderingHints().put(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF);
            jungGraphPanel1.getVV().repaint();
        }
    }//GEN-LAST:event_jCheckBoxMenuItem_antialiasActionPerformed

    private void jMenuItem11ActionPerformed(java.awt.event.ActionEvent evt) {
        FnInterop.clearkb(true);
    }

    private void jMenuItem12ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem12ActionPerformed
        GlobalGraphFilter.showFilterDialog(this);
    }//GEN-LAST:event_jMenuItem12ActionPerformed

    /**
    * @param args the command line arguments
    */
    public static void main(final String args[]) {

        java.awt.EventQueue.invokeLater(new Runnable() {
            public void run() {
            	try {
					UIManager.setLookAndFeel(
							UIManager.getSystemLookAndFeelClassName());
				} catch (ClassNotFoundException | InstantiationException | IllegalAccessException
						| UnsupportedLookAndFeelException e) {
					e.printStackTrace();
				}
            		if (OSTools.isMac()) System.setProperty("apple.laf.useScreenMenuBar", "true");
            	
                instance = new GUI2();
                instance.setVisible(true);
            }
        });
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButton1;
    private javax.swing.JCheckBoxMenuItem jCheckBoxMenuItem_antialias;
    //private javax.swing.JCheckBoxMenuItem jCheckBoxMenuItem_autoRefresh;
    private javax.swing.JCheckBoxMenuItem jCheckBoxMenuItem_autoRelayout;
    private javax.swing.JCheckBoxMenuItem jCheckBoxMenuItem_showOntologyTerms;
    private javax.swing.JCheckBoxMenuItem jCheckBoxMenuItem_showNewAssertions;
    private javax.swing.JMenu jMenu1;
    private javax.swing.JMenu jMenu3;
    private javax.swing.JMenu jMenu4;
    private javax.swing.JMenu jMenu5;
    private javax.swing.JMenu jMenu6;
    private javax.swing.JMenu jMenu7;
    private javax.swing.JMenuBar jMenuBar1;
    private javax.swing.JMenuItem jMenuItem1;
    private javax.swing.JMenuItem jMenuItem10;
    private javax.swing.JMenuItem jMenuItem11;
    private javax.swing.JMenuItem jMenuItem12;
    private javax.swing.JMenuItem jMenuItem2;
    private javax.swing.JMenuItem jMenuItem3;
    private javax.swing.JMenuItem jMenuItem4;
    private javax.swing.JMenuItem jMenuItem5;
    private javax.swing.JMenuItem jMenuItem6;
    private javax.swing.JMenuItem jMenuItem7;
    private javax.swing.JMenuItem jMenuItem8;
    private javax.swing.JMenuItem jMenuItem9;
    private javax.swing.JMenuItem jMenuItem_refreshGraph;
    private javax.swing.JMenuItem jMenuItem_relayout;
    private javax.swing.JMenu jMenu_globablFilter;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JTabbedPane jTabbedPane1;
    private javax.swing.JToggleButton jToggleButton_repl;
    private javax.swing.JToggleButton jToggleButton_plugins;
    private javax.swing.JToolBar jToolBar1;
    private edu.buffalo.cse.sneps3.gui.JungGraphPanel jungGraphPanel1;
    private javax.swing.JMenuItem loadDemo;
    private javax.swing.JMenuItem menuItem_guidocs;
    private javax.swing.JMenuItem menuItem_sneps3manual;
    private edu.buffalo.cse.sneps3.gui.PluginPanel pluginPanel1;
    private edu.buffalo.cse.sneps3.gui.REPLPanel replPanel1;
    private javax.swing.JMenuItem saveCurrentKB;
    //private javax.swing.JMenuItem saveKBasDemo;
    private javax.swing.JSplitPane splitPane_graphAndREPL;
}
