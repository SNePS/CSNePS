/*
 Making use of the API is really like having access to the back end of the GUI.
 */

package csneps.api;

import clojure.java.api.Clojure;
import clojure.lang.*;
import csneps.gui.GUI2;
import csneps.gui.business.FnInterop;
import csneps.gui.business.Term;
import csneps.gui.dataaccess.Controller;
import csneps.gui.dataaccess.Model;

import java.util.HashSet;
import java.util.Set;

public class CSNePS extends FnInterop {

    public CSNePS(){
        load_csneps();
    }

    public void startGUI(){
        Controller.gui_startGUI();
    }

    public static void clearkb(boolean clearall){
        Controller.snuser_clearkb(clearall);
    }

    public static void load(String filename) { Controller.snuser_load(filename); }

    public static Set<Term> pathsfrom(Term term, String path){

        Set<Term> results = new HashSet<>();

        IPersistentSet res = Controller.snip_pathsfrom(term.getClojureTerm(), (IPersistentList) RT.readString(path));
        ISeq resSeq = res.seq();

        while(resSeq != null && resSeq.count() > 0){
            results.add(Term.create((IPersistentMap) resSeq.first()));
            resSeq = resSeq.next();
        }

        return results;
    }

    // Internal implementation //
    private static void load_csneps(){
        IFn require = Clojure.var("clojure.core", "require");
        require.invoke(Clojure.read("csneps.core.snuser"));
        IFn startExecutor = Clojure.var("csneps.snip.inference-graph.concurrent", "startExecutor");
        startExecutor.invoke();
        clearkb(true);
        GUI2.model = new Model();
        Controller.gui_add_watches(GUI2.getModel());
        GUI2.initializeModel();
    }
}
