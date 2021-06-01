package csneps.api;

import clojure.java.api.Clojure;
import clojure.lang.IFn;

public class CSNePS {
    private static IFn csneps_start_gui_fn;
    private static IFn snuser_clearkb_fn;

    public CSNePS(){
        load_csneps();
    }

    public void startGUI(){
        gui_startGUI();
    }

    public void clearkb(boolean clearall){
        snuser_clearkb(clearall);
    }

    // Internal implementation //
    private static void load_csneps(){
        IFn require = Clojure.var("clojure.core", "require");
        require.invoke(Clojure.read("csneps.core.snuser"));
        IFn startExecutor = Clojure.var("csneps.snip.inference-graph.concurrent", "startExecutor");
        startExecutor.invoke();
        snuser_clearkb(true);
    }

    private static void gui_startGUI() {
        if (csneps_start_gui_fn == null)
            csneps_start_gui_fn = Clojure.var("csneps.gui", "startGUI");
        csneps_start_gui_fn.invoke();
    }

    private static void snuser_clearkb(boolean clearall) {
        if (snuser_clearkb_fn == null)
            snuser_clearkb_fn = Clojure.var("csneps.core.snuser", "clearkb");
        snuser_clearkb_fn.invoke(clearall);
    }
}
