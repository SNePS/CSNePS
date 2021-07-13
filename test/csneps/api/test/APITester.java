package csneps.api.test;

import csneps.api.CSNePS;
import csneps.gui.business.Context;
import csneps.gui.business.Term;

public class APITester {
    public static void main(String[] args){
        CSNePS csneps = new CSNePS();
        csneps.startGUI();
        System.out.println(Context.getContexts());
        System.out.println(CSNePS.pathsfrom(Term.getTerm("Entity"), "(compose class- member)"));
        System.out.println(Term.getTerm("wft1").getDescription());
    }
}
