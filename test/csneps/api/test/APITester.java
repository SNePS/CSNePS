package csneps.api.test;

import csneps.api.CSNePS;
import csneps.gui.business.Context;

public class APITester {
    public static void main(String[] args){
        CSNePS csneps = new CSNePS();
        csneps.startGUI();
        System.out.println(Context.getContexts());
    }
}
