package edu.buffalo.cse.sneps3.gui.business.repl;

import java.io.IOException;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.util.Vector;

import clojure.lang.IFn;
import clojure.lang.RT;
import clojure.lang.Var;
import clojure.tools.nrepl.Connection;
import edu.buffalo.cse.sneps3.gui.REPLPanel;

public class REPLView {

	private REPLPanel replPanel;
	
	private String currentNamespace = "csneps.core.snuser";
	
	private Connection nreplConnection;
	private String sessionId;
	
	private Var configureRepl = RT.var("csneps.gui", "configure-repl");
	private IFn evalExpression;
	
	public String getCurrentNamespace(){
		return currentNamespace;
	}
	
	public void setCurrentNamespace(String ns){
		currentNamespace = ns;
	}
	
	private void prepareRepl(Connection c){
		sessionId = nreplConnection.newSession(null);
		System.out.println(sessionId);
		evalExpression = (IFn)configureRepl.invoke(this, replPanel.getLogComponent(), nreplConnection.client, sessionId);
	}
	
	public void eval(String expr){
		evalExpression.invoke(expr, true);
	}
	
	public void log(String s){
		replPanel.appendText(s);
	}

	//Vector<IREPLView> views;
	//PipedInputStream input
	
	public REPLView(REPLPanel replPanel, Connection nreplConnection){
		this.replPanel = replPanel;
		this.nreplConnection = nreplConnection;
		
		prepareRepl(nreplConnection);
		//views = new Vector<PipedOutputStream>();
	}
	
	
	public static void readStream(PipedInputStream input) throws IOException{
		byte[] buffer = new byte[1024];
		int bytesRead;
		while((bytesRead = input.read(buffer)) != -1){
			System.out.println(new String(buffer));
		}
	}
	
	//Testing
	public static void main(String[] args) throws IOException{
		PipedInputStream in = new PipedInputStream();
		final PipedOutputStream out = new PipedOutputStream(in);
		new Thread(
			    new Runnable(){
			      public void run(){
			        try {
						out.write(new String("Test test test").getBytes());
					} catch (IOException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
			      }
			    }
			  ).start();
		readStream(in);
	}
	
}
