package liir.nlp.coref.ilp;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.SortedSet;
import java.util.TreeSet;

import gurobi.*;

public class ILPModel {
	
	GRBEnv    env ;
    GRBModel  model ;
    HashMap<String, GRBVar> varmap;
    int contraint_num=0;
    
    
	public ILPModel ()
	{
		try	{
				env   = new GRBEnv();
				model = new GRBModel(env);
				varmap=new HashMap<String, GRBVar>();
	
			}
		catch (GRBException e) {
	      System.out.println("Error code: " + e.getErrorCode() + ". " +
	                         e.getMessage());
	    }
	}
	
	public ILPModel (String logfile)
	{
		try	{
				env   = new GRBEnv(logfile);
				model = new GRBModel(env);
				varmap=new HashMap<String, GRBVar>();
	
			}
		catch (GRBException e) {
	      System.out.println("Error code: " + e.getErrorCode() + ". " +
	                         e.getMessage());
	    }
	}
	
	public ILPModel (String logfile, String modelfile)
	{
		try	{
				env   = new GRBEnv(logfile);
				model = new GRBModel(env, modelfile);
				varmap=new HashMap<String, GRBVar>();
			}
		catch (GRBException e) {
	      System.out.println("Error code: " + e.getErrorCode() + ". " +
	                         e.getMessage());
	    }
	}
	
	
	public void addBinaryVar(String real_name)
	{
		try {
			GRBVar x = model.addVar(0.0, 1.0, 0.0, GRB.BINARY, real_name);
			model.update();
			varmap.put(real_name, x);
		} catch (GRBException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public void removeVar(String real_name) throws GRBException{
		System.out.println("remove var" + varmap.get(real_name));
		model.remove(varmap.get(real_name));
		model.update();
	}
	
	public void addBinaryVar(ArrayList<String> real_names)
	{
		try {
			for (String real_name : real_names){
				GRBVar x = model.addVar(0.0, 1.0, 0.0, GRB.BINARY, real_name);			
				varmap.put(real_name, x);
			}
			model.update();
		} catch (GRBException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public void addNormalConstraint(ArrayList<String> vars, ArrayList<Double> weights, String operation, double obj )
	{
		try {
		GRBLinExpr expr = new GRBLinExpr();
		for (int i =0 ; i< vars.size() ; i++)
		{
			expr.addTerm(weights.get(i), varmap.get(vars.get(i)));
		}
		switch (operation) {
			case ">=": 
			
				model.addConstr(expr, GRB.GREATER_EQUAL, obj, "c"+ String.valueOf(contraint_num));
				contraint_num +=1;
				break;
			case "<=":
				model.addConstr(expr, GRB.LESS_EQUAL, obj, "c"+ String.valueOf(contraint_num));
				contraint_num +=1;

				break;
			default :
				model.addConstr(expr, GRB.EQUAL, obj, "c"+ String.valueOf(contraint_num));
				contraint_num +=1;

				break;
				
		}
		
		} catch (GRBException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	
	public void addNormalConstraintF(ArrayList<String> vars, ArrayList<Float> weights, String operation, double obj )
	{
		try {
		GRBLinExpr expr = new GRBLinExpr();
		for (int i =0 ; i< vars.size() ; i++)
		{
			expr.addTerm(weights.get(i), varmap.get(vars.get(i)));
		}
		switch (operation) {
			case ">=": 
			
				model.addConstr(expr, GRB.GREATER_EQUAL, obj, "c"+ String.valueOf(contraint_num));
				contraint_num +=1;
				break;
			case "<=":
				model.addConstr(expr, GRB.LESS_EQUAL, obj, "c"+ String.valueOf(contraint_num));
				contraint_num +=1;

				break;
			default :
				model.addConstr(expr, GRB.EQUAL, obj, "c"+ String.valueOf(contraint_num));
				contraint_num +=1;

				break;
				
		}
		
		} catch (GRBException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public void setObjective(ArrayList<String> vars, ArrayList<Double> weights, int type)
	{
		try {
		GRBLinExpr expr = new GRBLinExpr();
		for (int i =0 ; i< vars.size() ; i++)
		{
			expr.addTerm(weights.get(i), varmap.get(vars.get(i)));
		}
		if (type == 1)
			
				model.setObjective(expr, GRB.MAXIMIZE);
			
		else 
			model.setObjective(expr, GRB.MINIMIZE);
		
		
		} catch (GRBException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
			
	}
	
	public void setObjectiveF(ArrayList<String> vars, ArrayList<Float> weights, int type)
	{
		try {
		GRBLinExpr expr = new GRBLinExpr();
		for (int i =0 ; i< vars.size() ; i++)
		{
			expr.addTerm(weights.get(i), varmap.get(vars.get(i)));
		}
		if (type == 1)
			
				model.setObjective(expr, GRB.MAXIMIZE);
			
		else 
			model.setObjective(expr, GRB.MINIMIZE);
		
		
		} catch (GRBException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
			
	}
	
	public void dispose () throws GRBException
	{
		model.dispose();
	     env.dispose();
	}
	
	public void writeModel(String output) throws GRBException
	{
		model.update();
		model.write(output);
	}
	
	
		
		
	public double getBestObj() throws GRBException{
		return model.get(GRB.DoubleAttr.ObjVal);
	}
	
	public void optimize_only() throws GRBException{
		System.out.println("Optimizing model");
		model.optimize();
	//	model.update();
	}
	
	public HashMap<String,Double> get_results() throws GRBException{
		GRBVar[] vars = model.getVars();
		HashMap<String,Double> values=new HashMap<>();
		try{
		for (GRBVar v : vars){
			values.put(v.get(GRB.StringAttr.VarName), v.get(GRB.DoubleAttr.X));
			
		}
		}
		catch(Exception e){
			System.out.println(e.toString());
		}
		return values;
	}
	
	
	
	public HashMap<String,Double> optimize() throws GRBException{
		model.optimize();
		GRBVar[] vars = model.getVars();
		HashMap<String,Double> values=new HashMap<>();
		for (GRBVar v : vars){
			values.put(v.get(GRB.StringAttr.VarName), v.get(GRB.DoubleAttr.X));
			
		}
		values.put("--OBJ--", model.get(GRB.DoubleAttr.ObjVal));
		return values;
		
	}
	
	public void optimizeAndSave(String output) throws GRBException, FileNotFoundException, UnsupportedEncodingException{
		model.optimize();
		GRBVar[] vars = model.getVars();
	//	HashMap<String,Double> values=new HashMap<>();
		PrintWriter writer = new PrintWriter(output, "UTF-8");

		for (GRBVar v : vars){
		    Double tmp = v.get(GRB.DoubleAttr.X);
					if  (tmp != 0.0)
		//	values.put(v.get(GRB.StringAttr.VarName), v.get(GRB.DoubleAttr.X));
					{
						  writer.print(v.get(GRB.StringAttr.VarName) + "\t");
					   writer.print(v.get(GRB.DoubleAttr.X));
					   writer.println();
					}
		}
		/*
		SortedSet<String> keys = new TreeSet<String>(values.keySet());
		
		PrintWriter writer = new PrintWriter(output, "UTF-8");
		for (String key : keys) { 
			   // do something
			   writer.print(key + "\t");
			   writer.print(values.get(key));
			   writer.println();
			}
			
			*/
		
		writer.print("--OBJ--" + "\t");
		   writer.print(model.get(GRB.DoubleAttr.ObjVal));
		   writer.println();
		
		   writer.close();
		   model.dispose();
		
		
	}
	
	public static void main(String[] args) throws GRBException, FileNotFoundException, UnsupportedEncodingException {
		/*
		ILPModel m = new ILPModel("log.txt");
		
		ArrayList<String> arr=new ArrayList<>();
		arr.add("x1");
		arr.add("x2");
		arr.add("x3");
		m.addBinaryVar(arr);
		
		ArrayList<Double> c=new ArrayList<>();
		c.add(1.0);
		c.add(3.0);
		c.add(4.0);
		m.addNormalConstraint(arr, c, "<=", 5.0);
		c.clear();
		c.add(1.0);
		c.add(-2.0);
		c.add(2.0);
		m.setObjective(arr, c, 1);
		
		
		m.writeModel("model.lp");
		*/
		ILPModel m = new ILPModel("log.txt", "model.lp");
		m.optimizeAndSave("out.ilp.txt");
		/*
		HashMap<String,Double>  h = m.optimize();
		
		for (String key : h.keySet()){
			System.out.println(key + " " + h.get(key));
		}
		*/
		
	}
}