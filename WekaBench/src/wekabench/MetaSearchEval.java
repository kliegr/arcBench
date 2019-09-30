/*
 * Weka ML benchmark
 *
 *     Copyright (C)2016 Tomas Kliegr
 *
 *     This program is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Affero General Public License as
 *     published by the Free Software Foundation, either version 3 of the
 *     License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Affero General Public License for more details.
 *
 *     You should have received a copy of the GNU Affero General Public License
 *     along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package wekabench;

import java.io.PrintWriter;
import weka.classifiers.Evaluation;
import weka.classifiers.meta.MultiSearch;
import weka.classifiers.meta.multisearch.DefaultEvaluationMetrics;
import weka.classifiers.misc.InputMappedClassifier;
import weka.classifiers.rules.FURIA;
import weka.classifiers.rules.JRip;
import weka.classifiers.rules.PART;
import weka.classifiers.trees.J48;
import weka.core.Instances;
import weka.core.SelectedTag;
import weka.core.Utils;
import weka.core.setupgenerator.AbstractParameter;
import weka.core.setupgenerator.ListParameter;
import weka.core.setupgenerator.MathParameter;

/**
 * A little demo java program for using WEKA.<br/>
 * Check out the Evaluation class for more details.
 * 
 * @author FracPete (fracpete at waikato dot ac dot nz)
 * @version $Revision: 10229 $
 * @see Evaluation
 */
public class MetaSearchEval {
  
    
    


  public static void main(String[] args) throws Exception {
      String path="";      
      String targetVariable=null;
      
      String[] datasets =  {"anneal","australian","autos","breast-w","colic","credit-a","credit-g","diabetes","glass","heart-statlog","hepatitis","hypothyroid","ionosphere","iris","labor","letter","lymph","segment","sonar","spambase","vehicle","vowel"};    
      String[] algorithms = {"J48","JRip","FURIA","PART"};
      
      
      if (args.length ==1)
      {
          path = args[0];
      }
      if (args.length ==2)
      {
          path = args[0];
          algorithms = new String[] {args[1]};                    
      }
      if (args.length ==4)
      {
          path = args[0];
          algorithms = args[1].split(";");     
          datasets = args[2].split(";");     
          targetVariable = args[3];          
      }
      
    
     for (String algorithm : algorithms)
     {
        PrintWriter writer = new PrintWriter(path+"WEKA_results/" + algorithm + "-accuracy.csv", "UTF-8");
        writer.println("dataset,accuracy,rules");
        for (String dataset : datasets)
        {
            if (dataset.equals("letter") & algorithm.equals("PART")) continue;
            double accSum=0;
            double ruleSum=0;
            int foldSuccessCount=0;
            for (int fold=0;fold < 10; fold++)
            {
                
                Instances train=DataUtils.loadData(path +"data/folds_nodiscr/train/" + dataset + Integer.toString(fold) + ".csv",targetVariable, null);        
                Instances test=DataUtils.loadData(path +"data/folds_nodiscr/test/" + dataset + Integer.toString(fold) + ".csv",targetVariable,null);
                double[] rs ;
                try {
                    if (algorithm.equals("J48"))
                    {
                        rs = multiSearchJ48(train,test);
                    }
                    else if (algorithm.equals("JRip"))
                    {
                        rs = multiSearchRIPPER(train,test);
                    }
                    else if (algorithm.equals("PART"))
                    {
                        rs = multiSearchPART(train,test);
                    }                        
                    else if (algorithm.equals("FURIA"))
                    {
                        rs = multiSearchFURIA(train,test);
                    }                     
                    else
                    {
                        throw new Exception("unknown algorithm");
                    }                    
                accSum = accSum + rs[1];
                ruleSum = ruleSum + rs[0];
                foldSuccessCount = foldSuccessCount +1;
                }
                catch(Exception e)
                {
                    // This is known to happen on iris and primary-tumor dataset for PART
                    e.printStackTrace();
                    System.out.println("Skip fold on failure");
                }
                
            }
            double accuracy= accSum/foldSuccessCount;
            double rules= ruleSum/foldSuccessCount;
            System.out.println("Accuracy:" + accuracy); 
            System.out.println("Rules:" + rules); 
            writer.println(dataset +"," + accSum/foldSuccessCount + "," + ruleSum/foldSuccessCount);
            writer.flush();
            
        }
     }    
  }

private static double[] multiSearchJ48(Instances train,Instances test) throws Exception{  
    J48 cl = new J48();
    Evaluation eval = new Evaluation(train);
     // configure multisearch
    
    MathParameter conf1 = new MathParameter();
    conf1.setProperty("confidenceFactor");
    conf1.setBase(10);
    conf1.setMin(0.05);
    conf1.setMax(0.4);
    conf1.setStep(0.05);
    conf1.setExpression("I");
    
    MathParameter conf2 = new MathParameter();
    conf2.setProperty("minNumObj");
    conf2.setBase(10);
    conf2.setMin(2);
    conf2.setMax(14);
    conf2.setStep(4);
    conf2.setExpression("I");
    
    MathParameter conf3 = new MathParameter();
    conf3.setProperty("numFolds");
    conf3.setMin(2);
    conf3.setMax(4);
    conf3.setStep(1);
    conf3.setExpression("I");

    ListParameter conf4 = new ListParameter();
    conf4.setProperty("reducedErrorPruning");
    conf4.setList("True False");

    ListParameter conf5 = new ListParameter();
    conf5.setProperty("subtreeRaising");
    conf5.setList("True False");

    ListParameter conf6 = new ListParameter();
    conf6.setProperty("binarySplits");
    conf6.setList("True False");

    
    MultiSearch multi = new MultiSearch();
    multi.setClassifier(cl);
    multi.setSearchParameters(new AbstractParameter[]{
      conf1,conf2,conf3,conf4,conf5,conf6
    });
    SelectedTag tag = new SelectedTag(
    DefaultEvaluationMetrics.EVALUATION_ACC,
    new DefaultEvaluationMetrics().getTags());
    multi.setEvaluation(tag);

    // output configuration
    System.out.println("\nMultiSearch commandline:\n" + Utils.toCommandLine(multi));

    // optimize
    System.out.println("\nOptimizing...\n");
    multi.buildClassifier(train);
    System.out.println("Best setup:\n" + Utils.toCommandLine(multi.getBestClassifier()));
    System.out.println("Best parameter: " + multi.getGenerator().evaluate(multi.getBestValues()));
    
    InputMappedClassifier imc = new InputMappedClassifier();
    imc.setClassifier(multi.getBestClassifier());
    
    imc.buildClassifier(train);
    J48 best = (J48) imc.getClassifier();
    
    double numRules  = best.measureNumRules();
    System.out.println("Number of rules:" + numRules);
    eval.evaluateModel(imc, test);
    double accuracy = eval.pctCorrect()/100;
    System.out.println("Accuracy:" + accuracy); 
    return new double[]{numRules,accuracy};    
}

private static double[] multiSearchFURIA(Instances train,Instances test) throws Exception{  
    FURIA cl = new FURIA();
    Evaluation eval = new Evaluation(train);
     // configure multisearch
    
    
    MathParameter conf1 = new MathParameter();
    conf1.setProperty("minNo");
    conf1.setBase(10);
    conf1.setMin(0);
    conf1.setMax(1);
    conf1.setStep(0.25);
    conf1.setExpression("I");
    
    MathParameter conf2 = new MathParameter();
    conf2.setProperty("folds");
    conf2.setMin(2);
    conf2.setMax(4);
    conf2.setStep(1);
    conf2.setExpression("I");

    
    MultiSearch multi = new MultiSearch();
    multi.setClassifier(cl);
    multi.setSearchParameters(new AbstractParameter[]{
      conf1,conf2
    });
    SelectedTag tag = new SelectedTag(
    DefaultEvaluationMetrics.EVALUATION_ACC,
    new DefaultEvaluationMetrics().getTags());
    multi.setEvaluation(tag);

    // output configuration
    System.out.println("\nMultiSearch commandline:\n" + Utils.toCommandLine(multi));

    // optimize
    System.out.println("\nOptimizing...\n");
    multi.buildClassifier(train);
    System.out.println("Best setup:\n" + Utils.toCommandLine(multi.getBestClassifier()));
    System.out.println("Best parameter: " + multi.getGenerator().evaluate(multi.getBestValues()));
    
    InputMappedClassifier imc = new InputMappedClassifier();
    imc.setClassifier(multi.getBestClassifier());
    
    imc.buildClassifier(train);
    FURIA best = (FURIA) imc.getClassifier();
    
    double numRules  = best.getRuleset().size();
    System.out.println("Number of rules:" + numRules);
   
    eval.evaluateModel(imc, test);
    double accuracy = eval.pctCorrect()/100;
    System.out.println("Accuracy:" + accuracy); 
    return new double[]{numRules,accuracy};    
}
private static double[] multiSearchPART(Instances train,Instances test) throws Exception{  
    PART cl = new PART();
    Evaluation eval = new Evaluation(train);
     // configure multisearch
    
    MathParameter conf1 = new MathParameter();
    conf1.setProperty("confidenceFactor");
    conf1.setBase(10);
    conf1.setMin(0.05);
    conf1.setMax(0.4);
    conf1.setStep(0.05);
    conf1.setExpression("I");
    
    MathParameter conf2 = new MathParameter();
    conf2.setProperty("minNumObj");
    conf2.setBase(10);
    conf2.setMin(2);
    conf2.setMax(14);
    conf2.setStep(4);
    conf2.setExpression("I");
    
    MathParameter conf3 = new MathParameter();
    conf3.setProperty("numFolds");
    conf3.setMin(2);
    conf3.setMax(4);
    conf3.setStep(1);
    conf3.setExpression("I");

    ListParameter conf4 = new ListParameter();
    conf4.setProperty("useMDLcorrection");
    conf4.setList("True False");

    ListParameter conf5 = new ListParameter();
    conf5.setProperty("reducedErrorPruning");
    conf5.setList("True False");

    ListParameter conf6 = new ListParameter();
    conf6.setProperty("unpruned");
    conf6.setList("True False");
    ListParameter conf7 = new ListParameter();
    conf7.setProperty("binarySplits");
    conf7.setList("True False");
    
    MultiSearch multi = new MultiSearch();
    multi.setClassifier(cl);
    multi.setSearchParameters(new AbstractParameter[]{
      conf1,conf2,conf3,conf4,conf5,conf6,conf7
    });
    SelectedTag tag = new SelectedTag(
    DefaultEvaluationMetrics.EVALUATION_ACC,
    new DefaultEvaluationMetrics().getTags());
    multi.setEvaluation(tag);

    // output configuration
    System.out.println("\nMultiSearch commandline:\n" + Utils.toCommandLine(multi));

    // optimize
    System.out.println("\nOptimizing...\n");
    multi.buildClassifier(train);
    System.out.println("Best setup:\n" + Utils.toCommandLine(multi.getBestClassifier()));
    System.out.println("Best parameter: " + multi.getGenerator().evaluate(multi.getBestValues()));
    
    InputMappedClassifier imc = new InputMappedClassifier();
    imc.setClassifier(multi.getBestClassifier());
    
    imc.buildClassifier(train);
    PART best = (PART) imc.getClassifier();
    
    double numRules  = best.measureNumRules();
    System.out.println("Number of rules:" + numRules);
    eval.evaluateModel(imc, test);
    double accuracy = eval.pctCorrect()/100;
    System.out.println("Accuracy:" + accuracy); 
    return new double[]{numRules,accuracy};    
}

private static double[] multiSearchRIPPER(Instances train,Instances test) throws Exception{  
    JRip cl = new JRip();
    Evaluation eval = new Evaluation(train);
     // configure multisearch
     MathParameter conf = new MathParameter();
    conf.setProperty("folds");
    conf.setMin(2);
    conf.setMax(4);
    conf.setStep(1);
    conf.setExpression("I");
    ListParameter conf2 = new ListParameter();
    conf2.setProperty("checkErrorRate");
    conf2.setList("True False");
    ListParameter conf3 = new ListParameter();
    conf3.setProperty("usePruning");
    conf3.setList("True False");    
    
    MultiSearch multi = new MultiSearch();
    multi.setClassifier(cl);
    multi.setSearchParameters(new AbstractParameter[]{
      conf,conf2,conf3
    });
    SelectedTag tag = new SelectedTag(
    DefaultEvaluationMetrics.EVALUATION_ACC,
    new DefaultEvaluationMetrics().getTags());
    multi.setEvaluation(tag);

    // output configuration
    System.out.println("\nMultiSearch commandline:\n" + Utils.toCommandLine(multi));

    // optimize
    System.out.println("\nOptimizing...\n");
   
    multi.buildClassifier(train);
   
    System.out.println("Best setup:\n" + Utils.toCommandLine(multi.getBestClassifier()));
    System.out.println("Best parameter: " + multi.getGenerator().evaluate(multi.getBestValues()));
    
    InputMappedClassifier imc = new InputMappedClassifier();
    imc.setClassifier(multi.getBestClassifier());
    
    imc.buildClassifier(train);
    JRip best = (JRip) imc.getClassifier();
    double numRules = best.getRuleset().size();
    System.out.println("Number of rules:" + numRules);
    eval.evaluateModel(imc, test);
    double accuracy = eval.pctCorrect()/100;
    System.out.println("Accuracy:" + accuracy); 
    return new double[]{numRules,accuracy};    
}
private static void simpleFuria(Instances train,Instances test) throws Exception{  
    FURIA cl = new FURIA();
    Evaluation eval = new Evaluation(train);
    
    InputMappedClassifier imc = new InputMappedClassifier();
    imc.setClassifier(cl);
    imc.buildClassifier(train);

    eval.evaluateModelOnce(imc, test.firstInstance());
    eval.evaluateModel(imc, test);
    System.out.println("Accuracy:"+eval.pctCorrect()/100);    
    }
}
