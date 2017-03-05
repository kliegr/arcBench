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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintWriter;
import weka.classifiers.Classifier;
import weka.classifiers.Evaluation;
import weka.classifiers.misc.InputMappedClassifier;
import weka.classifiers.rules.FURIA;
import weka.classifiers.rules.JRip;
import weka.classifiers.rules.PART;
import weka.classifiers.trees.J48;
import weka.core.Instances;


public class DataLoadEval {
  
    
  public static void main(String[] args) throws Exception {
      String path="";
      if (args.length ==1)
      {
          path = args[0];
      }
   evalTimeKDD(path);

  }

  private static void evalTimeKDD(String path) throws FileNotFoundException, Exception
  {           
      String outputPath = path + "result/";
      String targetVariable="label";
      String[] algorithms = {"J48","PART","FURIA","JRip"};    
      int ITERATIONS=3;      
      for (String algorithm:algorithms)
      {
            File f = new File(outputPath+ algorithm + "-scaling.csv");      
            PrintWriter writer;
            boolean exists = f.isFile();
            writer = new PrintWriter( new FileOutputStream(f,true));
            if (!exists)
            {
                writer.write("trainPath,testPath,accuracy,milliseconds\n");
            }

            int[] folds = {1000,10000,20000,30000,40000,50000,100000,500000,1000000,3000000,4898431};

            for (int fold =0; fold< folds.length;fold++)
            {                
                  int foldsize = folds[fold];
                  if (foldsize>50000) continue;
                  String trainPath=path +"data/scaling/KDDCup99_"+foldsize+".csv";      
                  String testPath=path +"data/scaling/KDDCup99_full_test.csv";      

                  evalStep(trainPath, testPath, algorithm, targetVariable, ITERATIONS, writer);
          }
          writer.flush();
          writer.close();
      }      
  }
    private static void evalStep(String trainPath, String testPath, String algorithm, String targetVariable, int ITERATIONS, PrintWriter writer) throws Exception {
        System.out.println("trainPath:" + trainPath);
        System.out.println("testPath:" + testPath);
        System.out.println("algorithm:" + algorithm);
        
        Instances test=DataUtils.loadData(testPath,targetVariable,null);
        Instances train=DataUtils.loadData(trainPath,targetVariable,null);
        System.out.println("Data loaded");              
        
        Evaluation eval = new Evaluation(train);

        Classifier cl;
        if (algorithm.equals("J48"))
        {
            cl = new J48();
        }
        else if (algorithm.equals("JRip"))
        {
            cl = new JRip();
        }
        else if (algorithm.equals("PART"))
        {
            cl = new PART();
        }
        else if (algorithm.equals("FURIA"))
        {
            cl = new FURIA();
        }
        else
        {
            throw new Exception("unknown algorithm");
        }
        
        InputMappedClassifier imc = new InputMappedClassifier();
        imc.setClassifier(cl);
        
        long start = System.currentTimeMillis();
        for (int i=0;i<ITERATIONS;i++)
        {
            try{
                imc.buildClassifier(train);
            }
            catch(Exception e)
            {
                e.printStackTrace();
                writer.append(trainPath +"," +  testPath + ","+ e.getMessage()+"\n");
            }
        }
        long end = System.currentTimeMillis();
        
        float averageModelBuildTime =  (end-start)/ITERATIONS ;
        System.out.println("Model building took " + averageModelBuildTime + " ms.");
        eval.evaluateModel(imc, test);
        System.out.println("Accuracy:"+eval.pctCorrect()/100);
        writer.append(trainPath +"," +  testPath + ","+eval.pctCorrect()/100 + "," +averageModelBuildTime+"\n");
        writer.flush();
    }
}
