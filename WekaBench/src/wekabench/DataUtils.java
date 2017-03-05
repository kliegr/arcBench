
package wekabench;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Enumeration;
import weka.core.Attribute;
import weka.core.Instances;
import weka.core.converters.ArffSaver;
import weka.core.converters.CSVLoader;
import weka.filters.Filter;
import weka.filters.unsupervised.attribute.NumericToNominal;
import weka.filters.unsupervised.attribute.Remove;
import weka.filters.unsupervised.attribute.StringToNominal;


public class DataUtils {
     public static void saveDataAsARFF(Instances data, String arffpath) throws IOException
    {
        ArffSaver saver = new ArffSaver();
        saver.setInstances(data);
        saver.setFile(new File(arffpath));
        saver.writeBatch();
        
    }
    public static Instances loadData(String path,String targetVariable, ArrayList<Integer> attToRemove) throws IOException, Exception{
      
       CSVLoader loader = new CSVLoader();
       System.out.println("Loading dataset:" + path);
       loader.setSource(new File(path));    
       Instances data = loader.getDataSet(); 
       
       if (targetVariable == null)
       {
           data.setClass(data.attribute(data.numAttributes()-1));           
       }
       else
       {
           data.setClass(data.attribute(targetVariable));
       }
                           
       //remove attribute if it has no values
       Enumeration<Attribute> e = data.enumerateAttributes();
       
        //determine which attributes are empty
        //if so create filter removing the attribute 
        //and apply it on training and test data                
       if (attToRemove == null) //this line ensures that the empty check is performed only on training data
       {
            attToRemove=new ArrayList();
            int i=0;       
            while (e.hasMoreElements())
            {
                Attribute a  = e.nextElement();
                
                if (a.numValues() == 0 & !a.isNumeric())
                 {
                      attToRemove.add(i);
                 }
                i++;
            }
            if (attToRemove.size()>0)
            {
                
                Remove removeFilter= new Remove();
                removeFilter.setAttributeIndicesArray(attToRemove.stream().mapToInt(j -> j).toArray());
                removeFilter.setInputFormat(data); 
                data=Filter.useFilter(data, removeFilter);
            }
               
       }

       //FURIA cannot handle numeric class
       if (data.classAttribute().isNumeric())
       {
            NumericToNominal convertNumeric= new NumericToNominal();/*
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
            convertNumeric.setAttributeIndices("last");
            convertNumeric.setInputFormat(data);
            data=Filter.useFilter(data, convertNumeric);
       }
       StringToNominal convertString= new StringToNominal();
       convertString.setAttributeRange("first-last");   
       convertString.setInputFormat(data);
       data=Filter.useFilter(data, convertString);
       return data;

  }
}
