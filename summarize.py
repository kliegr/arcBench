import os
import pandas as pd
import re
from scipy.stats import  wilcoxon
os.chdir("CBA_results")

main="117-noExtend-D-mci=0-cba.csv"
roundingPlaces=3
df_reference=pd.read_csv(main,delimiter=",",index_col=False,engine="python")
first=True
resultFile="summary.csv"
if os.path.isfile(resultFile):
    os.remove(resultFile) 
for filename in os.listdir("."):
    if not re.search("^\d+", filename): 
        continue
    if filename.endswith(".csv"): 
        print("processing " + filename)
        df = pd.read_csv(filename)
        if (len(df.columns) != 5):
            print("skipping")
            continue
        dfout = df.mean()
        median = df.buildtime.median()
        dfout["name"] = filename
        dfout["datasets"] = len(df)
        dfout["median-buildtime"] = median
        #compute won-tie-loss
        if filename!=main:
            dfMerged=pd.merge(df, df_reference, how='inner', on="dataset",suffixes=('_cl1', '_cl2'))
            dfout["won"]=len(dfMerged[dfMerged.accuracy_cl1.round(roundingPlaces) > dfMerged.accuracy_cl2.round(roundingPlaces)])
            dfout["tie"]=len(dfMerged[dfMerged.accuracy_cl1.round(roundingPlaces) == dfMerged.accuracy_cl2.round(roundingPlaces)])
            dfout["loss"]=len(dfMerged[dfMerged.accuracy_cl1.round(roundingPlaces) < dfMerged.accuracy_cl2.round(roundingPlaces)])
            dfout["omitted"]=abs(len(df)-len(df_reference))
            dfout["wilcoxonPval"]=wilcoxon(dfMerged.accuracy_cl1,dfMerged.accuracy_cl2)[1]
        dfout = dfout[["name","datasets","accuracy","rules","antlength","buildtime","median-buildtime","won","tie","loss","omitted","wilcoxonPval"]]
        with open(resultFile, 'a') as f:
            dfout.to_frame().transpose().to_csv(f, header=first,index=False)
        first=False
    else:
        continue

print("written results to: CBA_results/"+ resultFile)