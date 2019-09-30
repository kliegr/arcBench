import pandas as pd
from scipy.stats import  wilcoxon
cols=["dataset","accuracy","optimized","time","numerical"]
#This corresponds to configuration QCBA#6 from the paper
main="CBA_results/198-numericOnly-T-Pcba-A-mci=0-qcba.csv"
other=[{"fname":"WEKA_results/J48-accuracy.csv","friendly":"WEKA_results/J48 auto"},{"fname":"WEKA_results/PART-accuracy.csv","friendly":"PART auto"},{"fname":"WEKA_results/RIPPER-accuracy.csv","friendly":"RIPPER auto"},{"fname":"WEKA_results/FURIA-accuracy.csv","friendly":"FURIA auto"},{"fname":"WEKA_results/CBA-accuracy.csv","friendly":"CBA"},{"fname":"SBRL_results/SBRL-Long.csv","friendly":"SBRL - Long"},
{"fname":"IDS_results/IDS.csv","friendly":"IDS"}]
df1=pd.read_csv(main,delimiter=",",index_col=False,engine="python")
fo = open("WEKA_results/wontielosstable.tex", 'w')
fo.write('dataset &  ' + main+  '& tie  & loss & omitted & p \\\\\n')

#number of rows in baseline with nan accuracy
if df1.accuracy.isnull().any():
    print("dropping " + str(sum(df1.accuracy.isnull()))+ " entry with nan values from baseline")
    df1=df1.drop(df1.accuracy.isnull())
for f in other:
    #df2=pd.read_csv(f["fname"],delimiter=r"\s+&\s+",index_col=False,skipfooter=3,engine="python")	
    df2=pd.read_csv(f["fname"],delimiter=",",index_col=False,engine="python")
    df=pd.merge(df1, df2, how='inner', on="dataset",suffixes=('_cl1', '_cl2'))
    print (f)
    won=len(df[df.accuracy_cl1.round(2) > df.accuracy_cl2.round(2)])
    print ('QCBA #5 won: %.0f' % won )
    tie=len(df[df.accuracy_cl1.round(2) == df.accuracy_cl2.round(2)])
    print ('tie: %.0f' % tie)
    loss=len(df[df.accuracy_cl1.round(2) < df.accuracy_cl2.round(2)])
    print ('loss: %.0f' % loss)
    omitted=abs(len(df1)-len(df))
    print ('omitted: %.0f' % omitted)
    #The warning "RuntimeWarning: invalid value encountered in less r_minus" is generated only if the code is run with Python 3, 
    # when it is run with Python 2, there is no warning. The results are the same for Python 2 and Python 3
    stat=wilcoxon(df.accuracy_cl1,df.accuracy_cl2)
    print ("Wilcoxon signed-rank test p-value",stat[1])
    fo.write('%s & %.0f & %.0f& %.0f & %.0f & %.5f \\\\\n'% (f["friendly"], won,tie,loss,omitted,stat[1]))
fo.close()