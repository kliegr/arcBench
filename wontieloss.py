import pandas as pd
from scipy.stats import  wilcoxon
cols=["dataset","accuracy","optimized","time","numerical"]
main="result/MARC-MultiRule-accuracy.csv"
other=[{"fname":"result/J48-accuracy.csv","friendly":"J48 auto"},{"fname":"result/PART-accuracy.csv","friendly":"PART auto"},{"fname":"result/RIPPER-accuracy.csv","friendly":"RIPPER auto"},{"fname":"result/FURIA-accuracy.csv","friendly":"FURIA auto"},{"fname":"result/CBA-accuracy.csv","friendly":"CBA"},{"fname":"result/MARC-OneRule-accuracy.csv","friendly":"MARC One Rule"}]
df1=pd.read_csv(main,delimiter=",",index_col=False,engine="python")
fo = open(main+".wontieloss", 'w')
fo.write('dataset &  M-MultiRule & tie  & loss & omitted & p \\\\\n')
#number of rows in baseline with nan accuracy
if df1.accuracy.isnull().any():
    print "dropping " + str(sum(df1.accuracy.isnull()))+ " entry with nan values from baseline"
    df1=df1.drop(df1.accuracy.isnull())
for f in other:
    #df2=pd.read_csv(f["fname"],delimiter=r"\s+&\s+",index_col=False,skipfooter=3,engine="python")	
    df2=pd.read_csv(f["fname"],delimiter=",",index_col=False,engine="python")
    df=pd.merge(df1, df2, how='inner', on="dataset",suffixes=('_cl1', '_cl2'))
    print ("xxxx")
    print (f)
    won=len(df[df.accuracy_cl1.round(2) > df.accuracy_cl2.round(2)])
    print ('M-MultiRule won: %.0f' % won )
    tie=len(df[df.accuracy_cl1.round(2) == df.accuracy_cl2.round(2)])
    print ('tie: %.0f' % tie)
    loss=len(df[df.accuracy_cl1.round(2) < df.accuracy_cl2.round(2)])
    print ('loss: %.0f' % loss)
    omitted=abs(len(df1)-len(df2))
    print ('omitted: %.0f' % omitted)
    stat=wilcoxon(df.accuracy_cl1,df.accuracy_cl2)
    print "Wilcoxon signed-rank test p-value",stat[1]
    fo.write('%s & %.0f & %.0f& %.0f & %.0f & %.5f \\\\\n'% (f["friendly"], won,tie,loss,omitted,stat[1]))
fo.close()