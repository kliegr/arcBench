from IDS_deterministic_local import *

filename='data/folds/test/labor0.csv'
df = pd.read_csv(filename)
df1 = df.iloc[:,-1]
cl_name=df.columns.tolist()[len(df.columns)-1]
df.drop(cl_name, axis=1)
Y = list(df1)

print("starting apriori")
itemsets = run_apriori(df, 0.8)
print("finished apriori")
list_of_rules = createrules(itemsets, list(set(Y)))
print("----------------------")
for r in list_of_rules:
    r.print_rule()
print(len(list_of_rules))
lambda_array = [0.5]*7     # use separate hyperparamter search routine
epsilon = 0.05

with open('result/ids-rulecount.csv', 'wb') as csvfile:
    writer = csv.writer(csvfile, delimiter=',')
    writer.writerow(["filename","input_rules","sel_rules","obj_value","elaps_time"])
    for l in range(10,20)+range(20,100,10)+range(100,1100,100):
        if l < 200:
            continue
        print("starting ids with l=" + str(l))
        sublist = list_of_rules[0:l]
        start_time = datetime.datetime.now()
        for i in range(1,11):
            soln_set, obj_val = deterministic_local_search(sublist, df, Y, lambda_array, epsilon)
        elapsed_time = (datetime.datetime.now() - start_time)
        writer.writerow([filename,len(sublist),soln_set,obj_val,elapsed_time/10])
        csvfile.flush()
        print(soln_set)
        print(obj_val)
        print(elapsed_time)
        print("finished ids")