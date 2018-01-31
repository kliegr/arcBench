from IDS_deterministic_local import *
from timeout import timeout
from timeout import TimeoutError
datasets = ["anneal","australian","autos","breast-w","colic","credit-a","credit-g","diabetes","glass","heart-statlog","hepatitis","hypothyroid","ionosphere","iris","labor","letter","lymph","segment","sonar","spambase","vehicle","vowel"]

def IDS_det_dataset(filename):
    print (filename)
    df=pd.read_csv(filename)
    df = pd.read_csv(filename)
    df1 = df.iloc[:,-1]
    cl_name=df.columns.tolist()[len(df.columns)-1]
    df.drop(cl_name, axis=1)
    Y = list(df1)

    print("starting apriori")
    itemsets = run_apriori(df, 0.01)
    print("finished apriori")
    list_of_rules = createrules(itemsets, list(set(Y)))
    print("----------------------")
    for r in list_of_rules:
        r.print_rule()
    print(len(list_of_rules))
    lambda_array = [0.5]*7     # use separate hyperparamter search routine
    epsilon = 0.05


    soln_set, obj_val = deterministic_local_search(list_of_rules, df, Y, lambda_array, epsilon)
    return (soln_set, obj_val)

with open('uci.csv', 'wb') as csvfile:
    writer = csv.writer(csvfile, delimiter=',')
    writer.writerow(["dataset","sel_rules","obj_value"])

    for dataset in datasets:  
        try: 
            with timeout(seconds=1800):
                filename = "data/folds/test/"+dataset+"0.csv"
                soln_set, obj_val=IDS_det_dataset(filename)
                writer.writerow([dataset,len(soln_set)])
                csvfile.flush()
        except TimeoutError:
            print("TimeoutError")
            writer.writerow([dataset,"TimeoutError"])
            csvfile.flush()
            continue