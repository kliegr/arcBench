%matplotlib inline
import matplotlib.pyplot as plt
import pandas as pd

df = pd.read_csv("result/ids-rulecount.csv", usecols=["input_rules","elaps_time"])
p = df.plot()
p.show()


