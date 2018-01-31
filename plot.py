%matplotlib inline
import matplotlib.pyplot as plt
import pandas as pd

df = pd.read_csv("result-rulecount.csv", usecols=["input_rules","elaps_time"])
p = df.plot()
p.show()


