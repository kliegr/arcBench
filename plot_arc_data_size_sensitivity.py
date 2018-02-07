%matplotlib inline
import matplotlib.pyplot as plt
import pandas as pd

df = pd.read_csv("result/arc-data-size.csv", usecols=["input rules","time"])
p = df.plot()
p.show()


