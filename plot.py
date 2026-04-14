import matplotlib.pyplot as plt
import numpy as np
import pandas as p
from pandas import DataFrame

np.random.seed(19680801)

lens = p.read_csv('lens.csv').replace('timeout', '11')
sl = p.read_csv('sl.csv').replace('timeout', '11')

time_column = 'time(seconds/timeout)'

fig, ax = plt.subplots()
ax.set_title('benchmark execution times (seconds)\nLower is better')
ax.set_xlabel('lens')
ax.set_ylabel('sl')
ax.scatter(
    lens[time_column].astype(float),
    sl[time_column].astype(float),
    s=100,
    label=lens['name'],
    color='red',
    alpha=0.3,
    edgecolors='none',
)
ax.axline((0, 0), (11, 11))

ax.set_xscale('log')
ax.set_yscale('log')

plt.show()
