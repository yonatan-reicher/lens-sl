import matplotlib.pyplot as plt
import numpy as np
import pandas as p
from pandas import DataFrame

np.random.seed(19680801)

lens = p.read_csv('lens.csv').replace('timeout', '11')
sl = p.read_csv('sl.csv').replace('timeout', '11')

time_column = 'time(seconds/timeout)'

fig, (ax_left, ax_right) = plt.subplots(1, 2)
lim = 0.3
ax_left.set_title('benchmark execution times (seconds)\nLower is better')
ax_right.set_title('benchmark execution times (seconds [Zoomed in])\nLower is better')
ax_right.set_xlim(0, lim)
ax_right.set_ylim(0, lim)

xs = lens[time_column].astype(float)
ys = sl[time_column].astype(float)
for ax in (ax_left, ax_right):
    ax.set_xlabel('lens')
    ax.set_ylabel('sl')
    ax.scatter(
        xs,
        ys,
        s=100,
        label=lens['name'],
        color='red',
        alpha=0.3,
        edgecolors='none',
    )
    ax.axline((0, 0), (11, 11))

    for i, (x, y) in enumerate(zip(xs, ys)):
        ax.annotate(lens['name'][i], xy=(x, y))

plt.show()
