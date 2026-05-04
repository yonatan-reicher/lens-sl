import matplotlib.pyplot as plt
import numpy as np
import pandas as p

np.random.seed(19680801)

time_column = 'time(seconds/timeout)'

lens = p.read_csv('csvs/lens-2026-04-27.csv')
sl = p.read_csv('csvs/lens-with-discard-2026-04-29.csv')
# Set name as the index
lens.set_index('name', inplace=True)
sl.set_index('name', inplace=True)
# Keep only shared names
lens = lens[lens.index.isin(sl.index)]
sl = sl[sl.index.isin(lens.index)]
# Replace 'timeout' with some maximum value
max_time = 3600
lens.replace('timeout', max_time, inplace=True)
sl.replace('timeout', max_time, inplace=True)
lens[time_column] = lens[time_column].astype(float)
sl[time_column] = sl[time_column].astype(float)

commands = ('graph', 'ratio')
while True:
    command = input(f"Enter command ({'/'.join(commands)}): ")
    if command in commands: break

if command == 'ratio':
    # Map `sl`'s time column to be divided by the corresponding `lens` time column
    print(sl)
    print(lens)
    print(sl[time_column] / lens[time_column])
elif command == 'graph':
    fig, (ax_left, ax_right) = plt.subplots(1, 2)
    lim = 0.3
    ax_left.set_title('benchmark execution times (seconds)\nLower is better')
    ax_right.set_title('benchmark execution times (seconds [Zoomed in])\nLower is better')
    ax_right.set_xlim(0, lim)
    ax_right.set_ylim(0, lim)
    #
    xs = lens[time_column].astype(float)
    ys = sl[time_column].astype(float)
    for ax in (ax_left, ax_right):
        ax.set_xlabel('lens')
        ax.set_ylabel('sl')
        ax.scatter(
            xs,
            ys,
            s=100,
            label=lens.index,
            color='red',
            alpha=0.3,
            edgecolors='none',
        )
        ax.axline((0, 0), (11, 11))
        #
        # for i, (x, y) in enumerate(zip(xs, ys)):
        #     ax.annotate(lens['name'][i], xy=(x, y))
    #
    plt.show()
