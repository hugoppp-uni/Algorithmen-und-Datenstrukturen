import numpy as np
import matplotlib.pyplot as plt
import matplotlib.axes._axes as axes
import csv


def generate_graph(titl, xla, yla, d, pathArray, filename, legend, clipBegin=0.0, clipEnd=0.0):
    xArr = []
    yArr = []
    fig = plt.figure()
    ax1 = fig.add_axes((0.1, 0.2, 0.8, 0.7))
    assert isinstance(ax1, axes.Axes)
    for path in pathArray:
        x = []
        y = []
        with open(path, 'r') as csvfile:
            reader = csv.reader(csvfile, delimiter=';')
            next(reader)
            for row in reader:
                x.append(row[0])
                y.append(row[1])
            xArr.append(x)
            yArr.append(y)

    for i in range(0, len(xArr)):
        ax1.plot(np.array(xArr[i], int), np.array(yArr[i], int), '--.', linewidth=1)
    # ax1.axhline(0, color='lightgrey', lw=1)
    # plt.axvline(0, color='lightgrey', lw=1)
    ax1.legend(legend)
    ax1.autoscale(enable=True, axis='both')
    plt.title(titl)
    plt.xlabel(xla)
    plt.ylabel(yla)
    plt.figtext(0.5, 0.06, d, ha="center",  # fontsize=12,
                bbox={"facecolor": "lightgrey", "alpha": 0.5, "pad": 5})
    # fig.set_size_inches(7, 8, forward=True)
    fig.savefig('out/' + filename, bbox_inches='tight')
    # fig.show()
    # ax1.clf()


t = 'Pivot Methods Comparison - First Implementation'
xl = 'Elements'
yl = 'Time / ms'
desc = "Average over 20 runs, random numbers"

generate_graph('', xl, yl, desc,
               ['data/qsort/firstRun/left.csv',
                'data/qsort/firstRun/right.csv',
                'data/qsort/firstRun/middle.csv',
                'data/qsort/firstRun/median.csv',
                'data/qsort/firstRun/random.csv'],
               'pivotMethods_firstImplementation.pdf',
               ['left', 'right', 'middle', 'median', 'random'])
