import numpy as np
import matplotlib.pyplot as plt
import matplotlib.axes._axes as axes
import csv


def generate_graph(titl, xla, yla, d, pathArray, legend, clipBegin=0.0, clipEnd=0.0):
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
    # fig.show()
    # ax1.clf()
    return fig


xl = 'Elements'
yl = 'Time / ms'
desc = "Average over 20 runs, random numbers"

figure = generate_graph('', xl, yl, desc,
                        ['data/qsort/firstRun/left.csv',
                         'data/qsort/firstRun/right.csv',
                         'data/qsort/firstRun/middle.csv',
                         'data/qsort/firstRun/median.csv',
                         'data/qsort/firstRun/random.csv'],
                        ['left', 'right', 'middle', 'median', 'random'])
figure.savefig('out/' + 'pivotMethods_Implementation1.pdf', bbox_inches='tight')

figure = generate_graph('', xl, yl, desc,
                        [
                            'data/qsort/secondRun/right.csv',
                            'data/qsort/firstRun/right.csv',
                            'data/qsort/secondRun/middle.csv',
                            'data/qsort/firstRun/middle.csv'
                        ],
                        ['right 2nd Impl.', 'right 1st Impl.', 'middle 2nd', 'middle 1st'])
figure.savefig('out/' + 'pivotMethods_Implementation2a.pdf', bbox_inches='tight')

figure = generate_graph('', xl, yl, desc,
                        [
                            'data/qsort/secondRun/median.csv',
                            'data/qsort/firstRun/median.csv',
                            'data/qsort/secondRun/middle.csv',
                            'data/qsort/firstRun/middle.csv'
                        ],
                        ['median 2nd Impl.', 'median 1st Impl.', 'middle 2nd', 'middle 1st'])
figure.savefig('out/' + 'pivotMethods_Implementation2b.pdf', bbox_inches='tight')
