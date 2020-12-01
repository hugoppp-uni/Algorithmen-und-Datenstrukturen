import numpy as np
import matplotlib.pyplot as plt
import csv


def generate_graph(pathArray, filename, legend, clipBegin=0.0, clipEnd=0.0):
    xArr = []
    yArr = []
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
        plt.plot(np.array(xArr[i], int), np.array(yArr[i], int), '.')
    plt.axhline(0, color='lightgrey', lw=1)
    plt.axvline(0, color='lightgrey', lw=1)
    plt.legend(legend, )
    plt.autoscale(enable=True, axis='both')
    # plt.savefig('out/' + filename)
    plt.show()
    plt.clf()


plt.figure()
plt.title('qsort list comprh test')
plt.xlabel('Elements')
plt.ylabel('Time / ms')

generate_graph(['data/qsort/randLeft20_ListComp_1.csv', 'data/qsort/randLeft20_1.csv'],
              'plot1', ['list comprehension', 'own implementation'])
