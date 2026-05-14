import matplotlib.pyplot as plt
import numpy as np


max_b = 10
max_n = 15

#Ajouter dans l les données contenues dans time.txt à la main...
l = []

sat = [[[] for i in range(max_n-1)] for i in range(max_b)]

mean = [[0 for j in range(max_n-1)] for i in range(max_b)]
pts = [10*(i+2) for i in range(max_n -1)]

for (a, b, c, s) in l:
    cl[a-1][int(b/10) -2].append(s)

for i in range(max_b):
    for j in range(max_n -1):
        mean[i][j] = sum(cl[i][j])/len(cl[i][j])

for i in range(max_b):
    plt.plot(pts, mean[i])


# Create figure with default size
plt.xlabel("Nombre de noeuds")
plt.ylabel("Satisfaction")
plt.legend(["b = " + str(i+1) for i in range(max_b)])

# Show and save the chart
plt.savefig("output.png")
plt.show()