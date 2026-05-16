import matplotlib.pyplot as plt
import numpy as np


max_b = 10
max_n = 15

#Ajouter dans l les données contenues dans time.txt à la main...
l = []

cl = [[[] for i in range(max_n-1)] for i in range(max_b)]
sat = [[[] for i in range(max_n-1)] for i in range(max_b)]

mean_t = [[0 for j in range(max_n-1)] for i in range(max_b)]
mean_s = [[0 for j in range(max_n-1)] for i in range(max_b)]
pts = [10*(i+2) for i in range(max_n -1)]

for (a, b, c, d) in l:
    cl[a-1][int(b/10) -2].append(c)
    sat[a-1][int(b/10) -2].append(d)

for i in range(max_b):
    for j in range(max_n -1):
        mean_t[i][j] = sum(cl[i][j])/len(cl[i][j])
        mean_s[i][j] = sum(cl[i][j])/len(cl[i][j])


fig, axs = plt.subplots(1, 2, figsize = (8, 4))


for i in range(max_b):
    axs[0].plot(pts, mean_t[i])
    axs[1].plot(pts, mean_sat[i])



# Create figure with default size
axs[0].set_xlabel("Nombre de noeuds")
axs[0].set_ylabel("Temps de convergence")
axs[0].set_legend(["b = " + str(i+1) for i in range(max_b)])
axs[1].set_xlabel("Nombre de noeuds")
axs[1].set_ylabel("Satisfaction")
axs[1].set_legend(["b = " + str(i+1) for i in range(max_b)])

# Show and save the chart
plt.savefig("chart_time_sat.png")
plt.show()