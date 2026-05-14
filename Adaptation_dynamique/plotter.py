import matplotlib.pyplot as plt
import numpy as np


max_b = 3
max_n = 201

#Insérer les données contenues dans time.txt dans l
l = []

cl = [[[] for i in range(max_n)] for i in range(max_b)]
sat = [[[] for i in range(max_n)] for i in range(max_b)]

mean_t = [[0 for j in range(max_n)] for i in range(max_b)]
mean_sat = [[0 for j in range(max_n)] for i in range(max_b)]
pts = [11+i for i in range(max_n)]


for (a, b, c, d) in l:
    cl[a-1][b-11].append(c)
    sat[a-1][b-11].append(d)

for i in range(max_b):
    for j in range(max_n):
        mean_t[i][j] = sum(cl[i][j])/len(cl[i][j])
        mean_sat[i][j] = sum(sat[i][j])/len(sat[i][j])


fig, axs = plt.subplots(1, 2, figsize = (10, 4))

for i in range(max_b):
    axs[0].plot(pts, mean_t[i])

axs[1].plot(pts, mean_sat[i])
# Create figure with default size
axs[0].set_xlabel("Nombre de noeuds")
axs[1].set_xlabel("Nombre de noeuds")
axs[0].set_ylabel("Temps de convergence (s)")
axs[1].set_ylabel("Stabilité")
leg = ["b="+str(i+1) for i in range(3)]
axs[0].legend(leg)
plt.title("Protocol dynamique: ajout périodique de noeuds")

# Show and save the chart
plt.savefig("chart_time_dynamic2.png")
plt.show()