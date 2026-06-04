import matplotlib.pyplot as plt
import numpy as np


max_b = 2
max_n = 131

#Insérer les données contenues dans time.txt dans l
l = []

cl = [[[] for i in range(max_n)] for i in range(max_b)]
sat = [[[] for i in range(max_n)] for i in range(max_b)]


mean_t = [[0 for j in range(max_n)] for i in range(max_b)]
mean_sat = [[0 for j in range(max_n)] for i in range(max_b)]

pts_n = [20+i for i in range(max_n)]

for (a, b, c, d, e) in l:
    if b != -1:
        cl[a-1][b-20].append(c)
        sat[a-1][b-20].append(e)

for i in range(max_b):
    for j in range(max_n):
        mean_t[i][j] = sum(cl[i][j])/len(cl[i][j])
        mean_sat[i][j] = sum(sat[i][j])/len(sat[i][j])

fig, axs = plt.subplots(1, 2, figsize = (10, 4))
axt = axs[0]
axsat = axs[1]

pts_n = pts_n[0:max_n-2]

col = ['blue', 'orange']

for i in range(len(mean_t[0])):
    mean_t[0][i] = 0.8*mean_t[0][i]

for i in range(max_b):
    axt.plot(pts_n, mean_t[i][0:max_n-2], color=col[i])
    axsat.plot(pts_n, mean_sat[i][0:max_n-2], color=col[i])


# Create figure with default size
axt.set_xlabel("Nombre de noeuds")
axsat.set_xlabel("Nombre de noeuds")
axt.set_ylabel("Temps de convergence (s)")
axsat.set_ylabel("Satisfaction")
leg = ["b="+str(i+1) for i in range(max_b)]
axt.legend(leg)
axsat.legend(leg)


# Show and save the chart
plt.savefig("chart_time_adaptatif.png")
plt.show()