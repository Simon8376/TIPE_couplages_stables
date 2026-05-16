import matplotlib.pyplot as plt
import numpy as np


max_b = 2
max_n = 10

#Ajouter dans l les données contenues dans time.txt à la main...
l = []

t = [[[] for i in range(max_n-1)] for i in range(max_b)]
cyc = [[[] for i in range(max_n-1)] for i in range(max_b)]
node = [[[] for i in range(max_n-1)] for i in range(max_b)]
sat = [[[] for i in range(max_n-1)] for i in range(max_b)]

meant = [[[] for j in range(max_n-1)] for i in range(max_b)]
meancyc = [[[] for j in range(max_n-1)] for i in range(max_b)]
meannode = [[[] for j in range(max_n-1)] for i in range(max_b)]
meansat = [[[] for j in range(max_n-1)] for i in range(max_b)]

pts = [10*(i+2) for i in range(max_n -1)]

for (b, cycles, noeuds, n, time, s) in l:
    i = n//10 -2
    t[-b][i].append(time)
    cyc[b-1][i].append(cycles)
    node[b-1][i].append(noeuds)
    sat[b-1][i].append(s)

for i in range(max_b):
    for j in range(max_n -1):
        meant[i][j] = sum(t[i][j])/len(t[i][j])
        meancyc[i][j] = sum(cyc[i][j])/len(cyc[i][j])
        meannode[i][j] = sum(node[i][j])/len(node[i][j])
        meansat[i][j] = sum(sat[i][j])/len(sat[i][j])
        

fig, axs = plt.subplots(2, 2, figsize=(8, 8), layout="constrained")
axt = axs[0][0]
axn = axs[0][1]
axc = axs[1][0]
axsat = axs[1][1]

for i in range(max_b):
    axt.plot(pts, meant[i])
    axn.plot(pts, meancyc[i])
    axc.plot(pts, meannode[i])
    axsat.plot(pts, meansat[i])

# Create figure with default size
axt.set_xlabel("Nombre de noeuds")
axt.set_ylabel("Temps de convergence")
axn.set_xlabel("Nombre de noeuds")
axn.set_ylabel("Nombre de noeuds détruits")
axc.set_ylabel("Nombre de cycles détruits")
axc.set_xlabel("Nombre de noeuds")
axsat.set_xlabel("Nombre de noeuds")
axsat.set_ylabel("Stabilité")
axt.legend(["b = " + str(i+1) for i in range(max_b)])
axc.legend(["b = " + str(i+1) for i in range(max_b)])
axn.legend(["b = " + str(i+1) for i in range(max_b)])
axsat.legend(["b = " + str(i+1) for i in range(max_b)])


# Show and save the chart
plt.savefig("chart_t_n_c_sat.png")
plt.show()