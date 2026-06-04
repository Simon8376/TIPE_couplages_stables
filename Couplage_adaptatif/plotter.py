import matplotlib.pyplot as plt
import numpy as np


max_b = 2
max_n = 100

#Insérer les données contenues dans time.txt dans l
l = []

cl = [[[[] for i in range(max_n)] for i in range(max_b)] for i in range(2)]
sat = [[[[] for i in range(max_n)] for i in range(max_b)] for i in range(2)]
span = [[] for i in range(max_b)]


mean_t = [[[0 for j in range(max_n)] for i in range(max_b)] for i in range(2)]
mean_sat = [[[0 for j in range(max_n)] for i in range(max_b)] for i in range(2)]
mean_span = [[0 for i in range(max_b)] for i in range(max_b)]

pts_n = [10+i for i in range(max_n)]
pts_b = ["b="+str(1+i) for i in range(max_b)]

for (a, b, c, d, e, f) in l:
    if a == '0':
        span[b-1][c-20].append(f)
    else:
        ind = 0
        if a == 'R':
            ind = 1
        cl[ind][b-1][c-20].append(d)
        sat[ind][b-1][c-20].append(f)

for i in range(max_b):
    for j in range(max_n):
        mean_span[i][j] = sum(span[i][j])/len(span[i][j])
        mean_t[i][j] = sum(cl[0][i][j]+cl[1][i][j])/(len(cl[0][i][j]) + len(cl[1][i][j]))
        mean_sat[i][j] = sum(sat[0][i][j]+cl[1][i][j])/(len(sat[0][i][j]) + len(cl[1][i][j]))

fig, axs = plt.subplots(1, 3, figsize = (12, 10))
axt = axs[0]
axsat = axs[1]
axspan = axs[2]

pts_n = pts_n[0:max_n-2]

col = ['blue', 'orange']

for i in range(max_b):
    axt.plot(pts_n, mean_t[i][0:max_n-2], color=col[i])
    axsat.plot(pts_n, mean_sat[i][0:max_n-2], color=col[i])
    axspan.plot(pts_n, mean_span[i][0:max_n-2], color=col[i])


# Create figure with default size
axt.set_xlabel("Nombre de noeuds")
axsat.set_xlabel("Nombre de noeuds")
axt.set_ylabel("Temps de convergence (s)")
axsat.set_ylabel("Satisfaction")
axspan.set_ylim(0, 18)
axspan.set_ylabel("Durée d'activité d'un lien")
leg = ["b="+str(i+1) for i in range(max_b)]
axt.legend(leg)
axsat.legend(leg)


# Show and save the chart
plt.savefig("chart_time_adaptatif.png")
plt.show()