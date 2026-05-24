import matplotlib.pyplot as plt
import numpy as np


max_b = 2
max_n = 100

#Insérer les données contenues dans time.txt dans l
l = []

cl = [[[[] for i in range(max_n)] for i in range(max_b)] for i in range(2)]
mod = [[[[] for i in range(max_n)] for i in range(max_b)] for i in range(2)]
sat = [[[[] for i in range(max_n)] for i in range(max_b)] for i in range(2)]
span = [[] for i in range(max_b)]


mean_t = [[[0 for j in range(max_n)] for i in range(max_b)] for i in range(2)]
mean_mod = [[[0 for j in range(max_n)] for i in range(max_b)] for i in range(2)]
mean_sat = [[[0 for j in range(max_n)] for i in range(max_b)] for i in range(2)]
mean_span = [0 for i in range(max_b)]

pts_n = [10+i for i in range(max_n)]
pts_b = ["b="+str(1+i) for i in range(max_b)]

for (a, b, c, d, e, f) in l:
    if a == '0':
        span[a-1].append(f)
    else:
        ind = 0
        if a == 'R':
            ind = 1
        cl[ind][b-1][c-11].append(d)
        mod[ind][b-1][c-11].append(e)
        sat[ind][b-1][c-11].append(f)

for i in range(max_b):
    mean_span[i] = sum(span[i])/len(span[i])
    for ind in range(2):
        for j in range(max_n):
            mean_t[e][i][j] = sum(cl[i][j])/len(cl[i][j])
            mean_mod[e][i][j] = sum(mod[i][j])/len(mod[i][j])
            mean_sat[e][i][j] = sum(sat[i][j])/len(sat[i][j])

fig, axs = plt.subplots(2, 2, figsize = (10, 10))
axt = axs[0][0]
axm = axs[0][1]
axsat = axs[1][0]
axspan = axs[1][1]

pts_n = pts_n[0:max_n-2]

col = ['blue', 'orange']
style = ['--', '---']

for ind in range(2):
    for i in range(max_b):
        axt.plot(pts_n, mean_t[i][0:max_n-2], style[ind], color=col[i])
        axm.plot(pts_n, mean_mod[i][0:max_n-2], style[ind], color=col[i])
        axsat.plot(pts_n, mean_sat[i][0:max_n-2], style[ind], color=col[i])

axspan.bar(pts_b, mean_span)

# Create figure with default size
axt.set_xlabel("Nombre de noeuds")
axsat.set_xlabel("Nombre de noeuds")
axm.set_xlabel("Nombre de noeuds")
axt.set_ylabel("Temps de convergence (s)")
axm.set_ylabel("Nombre de noeuds modifiés")
axsat.set_ylabel("Satisfaction")
axspan.set_ylim(0, 10)
axspan.set_ylabel("Nombre de tours moyen d'activité d'un lien")
leg = ["b="+str(i+1) for i in range(max_b)]
axt.legend(leg)
axm.legend(leg)
axsat.legend(leg)


# Show and save the chart
plt.savefig("chart_time_adaptatif.png")
plt.show()