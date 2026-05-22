import matplotlib.pyplot as plt
import numpy as np


max_b = 2
max_n = 200

#Insérer les données contenues dans time.txt dans l
l = []

cl = [[[] for i in range(max_n)] for i in range(max_b)]
mod = [[[] for i in range(max_n)] for i in range(max_b)]
sat = [[[] for i in range(max_n)] for i in range(max_b)]

mean_t = [[0 for j in range(max_n)] for i in range(max_b)]
mean_mod = [[0 for j in range(max_n)] for i in range(max_b)]
mean_sat = [[0 for j in range(max_n)] for i in range(max_b)]

pts = [10+i for i in range(max_n)]


for (a, b, c, d, e) in l:
    cl[a-1][b-11].append(c)
    mod[a-1][b-11].append(d)
    sat[a-1][b-11].append(e)

for i in range(max_b):
    for j in range(max_n):
        mean_t[i][j] = sum(cl[i][j])/len(cl[i][j])
        mean_mod[i][j] = sum(mod[i][j])/len(mod[i][j])
        mean_sat[i][j] = sum(sat[i][j])/len(sat[i][j])

plt.title("Protocol dynamique adaptatif")
fig, axs = plt.subplots(2, 2, figsize = (10, 4))
axt = axs[0][0]
axm = axs[0][1]
axsat = axs[1][0]

for i in range(max_b):
    axt.plot(pts, mean_t[i])
    axm.plot(pts, mean_mod[i])
    axsat.plot(pts, mean_sat[i])

# Create figure with default size
axt.set_xlabel("Nombre de noeuds")
axsat.set_xlabel("Nombre de noeuds")
axm.set_xlabel("Nombre de noeuds")
axt.set_ylabel("Temps de convergence (s)")
axm.set_ylabel("Nombre de noeuds modifiés")
axsat.set_ylabel("Satisfaction")
leg = ["b="+str(i+1) for i in range(max_b)]
axt.legend(leg)
axm.legend(leg)
axsat.legend(leg)


# Show and save the chart
plt.savefig("chart_time_adaptatif.png")
plt.show()