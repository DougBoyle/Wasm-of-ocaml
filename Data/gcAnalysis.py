import numpy as np
import matplotlib.pyplot as plt

mallocs = []
frees = []
memgrows = []
#with open("../wasm-of-ocaml/benchmarks/gc/mallocLog.json") as f:
with open("../wasm-of-ocaml/benchmarks/gc/mal.txt") as f:
    mallocs = eval(f.readline())
    frees = eval(f.readline())
    memgrows = eval(f.readline())
plt.plot(mallocs, label="mallocs")
plt.plot(frees, label="frees")
plt.vlines(np.where(memgrows), -300, -50, colors='k')
plt.legend()
plt.show()


# proportion of frees that do very little (known to happen in clusters)
bad_frees = len(np.where(np.array(frees) < 30))/len(frees)

#plt.hist(frees, cumulative=True, density=True, bins=range(max(frees)))
#plt.show()
