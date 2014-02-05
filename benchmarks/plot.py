# coding: utf-8
import matplotlib.mlab as mlab
import matplotlib.pyplot as plt

r = mlab.csv2rec("avg_cpu_100.csv",names=["x","y"])
plt.plot(r["x"],r["y"],label="cpu")
r = mlab.csv2rec("avg_gpu_100.csv",names=["x","y"])
plt.plot(r["x"],r["y"],label="gpu")
plt.legend()
plt.xlabel("Grid size")
plt.ylabel("Run time")
plt.title("Performance of the CPU average function over grid size.")
plt.show()
