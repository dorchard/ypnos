benchmarks : comp_avg_100 comp_life_100 id_100

sources = Benchmark.hs
bench = runghc -i"../src" Benchmark.hs
funcs = avg life id
archs = gpu cpu
nums = 10 100

comp_life : comp_life_100 comp_life_1000
comp_avg : comp_avg_100 comp_avg_1000
id : id_100.csv id_1000.csv

comp_life_100 : life_cpu_100.csv life_gpu_100.csv
comp_life_1000 : life_cpu_1000.csv life_gpu_1000.csv

comp_avg_100 : avg_cpu_100.csv avg_gpu_100.csv
comp_avg_1000 : avg_cpu_1000.csv avg_gpu_1000.csv

id_100: id_gpu_100.csv

id_1000: id_gpu_1000.csv

define BENCHMARK_template
$(1)_$(2)_$(3)0.csv : $(sources)
	$(bench) $(1) $(2) 2 $(3) $(3)0 $(1)_$(2)_$(3)0.csv
endef

$(foreach func,$(funcs),\
$(foreach arch,$(archs),\
$(foreach num,$(nums),\
$(eval $(call BENCHMARK_template,$(func),$(arch),$(num)))\
)))
