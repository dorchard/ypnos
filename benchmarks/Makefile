benchmarks : comp_avg

sources = Benchmark.hs
bench = runhaskell Benchmark.hs
funcs = avg
archs = gpu cpu
nums = 10

comp_avg : comp_avg_100 comp_avg_1000

comp_avg_100 : avg_cpu_100.csv avg_gpu_100.csv
comp_avg_1000 : avg_cpu_1000.csv avg_gpu_1000.csv

define BENCHMARK_template =
$(1)_$(2)_$(3)0.csv : $(sources)
	$(bench) $(1) $(2) 2 $(3) $(3)0 $(1)_$(2)_$(3)0.csv
endef

$(foreach func,$(funcs),\
$(foreach arch,$(archs),\
$(foreach num,$(nums),\
$(eval $(call BENCHMARK_template,$(func),$(arch),$(num)))\
)))
