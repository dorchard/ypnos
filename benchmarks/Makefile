benchmarks : average

hsources = average.hs
bench = runhaskell -i"../dist/build" Benchmark.hs
archs = ypnos accelerate
nums = 100 1000 10000

average : $(foreach arch,$(archs),$(foreach num,$(nums),$(arch)_$(num).csv))

define BENCHMARK_template =
$(1)_$(2).csv : $(sources)
	$(bench) $(2) $(1)_$(2).csv
endef

$(foreach arch,$(archs),\
$(foreach num,$(nums),\
$(eval $(call BENCHMARK_template,$(arch),$(num)))\
))
