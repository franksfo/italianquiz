#!/bin/sh
# usage:  benchmark.sh
# show only benchmark name and stats: ./benchmark.sh | egrep "\-\-"\|average\|stddev
lein run -m italianverbs.benchmark/benchmark $*
