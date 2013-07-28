#!/bin/sh
# usage:  populate.sh [count]
#  where _count_ is how many sentences to generate.
#   defaults to 100 (see src/italianverbs/populate.clj for 
#   where default is set).
lein run -m italianverbs.populate $*
