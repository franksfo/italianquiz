[![Build Status](https://secure.travis-ci.org/ekoontz/italianquiz.png?branch=master)](http://travis-ci.org/ekoontz/italianquiz)

# Italianquiz

## Introduction

A web application to drill basic aspects of Italian grammar. It is
written in Clojure, a Lisp that runs on the Java Virtual Machine
(JVM). The grammatical and lexical formalism is based on <a
href="http://en.wikipedia.org/wiki/Head-driven_phrase_structure_grammar">Head
Driven Phrase Structure Grammar (HPSG)</a> as described in the book <a
href="http://cslipublications.stanford.edu/site/0226674479.shtml">Head-Driven
Phrase Structure Grammar</a> [Pollard and Sag 1994].

## Getting Started
    eugenekontzspro:italianquiz ekoontz$ brew install leiningen
    eugenekontzspro:italianquiz ekoontz$ lein repl
    nREPL server started on port 57573 on host 127.0.0.1
    REPL-y 0.3.0
    Clojure 1.6.0
        Docs: (doc function-name-here)
              (find-doc "part-of-name-here")
      Source: (source function-name-here)
     Javadoc: (javadoc java-object-or-class-here)
        Exit: Control+D or (exit) or (quit)
     Results: Stored in vars *1, *2, *3, an exception in *e
    
    user=> (load "italianverbs/workbook")
    INFO  Th: 33:19,007 italianverbs.ug: Universal Grammar Immediate Dominance schemata are defined in our environment.
    INFO  Th: 33:19,162 italianverbs.grammar.english: English grammar loaded.
    INFO  Th: 33:30,753 italianverbs.grammar.english: Built grammatical and lexical cache in  11589  msec.
    INFO  Th: 33:30,860 italianverbs.grammar.italiano: Italian grammar defined.
    INFO  Th: 33:31,023 italianverbs.grammar.italiano: Built grammatical and lexical cache in  162  msec.
    nil
    user=> (in-ns 'italianverbs.workbook)
    #<Namespace italianverbs.workbook>
    italianverbs.workbook=> (fo (sentence)
    "en:(\"(Our woman will think).\") it:(\"La nostra donna penserà ().\")"

## Benchmarking

    user=> (load "italianverbs/benchmark")
    user=> (in-ns 'italianverbs.benchmark)
    #<Namespace italianverbs.benchmark>
    italianverbs.benchmark=> (standard-benchmark 1)
    ' (Lui andava ().) ' took:  328  msec.
    stats for 'bolt-benchmark-it' {:trials 1, :mean 328.0, :median 328, :stddev 0.0, :min 328, :max 328, :95% 328, :99% 328}
    ' ((Your happy mothers will look for you).) ' took:  819  msec.
    stats for 'bolt-benchmark-en' {:trials 1, :mean 819.0, :median 819, :stddev 0.0, :min 819, :max 819, :95% 819, :99% 819}
    ' (Voi perderete Roma (You all will lose Rome).) ' took:  3986  msec.
    stats for 'italian2english' {:trials 1, :mean 3986.0, :median 3986, :stddev 0.0, :min 3986, :max 3986, :95% 3986, :99% 3986}
    nil
