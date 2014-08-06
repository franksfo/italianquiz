[![Build Status](https://secure.travis-ci.org/ekoontz/italianquiz.png?branch=master)](http://travis-ci.org/ekoontz/italianquiz)

# Verbcoach

## Introduction

A web application to drill basic aspects of Italian grammar. It is
written in Clojure, a Lisp that runs on the Java Virtual Machine
(JVM). The grammatical and lexical formalism is based on <a
href="http://en.wikipedia.org/wiki/Head-driven_phrase_structure_grammar">Head
Driven Phrase Structure Grammar (HPSG)</a> as described in the book <a
href="http://cslipublications.stanford.edu/site/0226674479.shtml">Head-Driven
Phrase Structure Grammar</a> [Pollard and Sag 1994].

## Getting Started

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
    ("Il vostro uomo gentile dormiva (Your (pl) kind man was sleeping).")

## Benchmarking

    italianverbs.benchmark=> (load "italianverbs/benchmark")
    italianverbs.benchmark=> (load "benchmark")
    nil
    italianverbs.benchmark=> (standard-benchmark 3)
    ' (Tu lavori ().) ' took:  185  msec.
    ' (Il nostro uomo la scriveva ().) ' took:  1412  msec.
    ' (Quello ragazzo lavorava ().) ' took:  422  msec.
    stats for 'bolt-benchmark-it' {:trials 3, :mean 673.0, :median 422, :stddev 36.0, :min 185, :max 1412, :95% 422, :99% 422}
    ' ((Her ugly woman was losing it (&#x2640;)).) ' took:  2534  msec.
    ' ((Paola was working).) ' took:  161  msec.
    ' ((I will be less beautiful than Naples).) ' took:  3409  msec.
    stats for 'bolt-benchmark-en' {:trials 3, :mean 2034.6666666666667, :median 2534, :stddev 53.0, :min 161, :max 3409, :95% 2534, :99% 2534}
    nil
    italianverbs.benchmark=> (standard-benchmark 3)
    ' (Lui veniva ().) ' took:  83  msec.
    ' (La professoressa corta lo ricorda ().) ' took:  1367  msec.
    ' (Paola la mangerà ().) ' took:  1395  msec.
    stats for 'bolt-benchmark-it' {:trials 3, :mean 948.3333333333334, :median 1367, :stddev 35.0, :min 83, :max 1395, :95% 1367, :99% 1367}
    ' ((We support us).) ' took:  535  msec.
    ' ((His mother will think).) ' took:  384  msec.
    ' ((My red dogs sleep).) ' took:  1347  msec.
    stats for 'bolt-benchmark-en' {:trials 3, :mean 755.3333333333334, :median 535, :stddev 27.0, :min 384, :max 1347, :95% 535, :99% 535}
    nil
    italianverbs.benchmark=>

