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
    nil
    user=> (in-ns 'italianverbs.workbook)
    #<Namespace italianverbs.workbook>
    italianverbs.workbook> (fo (sentence))
    "loro l'amavano"
    italianverbs.workbook> (translate "I speak")
    "io parlo"

## Benchmarking

    user=> (load "italianverbs/benchmark")
    user=> (in-ns 'italianverbs.benchmark)
    #<Namespace italianverbs.benchmark>
    italianverbs.benchmark=> (standard-benchmark 1)
    ' tu mangerai ' took:  456  msec.
    stats for 'bolt-benchmark-it' {:trials 1, :mean 456.0, :median 456, :stddev 0.0, :min 456, :max 456, :95% 456, :99% 456}
    ' Antonio parlava ' took:  519  msec.
    stats for 'bolt-benchmark-en' {:trials 1, :mean 519.0, :median 519, :stddev 0.0, :min 519, :max 519, :95% 519, :99% 519}
    ' I speak ' took:  474  msec.
    stats for 'italian2english' {:trials 1, :mean 474.0, :median 474, :stddev 0.0, :min 474, :max 474, :95% 474, :99% 474}
    nil

