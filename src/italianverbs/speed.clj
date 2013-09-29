(ns italianverbs.rules
  (:refer-clojure :exclude [get-in resolve])
  (:use [italianverbs.generate :only (rewrite-as)]
        [italianverbs.grammar]
        [italianverbs.morphology :only (fo)]
        [italianverbs.rules]
        [italianverbs.ug :only (gen-all)])
  (:require [clojure.tools.logging :as log]
))

(defn speed-test [ & times]
  "TODO: show benchmark results and statistics (min,max,95%tile,stddev,etc)"
  (let [times (if times times 3)]
    (fo (take times (repeatedly #(time (random-sentence)))))))

(defn speed-test2 []
  (do
    (time (fo (take 1 (gen-all (shuffle np)))))
    (time (fo (take 1 (gen-all (shuffle vp)))))
    (time (fo (take 1 (gen-all (shuffle declarative-sentence)))))))

(defn speed-test3 [ & times]
  "TODO: show benchmark results and statistics (min,max,95%tile,stddev,etc)"
  (let [times (if times times 10)]
    (list
     (fo (take times (repeatedly #(time (take 1 (gen-all (shuffle np)))))))
     (fo (take times (repeatedly #(time (take 1 (gen-all (shuffle vp)))))))
     (fo (take times (repeatedly #(time (take 1 (gen-all (shuffle declarative-sentence))))))))))

