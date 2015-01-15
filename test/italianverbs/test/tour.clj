(ns italianverbs.test.tour
  (:require
   [clojure.test :refer :all]
   [italianverbs.engine :refer [generate]]
   [italianverbs.italiano :as it]
   [italianverbs.morphology :refer [fo]]
   [italianverbs.tour :refer :all]
   [italianverbs.unify :refer (fail? strip-refs unify)]
   [korma.core :as k]))

;; TODO: need a shim to return fixtures (simulated database results)
(deftest tour-generate
  (is (not (= "" "foo"))))

(def killer-spec 
  {:comp {:phrasal false} 
   :head {:phrasal :top} 
   :synsem {:sem {:tense :past 
                  :aspect :perfect 
                  :pred :parlare 
                  :subj {:mass false 
                         :furniture false 
                         :pred :loro 
                         :place false 
                         :drinkable false 
                         :human true 
                         :animate true 
                         :speakable false 
                         :activity false 
                         :physical-object true 
                         :buyable false 
                         :legible false 
                         :artifact false 
                         :gender :masc 
                         :edible false 
                         :part-of-human-body false} 
                  :obj :unspec} 
            :subcat []
            :cat :verb}})

(defn against-pred [spec]
  (let [pred-of-spec (get-in spec [:synsem :sem :pred] :top)]
    (if (= pred-of-spec :top)
      spec
      (filter #(not (fail? %))
              (mapcat (fn [lexemes]
                        (map (fn [lexeme]
                               (unify (if (= pred-of-spec
                                             (get-in lexeme [:synsem :sem :pred] :top))
                                        :top :fail) ;; short-circuit if not the right pred.
                                      spec
                                      {:synsem {:sem (strip-refs (get-in lexeme [:synsem :sem] :top))}}
                                      {:synsem {:essere (strip-refs (get-in lexeme [:synsem :essere] :top))}}))
                             lexemes))
                      (vals @it/lexicon))))))


(defn screen-against-pred-comp [spec]
  (let [pred-of-comp (get-in spec [:synsem :sem :subj :pred] :top)]
    (if (= pred-of-comp :top)
      spec
      (mapcat (fn [lexemes]
                (map (fn [lexeme]
                       (if (= pred-of-comp
                              (get-in lexeme [:synsem :sem :pred] :top))
                         (list lexeme)))
                     lexemes))
              (vals @it/lexicon)))))

(defn against-comp [spec]
  (let [pred-of-comp (get-in spec [:synsem :sem :subj :pred] :top)]
    (if (= pred-of-comp :top)
      spec
      (let [matching-lexemes (flatten (remove nil? (screen-against-pred-comp spec)))]
        (map (fn [lexeme]
               (unify spec
                      (if true :top {:comp {:synsem {:agr (strip-refs (get-in lexeme [:synsem :agr] :top))}}})
                      {:comp {:synsem {:agr {:person :3rd
                                             :number :plur}}}}
                      ))
             matching-lexemes)))))

(defn enrich [spec]
  (let [pred (get-in spec [:synsem :sem :pred] nil)]
    (mapcat against-comp
            (against-pred spec))))




