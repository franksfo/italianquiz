(ns italianverbs.sandbox
  (:require
   [italianverbs.fs :as fs]))

(def human {:human true
            :artifact false
            :edible false ;; sorry, cannibals..
            :animate true})
(def animal {:artifact false
             :animate true})
(def artifact {:artifact true
               :animate false})

(def np-1-lexicon
  (let [gender (ref :top)
        number (ref :top)
        agreement {:synsem {:gender gender
                            :number number}
                   :subcat {:a {:gender gender
                                :number number}}}]
    (list
     (fs/unify (fs/copy agreement)
               {:synsem {:cat :noun
                         :number :sing
                         :gender :masc
                         :edible false
                         :artifact true
                         :person :3rd}
                :subcat {:a {:cat :det}}
                :italian "compito"
                :english "homework"})
     (fs/unify (fs/copy agreement)
               {:synsem {:cat :noun
                         :number :sing
                         :gender :masc
                         :edible true
                         :artifact true
                         :person :3rd}
                :subcat {:a {:cat :det}}
                :italian "pane"
                :english "bread"})
     (fs/unify (fs/copy agreement)
               {:synsem {:cat :noun
                         :number :sing
                         :gender :fem
                         :edible true
                         :artifact true
                         :person :3rd}
                :subcat {:a {:cat :det}}
                :italian "pasta"
                :english "pasta"})
   
   (fs/unify (fs/copy agreement)
             (fs/copy human)
             {:synsem {:cat :noun
                       :number :sing
                       :gender :masc
                       :person :3rd}
              :subcat {:a {:cat :det}}
              :italian "ragazzo"
              :english "guy"})

   (fs/unify (fs/copy agreement)
             (fs/copy human)
             {:synsem {:cat :noun
                       :number :sing
                       :gender :fem
                       :person :3rd}
              :subcat {:a {:cat :det}}
              :italian "ragazza"
              :english "girl"})

   {:synsem {:cat :det
             :gender :masc
             :number :sing}
    :italian "il"
    :english "the"}
   {:synsem {:cat :det
             :gender :fem
             :number :sing}
    :italian "la"
    :english "the"}
   {:synsem {:cat :det
             :gender :masc
             :number :plur}
    :italian "i"
    :english "the"}
   {:synsem {:cat :det
             :gender :fem
             :number :plur}
    :italian "le"
    :english "the"})))
   

(defn find-first-in [query collection]
  "find the first member of the collection that unifies with query successfully."
  (if (= (.size collection) 0)
    nil
    (let [result (fs/unify query (first collection))]
      (if (not (fs/fail? result))
        result
        (find-first-in query (rest collection))))))

(def lexicon np-1-lexicon)

(defn lookup [query]
  (find-first-in query lexicon))

(def np-1-rules 
  (let [np-rule-1 ;; NP -> Comp Head
        (let [comp-synsem (ref {:cat :det})
              comp (ref {:synsem comp-synsem})
              head-synsem (ref {:cat :noun})
              head (ref {:synsem head-synsem
                         :subcat {:a comp-synsem}})]
          {:comment "np -> det noun"
           :head head
           :subcat :nil!
           :synsem head-synsem
           :comp comp
           :a comp
           :b head})]
    (list np-rule-1)))

(defn unify [& args]
  (apply fs/unify
         (map (fn [arg]
                (fs/copy arg))
              args)))



