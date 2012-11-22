(ns italianverbs.sandbox
  (:require
   [italianverbs.fs :as fs]
   [clojure.string :as string]))

(defn unify [& args]
  "like fs/unify, but fs/copy each argument before unifying."
  (apply fs/unify
         (map (fn [arg]
                (fs/copy arg))
              args)))

(defn find-first-in [query collection]
  "find the first member of the collection that unifies with query successfully."
  (if (= (.size collection) 0)
    nil
    (let [result (fs/unify query (first collection))]
      (if (not (fs/fail? result))
        result
        (find-first-in query (rest collection))))))

(defn find [query collection]
  "find all members of the collection that unifies with query successfully."
  (if (= (.size collection) 0)
    nil
    (let [result (unify query (first collection))]
      (if (not (fs/fail? result))
        (cons result (find query (rest collection)))
        (find query (rest collection))))))

(def human {:human true
            :artifact false
            :edible false ;; sorry, cannibals..
            :animate true})
(def animal {:artifact false
             :animate true})
(def artifact {:artifact true
               :animate false})

(def edible {:edible true
             :human false}) ;; sorry again, cannibals..

(def infinitive-verb
  {:synsem {:cat :verb
            :infl :infinitive}})

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
               {:synsem {:cat :noun
                         :number :sing
                         :gender :fem
                         :edible false
                         :artifact true
                         :person :3rd}
                :subcat {:a {:cat :det}}
                :italian "scala"
                :english "ladder"})

     
     (fs/unify (fs/copy agreement)
               {:synsem {:cat :noun
                         :number :sing
                         :gender :masc
                         :person :3rd
                         }}
               {:synsem (fs/copy human)}
               {:subcat {:a {:cat :det}}
                :italian "ragazzo"
                :english "guy"})

     (fs/unify (fs/copy agreement)
               {:synsem {:cat :noun
                         :number :sing
                         :gender :fem
                         :person :3rd}}
               {:synsem (fs/copy human)
                :subcat {:a {:cat :det}}
                :italian "ragazza"
                :english "girl"})

     (fs/unify (fs/copy agreement)
               {:synsem {:cat :noun
                         :human false
                         :number :sing
                         :gender :masc
                         :person :3rd}}
               {:synsem {:legible true}
                :subcat {:a {:cat :det}}
                :italian "libro"
                :english "book"})

     
     (fs/unify (fs/copy agreement)
               {:synsem {:cat :noun
                         :human false
                         :number :sing
                         :gender :masc
                         :person :3rd}}
               {:synsem (fs/copy animal)
                :subcat {:a {:cat :det}}
                :italian "cane"
                :english "dog"})
     
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

(def trans-finitizer
  (unify (let [obj (ref :top)
               subj (ref :top)
               italian-infinitive (ref :top)
               cat (ref :top)]
           {:root
            {:italian italian-infinitive
             :subcat {:a obj
                      :b subj}
             :synsem {:cat cat}}
            :subcat {:a obj
                     :b subj }
            :synsem {:subj subj
                     :obj obj
                     :cat cat
                     :infl :present}
            :italian {:agr subj
                       :root italian-infinitive}})
         (let [subj (ref :top)]
           {:italian
            {:agr subj}})))

(def intrans-finitizer
  (unify (let [obj (ref :top)
               subj (ref :top)
               italian-infinitive (ref :top)
               cat (ref :top)]
           {:root
            {:italian italian-infinitive
             :subcat {:a subj}
             :synsem {:cat cat}}
            :subcat {:a subj }
            :synsem {:subj subj
                     :cat cat
                     :infl :present}
            :italian {:agr subj
                      :root italian-infinitive}})
         (let [subj (ref :top)]
           {:italian
            {:agr subj}})))

(def vp-1-lexicon
  (let [verb-with-root
        (let [cat (ref :verb)
              subcat (ref :top)]
          {:synsem {:cat cat}
           :subcat subcat
           :root {:subcat subcat
                  :synsem {:cat cat}}})
        transitive
        (let [subj (ref :top)
              obj (ref :top)]
          {:synsem {:subj subj
                    :obj obj}
           :subcat {:a obj
                    :b subj}})

        intransitive
        (let [subj (ref :top)]
          {:synsem {:subj subj}
           :subcat {:a subj}})

        finite-transitive
        (let [subj (ref :top)
              obj (ref :top)]
          (fs/unify
           (fs/copy verb-with-root)
           {:synsem {:infl :present}
            :root {:synsem {:subj subj
                            :obj obj}}}))
        
        regular-verb-inflection
        (let [agreement {:person :top
                         :number :top}]
          {:italian {:morph agreement}
           :subcat {:b agreement}
           :root {:synsem {:cat :verb}}})

        fare
        (let [subj {:artifact false
                    :human true}
              obj {:human false
                   :artifact true}]
          (fs/unify
           (fs/copy transitive)
           {:italian "fare"
            :english "to do"
            :synsem {:cat :verb
                     :morph :irreg
                     :subj subj
                     :obj obj
                     :infl :infinitive}
            :subcat {:a obj
                     :b subj}}))
        dormire
        (fs/unify
         (fs/copy intransitive)
         (fs/copy infinitive-verb)
         {:italian "dormire"
          :english "to sleep"
          :synsem {:subj {:animate true}}})

        mangiare
        (fs/unify
         (fs/copy transitive)
         (fs/copy infinitive-verb)
         {:italian "mangiare"
          :english "to eat"
          :synsem {:subj {:animate true}
                   :obj edible}})
        
        leggere
        (fs/unify
         (fs/copy transitive)
         (fs/copy infinitive-verb)
         {:italian "leggere"
          :english "to read"
          :synsem {:subj {:human true}
                   :obj {:legible true}}})


        ]
    (concat
     (list

      dormire
      (unify {:root dormire}
             intrans-finitizer)

      leggere
      (unify {:root leggere}
             trans-finitizer)

      mangiare
      (unify {:root mangiare}
             trans-finitizer)

      fare
      ;; irregular forms of fare.
      (fs/unify
       (fs/copy finite-transitive)
       (fs/copy transitive)
       {:root (fs/copy fare)
        :italian "facio"
        :subcat {:b {:person :1st
                     :number :sing}}})
      (fs/unify
       (fs/copy finite-transitive)
       (fs/copy transitive)
       {:root (fs/copy fare)
        :italian "fai"
        :subcat {:b {:person :2nd
                     :number :sing}}})
      (fs/unify
       (fs/copy finite-transitive)
       (fs/copy transitive)
       {:root (fs/copy fare)
        :italian "fa"
        :subcat {:b {:person :3rd
                     :number :sing}}})
      (fs/unify
       (fs/copy finite-transitive)
       (fs/copy transitive)
       {:root (fs/copy fare)
        :italian "facciamo"
        :subcat {:b {:person :1st
                     :number :plur}}})
      (fs/unify
       (fs/copy finite-transitive)
       (fs/copy transitive)
       {:root (fs/copy fare)
        :italian "fate"
        :subcat {:b {:person :2nd
                     :number :plur}}})
      (fs/unify
       (fs/copy finite-transitive)
       (fs/copy transitive)
       {:root (fs/copy fare)
        :italian "fanno"
        :subcat {:b {:person :3rd
                     :number :plur}}})))))

(def vp-1-rules
  (list
   (let [vp-rule-1
         (let [comp-synsem (ref {:cat :noun :case :acc})
               comp (ref {:synsem comp-synsem :subcat :nil!})
               subj (ref {:cat :noun :case :nom})
               head-synsem (ref {:cat :verb
                                 :infl :present
                                 :subj subj
                                 :obj comp-synsem})
               head (ref {:synsem head-synsem
                          :subcat {:a comp-synsem
                                   :b subj}})]
           {:comment "vp -> head comp"
            :head head
            :subcat {:a subj}
            :synsem head-synsem
            :comp comp
            :a head
            :b comp})]
     vp-rule-1)))

(def pronouns
  (list {:synsem {:cat :noun
                  :case :nom
                  :human true
                  :artifact false ;; <- :human true => artifact :false
                  :person :1st
                  :number :sing}
         :subcat :nil!
         :italian "io"}
        {:synsem {:cat :noun
                  :case :nom
                  :human true
                  :artifact false ;; <- :human true => artifact :false
                  :person :2nd
                  :number :sing}
         :subcat :nil!
         :italian "tu"}
        {:synsem {:cat :noun
                  :case :nom
                  :human true
                  :artifact false ;; <- :human true => artifact :false
                  :person :3rd
                  :gender :masc
                  :number :sing}
         :subcat :nil!
         :italian "lui"}
        {:synsem {:cat :noun
                  :case :nom
                  :human true
                  :artifact false ;; <- :human true => artifact :false
                  :person :3rd
                  :gender :fem
                  :number :sing}
         :subcat :nil!
         :english "she"
         :italian "lei"}
        {:synsem {:cat :noun
                  :case :nom
                  :human true
                  :artifact false ;; <- :human true => artifact :false
                  :person :1st
                  :number :plur}
         :subcat :nil!
         :english "we"
         :italian "noi"}
        {:synsem {:cat :noun
                  :case :nom
                  :human true
                  :artifact false ;; <- :human true => artifact :false
                  :person :2nd
                  :number :plur}
         :subcat :nil!
         :italian "voi"}
        {:synsem {:cat :noun
                  :case :nom
                  :human true
                  :artifact false ;; <- :human true => artifact :false
                  :person :3rd
                  :number :plur}
         :subcat :nil!
         :italian "loro"}))

(def sentence-rules
  (let [subcatted (ref {:cat :noun})
        head-synsem (ref {:cat :verb
                          :infl {:not :infinitive}
                          :subj subcatted
                          })
        comp (ref {:synsem subcatted :subcat :nil!})
        head (ref {:synsem head-synsem
                   :subcat {:a subcatted
                            :b :nil!
                            }})]
    (list
     {:comment "s -> np vp"
      :subcat :nil!
      :head head
      :comp comp
      :a comp
      :b head
      :synsem head-synsem
      })))

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

(def lexicon (concat vp-1-lexicon np-1-lexicon pronouns))

(defn lookup [query]
  (find-first-in query lexicon))

(defn lookupa [query]
  (find query lexicon))

(defn it [italian]
  (lookup {:italian italian}))

(def rules (concat np-1-rules vp-1-rules sentence-rules))

(def np (nth rules 0))
(def vp (nth rules 1))
(def s (nth rules 2))

(defn conjugate [arg]
  (cond (nil? arg) ""
        (= (type arg) java.lang.String)
        arg
        (and (= (type arg)
                clojure.lang.PersistentArrayMap)
             (contains? (set (keys arg)) :1)
             (contains? (set (keys arg)) :2))
        (let [result1 (conjugate (:1 arg))
              result2 (conjugate (:2 arg))]
          (if (and (= (type result1) java.lang.String)
                   (= (type result2) java.lang.String))
            (string/join " " (list result1 result2))
            {:1 result1
             :2 result2}))
        :else
        ;; assume a map with keys (:root and :arg).
        (let [root (fs/get-in arg '(:root))
              person (fs/get-in arg '(:agr :person))
              number (fs/get-in arg '(:agr :number))
              stem (string/replace root #"[iae]re$" "")]
          (cond
           (and (= person :1st) (= number :sing))
           (str stem "o")
           (and (= person :2nd) (= number :sing))
           (str stem "i")
           (and (= person :3rd) (= number :sing))
           (str stem "a")
           (and (= person :1st) (= number :plur))
           (str stem "amo")
           (and (= person :2nd) (= number :plur))
           (str stem "ate")
           (and (= person :3rd) (= number :plur))
           (str stem "anno")
           :else arg))))

(defn get-italian [a b]
  (let [conjugated-a (conjugate a)
        conjugated-b (conjugate b)]
    (if (and
         (= (type conjugated-a) java.lang.String)
         (= (type conjugated-b) java.lang.String))
      (str conjugated-a " " conjugated-b)
      {:1 conjugated-a
       :2 conjugated-b})))

(defn under [parent child]
  (let [child (if (= (type child) java.lang.String)
                (it child)
                child)
        as (if (nil?
                (fs/get-in parent '(:a :italian)))
             :a
             :b)

        unified (unify parent
                       {as child})

        italian
        (get-italian
         (fs/get-in unified '(:a :italian))
         (fs/get-in unified '(:b :italian)))
        ]
    (merge ;; use merge so that we overwrite the value for :italian.
     unified
     {:italian italian})))

(defn en [english]
  (lookup {:english english}))

(defn finitize [infinitive]
  (unify {:root infinitive}
         trans-finitizer))

(def mangiare-finite
  (unify {:root (it "mangiare")}
         trans-finitizer))

(defn get-in [map path]
  (fs/get-in map path))

(def regular-sentence
  (do
    (def ilragazzo (under (under np "il") "ragazzo"))
    (def lapasta (under (under np "la") "pasta"))
    (def mfvp (under (under vp mangiare-finite) lapasta))
    (def sentence (under (under s ilragazzo) mfvp))
    sentence))
       
(defn over [x y]
  (if (or (= (type x) clojure.lang.LazySeq)
          (= (type x) clojure.lang.PersistentList))
    (mapcat (fn [each-x]
              (over each-x y))
            x)
    (if (or (= (type y) clojure.lang.LazySeq)
            (= (type y) clojure.lang.PersistentList))
      (remove fs/fail?
              (map (fn [each-y]
                     (under x each-y))
                   y))
      (let [result (under x y)]
        (if (not (fs/fail? result))
          (list result))))))
