(ns italianverbs.sandbox
  [:use [clojure.core :exclude [find]]]
  [:require
   [italianverbs.fs :as fs]
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.tools.logging :as log]])

(defn unify [& args]
  "like fs/unify, but fs/copy each argument before unifying."
  (apply fs/unify
         (map (fn [arg]
                (fs/copy arg))
              args)))

(def human {:human true
            :animate true})
(def animal {:animate true})

(def infinitive-verb
  {:synsem {:cat :verb
            :infl :infinitive}})

(def np-1-lexicon
  (let [gender (ref :top)
        number (ref :top)
        agreement {:synsem {:gender gender
                            :number number}
                   :subcat {:1 {:gender gender
                                :number number}}}
        common-noun {:synsem {:cat :noun
                              :person :3rd
                              :case :top}}]
    ;; common nouns are neither nominative or accusative. setting their case to :top allows them to (fs/match) with
    ;; verbs' case specifications like {:case {:not :acc}} or {:case {:not :nom}}.
    (list
     (unify agreement
               common-noun
               {:synsem {:sem {:pred :compito
                               :artifact true}
                         :number :sing
                         :gender :masc}
                :subcat {:1 {:cat :det}}
                :italian "compito"
                :english "homework"})

     (unify agreement
               common-noun
               {:synsem {:number :sing
                         :gender :masc
                         :sem {:pred :pane
                               :edible true
                               :artifact true}}
                :subcat {:1 {:cat :det}}
                :italian "pane"
                :english "bread"})

     (unify agreement
            common-noun
            {:synsem {:number :sing
                      :gender :fem
                      :sem {:pred :pasta
                            :edible true
                            :artifact true}}
             :subcat {:1 {:cat :det}}
                :italian "pasta"
                :english "pasta"})

     (unify agreement
            common-noun
            {:synsem {:number :sing
                      :gender :fem
                      :sem {:artifact true
                            :pred :scala}}
             :subcat {:1 {:cat :det}}
             :italian "scala"
             :english "ladder"})
     
     (unify agreement
            common-noun
            {:synsem {:number :sing
                      :gender :masc}}
            {:synsem {:sem human}}
            {:synsem {:sem {:pred :ragazzo}}}
            {:subcat {:1 {:cat :det}}
             :italian "ragazzo"
             :english "guy"})

     (unify agreement
            common-noun
            {:synsem {:number :sing
                      :gender :masc}}
            {:synsem {:sem human}}
            {:synsem {:sem {:pred :dottore}}}
            {:subcat {:1 {:cat :det}}
             :italian "dottore"
             :english "doctor"})

     (unify agreement
            common-noun
            {:synsem {:number :sing
                      :gender :fem}}
            {:synsem {:sem human}}
            {:synsem {:sem {:pred :professoressa}}}
            {:subcat {:1 {:cat :det}}
             :italian "professoressa"
             :english "professor"})

     (unify agreement
            common-noun
            {:synsem {:number :sing
                      :gender :fem}}
            {:synsem {:sem human}}
            {:synsem {:sem {:pred :ragazza}}}
            {:subcat {:1 {:cat :det}}
                :italian "ragazza"
             :english "girl"})

     (unify agreement
            common-noun
            {:synsem {:cat :noun
                      :number :sing
                      :gender :masc
                      :sem {:pred :libro
                            :legible true
                            :artifact true}
                      :person :3rd}}
            {:subcat {:1 {:cat :det}}
             :italian "libro"
             :english "book"})

     
     (unify agreement
               common-noun
               {:synsem {:number :sing
                         :gender :masc}}
               {:synsem {:sem (unify animal {:pred :cane})}
                :subcat {:1 {:cat :det}}
                :italian "cane"
                :english "dog"})

     (unify agreement
            common-noun
            {:synsem {:number :sing
                      :gender :masc
                      :person :3rd}}
            {:synsem {:sem (unify animal {:pred :gatto})}
             :subcat {:1 {:cat :det}}
             :italian "gatto"
             :english "cat"})
     
     {:synsem {:cat :det
               :def :def
               :gender :masc
               :number :sing}
      :italian "il"
      :english "the"}
     {:synsem {:cat :det
               :def :indef
               :gender :masc
               :number :sing}
      :italian "un"
      :english "a"}

     {:synsem {:cat :det
               :def :def
               :gender :fem
               :number :sing}
      :italian "la"
      :english "the"}
     {:synsem {:cat :det
               :def :def
               :gender :fem
               :number :sing}
      :italian "una"
      :english "a"}

     {:synsem {:cat :det
               :def :def
               :gender :masc
               :number :plur}
      :italian "i"
      :english "the"}
     {:synsem {:cat :det
               :def :def
               :gender :fem
               :number :plur}
      :italian "le"
      :english "the"}

     )))

(def finitizer
  (let [subj-sem (ref :top)
        root-sem (ref {:subj subj-sem})
        subj (ref {:sem subj-sem})
        subcat (ref {:1 subj})
        cat (ref :verb)
        italian-infinitive (ref :top)]
     {:root
      {:italian italian-infinitive
       :synsem {:cat cat
                :sem root-sem}
       :subcat subcat}
      :subcat subcat
      :synsem {:sem root-sem
               :cat cat
               :infl :present}
      :italian {:agr subj
                :infinitive italian-infinitive}}))

(def trans-finitizer
  (unify finitizer
         (let [obj-sem (ref :top)
               obj (ref {:sem obj-sem})]
           {:root
            {:subcat {:2 obj}}
            :synsem {:sem {:obj obj-sem}}})))

(def intrans-finitizer
  (unify finitizer
         {:root
          {:subcat {:2 :nil!}}}))

(def verb-with-root
  (let [cat (ref :verb)
        subcat (ref :top)]
    {:synsem {:cat cat}
     :subcat subcat
     :root {:subcat subcat
            :synsem {:cat cat}}}))

;; "x-itive": a generalization of intransitive and transitive (they both have a subject)
(def x-itive
  (let [subj-sem (ref :top)
        subj (ref {:sem subj-sem
                   :cat :noun
                   :case {:not :acc}})]
    {:synsem {:sem {:subj subj-sem}}
     :subcat {:1 subj}}))

(def intransitive
  (unify x-itive
         {:subcat {:2 :nil!}}))

(def transitive
  (unify x-itive
         (let [obj-sem (ref :top)
               obj (ref {:sem obj-sem
                         :cat :noun
                         :case {:not :nom}})]
           {:synsem {:obj obj-sem}
            :subcat {:2 obj}})))

(def fare
  (unify
   transitive
   {:italian {:infinitive "fare"
              :irregular {:present {:1sing "facio"
                                    :2sing "fai"
                                    :3sing "fa"
                                    :1plur "facciamo"
                                    :2plur "fate"
                                    :3plur "fanno"}}}
    :english {:infinitive "to do"
              :irregular {:present {:1sing "do"
                                    :2sing "do"
                                    :3sing "does"
                                    :1plur "do"
                                    :2plur "do"
                                    :3plur "do"}}}
    
    :synsem {:cat :verb
             :morph :irreg
             :infl :infinitive
             :sem {:pred :fare
                   :subj {:human true}
                   :obj {:artifact true}}}}))

(def dormire
  (unify
   intransitive
   infinitive-verb
   {:italian "dormire"
    :english "to sleep"
    :synsem {:sem {:subj {:animate true}
                   :pred :dormire}}}))

(def mangiare
  (unify
   transitive
   infinitive-verb
   {:italian "mangiare"
    :english "to eat"
    :synsem {:sem {:pred :mangiare
                   :subj {:animate true}
                   :obj {:edible true}}}}))


(def leggere
  (unify
   transitive
   infinitive-verb
   {:italian "leggere"
    :english "to read"
    :synsem {:sem {:pred :leggere
                   :subj {:human true}
                   :obj {:legible true}}}}))

(def vedere
  (unify
   transitive
   infinitive-verb
   {:italian "vedere"
    :english "to see"
    :synsem {:sem {:pred :vedere
                   :subj {:animate true}}}}))

(def scrivere
  (unify
   transitive
   infinitive-verb
   {:italian "scrivere"
    :english "to write"
    :synsem {:sem {:pred :scrivere
                   :subj {:human true}
                   :obj {:legible true}}}}))

(def vp-1-lexicon
  (concat
   (list
    dormire
    (unify {:root dormire}
           intrans-finitizer)
    fare
    (unify {:root fare}
           trans-finitizer)
    leggere
    (unify {:root leggere}
           trans-finitizer)
    mangiare
    (unify {:root mangiare}
           trans-finitizer)
    
    scrivere
    (unify {:root scrivere}
           trans-finitizer)

    vedere
    (unify {:root vedere}
           trans-finitizer))))

;;    [1]
;;   /   \
;;  /     \
;; H[1]    C
(def head-principle
  (let [head-synsem (ref :top)]
    {:synsem head-synsem
     :head {:synsem head-synsem}}))

;;     subcat<>
;;     /      \
;;    /        \
;; H subcat<1>  C[1]
(def subcat-1-principle
  (let [comp-synsem (ref :top)]
    {:subcat :nil!
     :head {:subcat {:1 comp-synsem}}
     :comp {:synsem comp-synsem}}))

(def vp-1-rules
  (list
   (let [vp-rule-1
         (let [obj-sem (ref :top)
               obj-synsem (ref {:sem obj-sem})
               obj (ref {:synsem obj-synsem :subcat :nil!})
               subj-sem (ref :top)
               subj-synsem (ref {:sem subj-sem})
               head-synsem (ref {:cat :verb
                                 :infl {:not :infinitive}
                                 :sem {:subj subj-sem
                                       :obj obj-sem}})
               head (ref {:synsem head-synsem
                          :subcat {:1 subj-synsem
                                   :2 obj-synsem}})]
           (unify head-principle
                  {:comment "vp -> head comp"
                   :head head
                   :subcat {:1 subj-synsem}
                   :comp obj
                   :1 head
                   :2 obj}))]
     vp-rule-1)))

(def pronouns
  (list {:synsem {:cat :noun
                  :case :nom
                  :sem (unify human {:pred :io})
                  :person :1st
                  :number :sing}
         :subcat :nil!
         :italian "io"}
        {:synsem {:cat :noun
                  :case :nom
                  :sem (unify human {:pred :tu})
                  :person :2nd
                  :number :sing}
         :subcat :nil!
         :italian "tu"}
        {:synsem {:cat :noun
                  :case :nom
                  :sem (unify human {:pred :lui})
                  :person :3rd
                  :gender :masc
                  :number :sing}
         :subcat :nil!
         :italian "lui"}
        {:synsem {:cat :noun
                  :case :nom
                  :sem (unify human {:pred :lei})
                  :person :3rd
                  :gender :fem
                  :number :sing}
         :subcat :nil!
         :english "she"
         :italian "lei"}
        {:synsem {:cat :noun
                  :case :nom
                  :sem (unify human {:pred :noi})
                  :person :1st
                  :number :plur}
         :subcat :nil!
         :english "we"
         :italian "noi"}
        {:synsem {:cat :noun
                  :case :nom
                  :sem (unify human {:pred :voi})
                  :person :2nd
                  :number :plur}
         :subcat :nil!
         :italian "voi"}
        {:synsem {:cat :noun
                  :case :nom
                  :sem (unify human {:pred :loro})
                  :person :3rd
                  :number :plur}
         :subcat :nil!
         :italian "loro"}))
    
(def sentence-rules
  (let [subj-sem (ref :top)
        subcatted (ref {:cat :noun
                        :sem subj-sem})
        head-synsem (ref {:cat :verb
                          :infl {:not :infinitive}
                          :sem {:subj subj-sem}})
        comp (ref {:synsem subcatted :subcat :nil!})
        head (ref {:synsem head-synsem
                   :subcat {:1 subcatted}})]
    (list
     (unify head-principle subcat-1-principle
            {:comment "s -> np vp"
             :head head
             :comp comp
             :1 comp
             :2 head}))))

(def np-1-rules 
  (let [np-rule-1 ;; NP -> Comp Head
        (unify head-principle subcat-1-principle
               (let [comp (ref {:synsem {:cat :det}})
                     head (ref {:synsem {:cat :noun}})]
                 {:comment "np -> det noun"
                  :head head
                  :comp comp
                  :1 comp
                  :2 head}))]
    (list np-rule-1)))
   
(def lexicon (concat vp-1-lexicon np-1-lexicon pronouns))

(defn lookup-in [query collection]
  "find all members of the collection that matches with query successfully."
  (if (= (.size collection) 0)
    nil
    (let [result (fs/match query (fs/copy (first collection)))]
      (if (not (fs/fail? result))
        (cons (first collection) (lookup-in query (rest collection)))
        (lookup-in query (rest collection))))))

(defn lookup [query]
  (lookup-in query lexicon))

;;;synonym
(defn find [query]
  (lookup query))


(defn it [italian]
  (set/union
   (lookup {:italian italian})
   (lookup {:italian {:infinitive italian}})))

(def rules (concat np-1-rules vp-1-rules sentence-rules))

(def np (nth rules 0))
(def vp (nth rules 1))
(def s (nth rules 2))

(defn conjugate [arg]
  (cond (nil? arg) ""
        (= (type arg) java.lang.String)
        arg
        (and (or (= (type arg)
                    clojure.lang.PersistentArrayMap)
                 (= (type arg)
                    clojure.lang.PersistentHashMap))
             (contains? (set (keys arg)) :1)
             (contains? (set (keys arg)) :2))
        (let [result1 (conjugate (:1 arg))
              result2 (conjugate (:2 arg))]
          (if (and (= (type result1) java.lang.String)
                   (= (type result2) java.lang.String))
            (string/join " " (list result1 result2))
            {:1 result1
             :2 result2}))

        (and (or (= (type arg)
                    clojure.lang.PersistentArrayMap)
                 (= (type arg)
                    clojure.lang.PersistentHashMap))
             (contains? (set (keys arg)) :agr)
             (contains? (set (keys arg)) :infinitive)
             (not (= java.lang.String (type (fs/get-in arg '(:infinitive))))))
        ;; irregular present-tense (e.g. "fare")
        (let [root (fs/get-in arg '(:infinitive))
              person (fs/get-in arg '(:agr :person))
              number (fs/get-in arg '(:agr :number))
              present (fs/get-in arg '(:infinitive :irregular :present))]
          (cond
           (and (= person :1st)
                (= number :sing))
           (fs/get-in present '(:1sing))
           (and (= person :2nd)
                (= number :sing))
           (fs/get-in present '(:2sing))
           (and (= person :3rd)
                (= number :sing))
           (fs/get-in present '(:3sing))
           (and (= person :1st)
                (= number :plur))
           (fs/get-in present '(:1plur))
           (and (= person :2nd)
                (= number :plur))
           (fs/get-in present '(:2plur))
           (and (= person :3rd)
                (= number :plur))
          (fs/get-in present '(:3plur))
           :else arg))  ;(str "[unknown conjugation:root=" root ";person=" person ";number=" number "]:" root)))

        (= (type arg) clojure.lang.Keyword)
        (str "cannot conjugate: " arg)
        
        :else
        ;; assume a map with keys (:root and :agr).
        (let [root (fs/get-in arg '(:infinitive))
              root (if (nil? root) "(nil)" root)
              root (if (not (= (type root) java.lang.String))
                      (fs/get-in arg '(:infinitive :infinitive))
                      root)
              person (fs/get-in arg '(:agr :person))
              number (fs/get-in arg '(:agr :number))
              stem (string/replace root #"[iae]re$" "")
              are-type (re-find #"are$" root)
              ere-type (re-find #"ere$" root)
              ire-type (re-find #"ire$" root)
              last-stem-char-is-i (re-find #"i$" stem)]
          (cond

           (and (= person :1st) (= number :sing))
           (str stem "o")

           (and (= person :2nd) (= number :sing)
                last-stem-char-is-i)
           (str stem)

           (and (= person :2nd) (= number :sing))
           (str stem "i")

           (and (= person :3rd) (= number :sing) (or ire-type ere-type))
           (str stem "e")

           (and (= person :3rd) (= number :sing) are-type)
           (str stem "a") 

           (and (= person :1st) (= number :plur)
                last-stem-char-is-i)
           (str stem "amo")

           (and (= person :1st) (= number :plur))
           (str stem "iamo")

           (and (= person :2nd) (= number :plur) are-type)
           (str stem "ate")

           (and (= person :2nd) (= number :plur) ere-type)
           (str stem "ete")

           (and (= person :2nd) (= number :plur) ire-type)
           (str stem "ite")

           (and (= person :3rd) (= number :plur))
           (str stem "ano")
           :else arg))))

(defn get-italian [a b]
  (let [conjugated-a (conjugate a)
        conjugated-b (if (not (nil? b)) (conjugate b) "")]
    (if (and
         (= (type conjugated-a) java.lang.String)
         (= (type conjugated-b) java.lang.String))
      (string/trim (str conjugated-a " " conjugated-b))
      {:1 conjugated-a
       :2 conjugated-b})))

(defn en [english]
  (lookup {:english english}))
;
(defn get-in [map path]
  (fs/get-in map path))

(defn over-parent-child [parent child]
  (cond

   (= (type child) java.lang.String)
   (over-parent-child parent (it child))
   
   (seq? parent)
   (flatten
    (map (fn [each-parent]
           (over-parent-child each-parent child))
         parent))

   (seq? child)
   (remove (fn [result]
             (or (fs/fail? result)
                 (nil? result)))
           (flatten
            (map (fn [each-child]
                   (let [parent parent
                         child each-child]
                     (over-parent-child parent child)))
                 child)))

   :else ; both parent and child are non-lists.
   ;; First, check to make sure complement matches head's (:synsem :sem) value; otherwise, fail.
   (let [
         ;; "add-child-where": find where to attach child (:1 or :2), depending on value of current left child (:1)'s :italian.
         ;; if (:1 :italian) is nil, the parent has no child at :1 yet, so attach new child there at :1.
         ;; Otherwise, a :child exists at :1, so attach new child at :2.
         add-child-where (if (nil?
                              (fs/get-in parent '(:1 :italian)))
                           :1
                           :2)
         head-is-where (if (= (fs/get-in parent '(:head))
                              (fs/get-in parent '(:1)))
                         :1
                         :2)
         child-is-head (= head-is-where add-child-where)
         comp (if child-is-head
                (fs/get-in parent '(:comp))
                child)
         head (if child-is-head
                child
                (fs/get-in parent '(:head)))
         sem-filter (fs/get-in head '(:subcat :2 :sem)) ;; :1 VERSUS :2 : make this more explicit about what we are searching for.
         comp-sem (fs/get-in comp '(:synsem :sem))
         do-match
         (if (and (not (nil? sem-filter))
                  (not (nil? comp-sem)))
           (fs/match {:synsem (fs/copy sem-filter)}
                     (fs/copy {:synsem (fs/copy comp-sem)})))]
     (if (= do-match :fail)
       :fail
       (let [unified (unify parent
                            {add-child-where child})]
         (if (not (fs/fail? unified))
           (merge ;; use merge so that we overwrite the value for :italian.
            unified
            {:italian (get-italian
                       (fs/get-in unified '(:1 :italian))
                       (fs/get-in unified '(:2 :italian)))})
           :fail))))))

(defn over [& args]
  "usage: (over parent child) or (over parent child1 child2)"
  (let [parent (first args)
        child1 (second args)
        child2 (if (> (.size args) 2) (nth args 2))]
    (if (not (nil? child2))
      (over-parent-child (over-parent-child parent child1) child2)
      (over-parent-child parent child1))))

(defn overa [& args]
  "try all rules as parents for the args as children."
  (let [child1 (first args)
        child2 (if (> (.size args) 1) (nth args 1))]
    (if (not (nil? child2))
      (over-parent-child (over-parent-child rules child1) child2)
      (over-parent-child rules child1))))

(def mangiare-finite
  (unify {:root (first (it "mangiare"))}
         trans-finitizer))

(defn regular-sentence []
  (let [ilragazzo (over (over np "il") "ragazzo")
        lapasta (over (over np "la") "pasta")
        myvp (over (over vp mangiare-finite) lapasta)
        sentence (over (over s ilragazzo) myvp)]
    sentence))

(defn lots-of-sentences-1 []
  (over
   (over s
         lexicon)
   (over
    (over vp lexicon)
    (over (over np lexicon) lexicon))))

(defn lots-of-sentences-2 []
  (over
   (over s
         (over (over np lexicon) lexicon))
   (over
    (over vp lexicon)
    (over (over np lexicon) lexicon))))

(defn lots-of-sentences []
  (concat
   (lots-of-sentences-1)
   (lots-of-sentences-2)))
 
;;; e.g.:
;;; (formattare (over (over s (over (over np lexicon) (lookup {:synsem {:human true}}))) (over (over vp lexicon) (over (over np lexicon) lexicon))))
(defn formattare [expressions]
  "format a bunch of expressions (feature-structures) showing just the italian."
  (if (or (= (type expressions) clojure.lang.PersistentArrayMap)
          (= (type expressions) clojure.lang.PersistentHashMap)
          (= (type expressions) clojure.lang.PersistentTreeMap))
    (formattare (list expressions))
    
    (map (fn [expr] (string/capitalize
                     (string/trim
                      (str
                       (string/trim
                        (get-italian
                         (let [italian (fs/get-in expr '(:italian))]
                           (cond
                            
                            (and (or (= (type italian)
                                        clojure.lang.PersistentArrayMap)
                                     (= (type italian)
                                        clojure.lang.PersistentHashMap))
                                 (not (nil? (fs/get-in italian '(:1 :infinitive :infinitive)))))
                            (string/join " " (list (fs/get-in italian '(:1 :infinitive :infinitive))
                                                   "(finite)"
                                                   (get-italian (fs/get-in italian '(:2)) "")))
                            
                            (and (or (= (type italian)
                                        clojure.lang.PersistentArrayMap)
                                     (= (type italian)
                                        clojure.lang.PersistentHashMap))
                                 (not (nil? (fs/get-in italian '(:1 :infinitive)))))
                            (string/join " " (list (get-italian (fs/get-in italian '(:1 :infinitive)) "")
                                                   "(finite)"
                                                   (get-italian (fs/get-in italian '(:2)) "")))
                            
                            
                            
                            (and (or (= (type italian)
                                        clojure.lang.PersistentArrayMap)
                                     (= (type italian)
                                        clojure.lang.PersistentHashMap))
                                 (not (nil? (fs/get-in italian '(:irregular)))))
                            (str (fs/get-in italian '(:infinitive)) " (finite)")
                            
                            (and (or (= (type italian)
                                        clojure.lang.PersistentArrayMap)
                                     (= (type italian)
                                        clojure.lang.PersistentHashMap))
                                 (not (nil? (fs/get-in italian '(:infinitive))))
                                 (= java.lang.String (type (fs/get-in italian '(:infinitive)))))
                            (str (fs/get-in italian '(:infinitive)) " (finite)")
                            
                            (and (or (= (type italian)
                                        clojure.lang.PersistentArrayMap)
                                     (= (type italian)
                                        clojure.lang.PersistentHashMap))
                                 (not (nil? (fs/get-in italian '(:infinitive)))))
                            (fs/get-in italian '(:infinitive :infinitive))
                           
                           :else
                           italian))
                         ""))
                       "."
                       (if (fs/get-in expr '(:comment)) (str " (" (fs/get-in expr '(:comment)) ")"))))))
         expressions)))

(def sentence-skeleton-1
  (unify s {:comp np :head (unify vp {:comp np})}))

(defn get-terminal-head-in [phrase-structure]
  (let [local-head (fs/get-in phrase-structure '(:head))]
    (if (not (nil? local-head))
      (get-terminal-head-in local-head)
      phrase-structure)))

(defn random-np-random-lexical-head [head-spec]
  (let [skeleton np
        head-specification (unify head-spec (get-terminal-head-in skeleton))
        matching-lexical-heads (mapcat (fn [lexeme] (if (not (fs/fail? lexeme)) (list lexeme)))
                                       (map (fn [lexeme] (fs/match (fs/copy head-specification) (fs/copy lexeme))) lexicon))]
    (if (> (.size matching-lexical-heads) 0)
      (nth matching-lexical-heads (rand-int (.size matching-lexical-heads))))))

(defn filter-by-match [spec lexicon]
  (mapcat (fn [lexeme] (if (not (fs/fail? lexeme)) (list lexeme)))
          (map (fn [lexeme] (fs/match (fs/copy spec) (fs/copy lexeme))) lexicon)))
  
(defn random-np [head-spec]
  (let [matching-lexical-heads (filter-by-match head-spec lexicon)
        random-lexical-head (if (> (.size matching-lexical-heads) 0)
                              (nth matching-lexical-heads (rand-int (.size matching-lexical-heads))))]
    (println (str "random-np: skel:" np))
    (println (str "np head-spec   :" head-spec))
    (println (str "random-np: rlh :" random-lexical-head))
    (let [unif (unify np {:head random-lexical-head})]
;      (println (str "unif: " unif))
      unif)))

(def head-specification (get-terminal-head-in sentence-skeleton-1))
(def matching-lexical-heads (mapcat (fn [lexeme] (if (not (fs/fail? lexeme)) (list lexeme)))
                                    (map (fn [lexeme] (fs/match (fs/copy head-specification) (fs/copy lexeme))) lexicon)))
(def random-lexical-head (if (> (.size matching-lexical-heads) 0)
                           (nth matching-lexical-heads (rand-int (.size matching-lexical-heads)))))
(def obj-spec (fs/get-in random-lexical-head '(:subcat :2)))

(def object-np (random-np {:synsem obj-spec}))

(defn random-sentence []
  (let [head-specification (get-terminal-head-in sentence-skeleton-1)
        matching-lexical-heads (mapcat (fn [lexeme] (if (not (fs/fail? lexeme)) (list lexeme)))
                                       (map (fn [lexeme] (fs/match (fs/copy head-specification) (fs/copy lexeme))) lexicon))
        random-lexical-head (if (> (.size matching-lexical-heads) 0)
                              (nth matching-lexical-heads (rand-int (.size matching-lexical-heads))))
        obj-spec (fs/get-in random-lexical-head '(:subcat :2))
        object-np (random-np {:synsem obj-spec
                              :subcat {:1 {:cat :det}}})

        subj-spec (fs/get-in random-lexical-head '(:subcat :1))
        subject-np (random-np {:synsem subj-spec
                               :subcat {:1 {:cat :det}}})]
    (unify sentence-skeleton-1
           {:head {:head random-lexical-head
                   :comp object-np}}
           {:comp subject-np})))
