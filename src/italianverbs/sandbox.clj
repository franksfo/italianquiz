(ns italianverbs.sandbox
  [:use
   [clojure.core :exclude [find]]
   [italianverbs.lexiconfn]
   [italianverbs.lexicon]]
  [:require
   [italianverbs.fs :as fs]
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.tools.logging :as log]])

(def sentence-rules
  (let [subj-sem (ref :top)
        subcatted (ref {:cat :noun
                        :sem subj-sem})
        head-synsem (ref {:cat :verb
                          :infl {:not :infinitive}
                          :sem {:subj subj-sem}
                          :subcat {:1 subcatted}})
        comp (ref {:synsem subcatted})
        head (ref {:synsem head-synsem})]
    (list
     (unify head-principle subcat-1-principle
            {:comment "s -> np vp"
             :head head
             :comp comp
             :1 comp
             :2 head}))))

(def np-1-rules 
  ;; TODO: combine (case,person,number,gender) into a single map labeled :agr for ease of notation
  (let [np-rule-1 ;; NP -> Comp Head
        (unify head-principle subcat-1-principle
               (let [case (ref :top)
                     comp (ref {:synsem {:cat :det}})
                     person (ref :top)
                     number (ref :top)
                     gender (ref :top)
                     agr (ref :top)
                     head (ref {:synsem {:cat :noun
                                         :agr agr}})]
                 {:comment "np -> det noun"
                  :synsem {:agr agr}
                  :head head
                  :comp comp
                  :1 comp
                  :2 head}))]
    (list np-rule-1)))
   
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
   (lookup {:italian {:infinitive italian}})
   (lookup {:root {:italian italian}})))

(def rules (concat np-1-rules vp-1-rules sentence-rules))

(def np (nth rules 0))
(def vp (nth rules 1))
(def s (nth rules 2))

(defn conjugate-it [arg]
  "conjugate an italian expression."
  (cond (nil? arg) ""
        (= (type arg) java.lang.String)
        arg
        (and (map? arg)
             (contains? (set (keys arg)) :1)
             (contains? (set (keys arg)) :2))
        (let [result1 (conjugate-it (:1 arg))
              result2 (conjugate-it (:2 arg))]
          (if (and (= (type result1) java.lang.String)
                   (= (type result2) java.lang.String))
            (string/join " " (list result1 result2))
            {:1 result1
             :2 result2}))

        (and (map? arg)
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

        (and (map? arg)
             (contains? arg :root)
             (contains? arg :agr)
             (= (fs/get-in arg '(:agr :gender)) :fem)
             (= (fs/get-in arg '(:agr :number)) :sing))
        (fs/get-in arg '(:root))

        (and (map? arg)
             (contains? arg :root)
             (contains? arg :agr)
             (= (fs/get-in arg '(:agr :gender)) :masc)
             (= (fs/get-in arg '(:agr :number)) :sing))
        (fs/get-in arg '(:root))
        
        ;; feminine noun pluralization
        (and (map? arg)
             (contains? arg :root)
             (contains? arg :agr)
             (= (fs/get-in arg '(:agr :gender)) :fem)
             (= (fs/get-in arg '(:agr :number)) :plur))
        (string/replace (fs/get-in arg '(:root))
                        #"a$" "e")

        ;; masculine noun pluralization
        (and (map? arg)
             (contains? arg :root)
             (contains? arg :agr)
             (= (fs/get-in arg '(:agr :gender)) :masc)
             (= (fs/get-in arg '(:agr :number)) :plur))
        (string/replace (fs/get-in arg '(:root))
                        #"[eo]$" "i") ;; dottore => dottori; medico => medici
        
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

(defn conjugate-en [arg]
  (cond (nil? arg) ""
        (= (type arg) java.lang.String)
        arg
        (and (map? arg)
             (contains? (set (keys arg)) :1)
             (contains? (set (keys arg)) :2))
        (let [result1 (conjugate-en (:1 arg))
              result2 (conjugate-en (:2 arg))]
          (if (and (= (type result1) java.lang.String)
                   (= (type result2) java.lang.String))
            (string/join " " (list result1 result2))
            {:1 result1
             :2 result2}))

        (and (map? arg)
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

        (and (map? arg)
             (contains? arg :root)
             (contains? arg :agr)
             (= (fs/get-in arg '(:agr :number)) :sing))
        (str (fs/get-in arg '(:root)))

        (and (map? arg)
             (contains? arg :root)
             (contains? arg :agr)
             (= (fs/get-in arg '(:agr :number)) :plur))
        (str (fs/get-in arg '(:root)) "s")
        
        :else
        ;; assume a map with keys (:root and :agr).
        (let [root (fs/get-in arg '(:infinitive))
              root (if (nil? root) "(nilroot)" root)
              root (if (not (= (type root) java.lang.String))
                      (fs/get-in arg '(:infinitive :infinitive))
                      root)
              person (fs/get-in arg '(:agr :person))
              number (fs/get-in arg '(:agr :number))
              stem (string/replace root #"^to " "")
              last-stem-char-is-e (re-find #"e$" stem)]
          (cond

           (and (= person :1st) (= number :sing))
           (str stem "")

           (and (= person :2nd) (= number :sing))
           (str stem "")

           (and (= person :3rd) (= number :sing))
           (str stem "s")

           (and (= person :1st) (= number :plur))
           (str stem "")

           (and (= person :2nd) (= number :plur))
           (str stem "")

           (and (= person :3rd) (= number :plur))
           (str stem "")
           :else arg))))

(defn get-italian [a b]
  (let [conjugated-a (conjugate-it a)
        conjugated-b (if (not (nil? b)) (conjugate-it b) "")]
    (if (and
         (string? conjugated-a)
         (string? conjugated-b))
      (string/trim
       (cond
        (and (= conjugated-a "il")
             (re-find #"^s[t]" conjugated-b))
        (str "lo " conjugated-b)
        (and (= conjugated-a "i")
             (re-find #"^[aeiou]" conjugated-b))
        (str "gli " conjugated-b)
        (and (= conjugated-a "il")
             (re-find #"^[aeiou]" conjugated-b))
        (str "l'" conjugated-b)
        (and (= conjugated-a "la")
             (re-find #"^[aeiou]" conjugated-b))
        (str "l'" conjugated-b)
        true
        (string/trim (str conjugated-a " " conjugated-b))))
      {:1 conjugated-a
       :2 conjugated-b})))

(defn get-english [a b]
  (let [conjugated-a (conjugate-en a)
        conjugated-b (if (not (nil? b)) (conjugate-en b) "")]
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
         sem-filter (fs/get-in head '(:synsem :subcat :2 :sem)) ;; :1 VERSUS :2 : make this more explicit about what we are searching for.
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
                       (fs/get-in unified '(:2 :italian)))
             :english (get-english
                       (fs/get-in unified '(:1 :english))
                       (fs/get-in unified '(:2 :english)))})

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
  (if (map? expressions)
    (formattare (list expressions))
    (map (fn [expr]
           (let [english (string/capitalize (fs/get-in expr '(:english)))
                 comment (fs/get-in expr '(:comment))]
             (string/trim
              (str
               (string/trim
                (string/capitalize
                 (get-italian
                  (let [italian (fs/get-in expr '(:italian))]
                    (cond
                     (and (map? italian)
                          (not (nil? (fs/get-in italian '(:1 :infinitive :infinitive)))))
                     (string/join " " (list (fs/get-in italian '(:1 :infinitive :infinitive))
                                            "(finite)"
                                            (get-italian (fs/get-in italian '(:2)) "")))
                     (and (map? italian)
                          (not (nil? (fs/get-in italian '(:1 :infinitive)))))
                     (string/join " " (list (get-italian (fs/get-in italian '(:1 :infinitive)) "")
                                            "(finite)"
                                            (get-italian (fs/get-in italian '(:2)) "")))
                     (and (map? italian)
                          (not (nil? (fs/get-in italian '(:irregular)))))
                     (str (fs/get-in italian '(:infinitive)) " (finite)")
                     (and (map? italian)
                          (not (nil? (fs/get-in italian '(:infinitive))))
                          (= java.lang.String (type (fs/get-in italian '(:infinitive)))))
                     (str (fs/get-in italian '(:infinitive)) " (finite)")
                     (and (map? italian)
                          (not (nil? (fs/get-in italian '(:infinitive)))))
                     (fs/get-in italian '(:infinitive :infinitive))
                     
                     :else
                     italian))
                  "")))
                " (" english ")" "."))))
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
;    (println (str "random-np: skel:" np))
;    (println (str "np head-spec   :" head-spec))
;    (println (str "random-np: rlh :" random-lexical-head))
    (let [matching-lexical-comps (filter-by-match {:synsem (fs/get-in random-lexical-head '(:synsem :subcat :1))}
                                                  lexicon)
          random-lexical-comp (if (> (.size matching-lexical-comps) 0)
                                (nth matching-lexical-comps (rand-int (.size matching-lexical-comps))))]
      (let [unified (unify np {:head random-lexical-head
                               :comp random-lexical-comp})]
        (if (not (fs/fail? unified))
          (merge
           {:italian (get-italian
                      (fs/get-in unified '(:1 :italian))
                      (fs/get-in unified '(:2 :italian)))
            :english (get-english
                      (fs/get-in unified '(:1 :english))
                      (fs/get-in unified '(:2 :english)))}
           unified)
          unified)))))

                                        ;(def head-specification (get-terminal-head-in sentence-skeleton-1))
(def head-specification (get-terminal-head-in vp))
(def matching-lexical-heads (mapcat (fn [lexeme] (if (not (fs/fail? lexeme)) (list lexeme)))
                                    (map (fn [lexeme] (fs/match (fs/copy head-specification) (fs/copy lexeme))) lexicon)))
(def random-lexical-head (if (> (.size matching-lexical-heads) 0)
                           (nth matching-lexical-heads (rand-int (.size matching-lexical-heads)))))
(def obj-spec (fs/get-in random-lexical-head '(:synsem :subcat :2)))

(def object-np (random-np {:synsem obj-spec}))

(def subj-spec (fs/get-in random-lexical-head '(:synsem :subcat :1)))
(def subject-np (random-np {:synsem (unify subj-spec
                                           {:subcat {:1 {:cat :det}}})}))

(defn random-subject-np [head-spec]
  (let [rand (rand-int 2)]
    (if (= rand 0)
      (random-np {:synsem
                  (unify
                   head-spec
                   {:subcat {:1 {:cat :det}}})})
      (let [matching (filter-by-match
                      {:synsem
                       (unify
                        head-spec
                        {:cat :noun
                         :agr {:case :nom}
                         :subcat :nil!})}
                      lexicon)]
        (if (> (.size matching) 0)
          (nth matching (rand-int (.size matching))))))))

(defn random-sentence []
  (let [head-specification ;(get-terminal-head-in sentence-skeleton-1)
        (get-terminal-head-in vp)
        matching-lexical-verb-heads (mapcat (fn [lexeme] (if (not (fs/fail? lexeme)) (list lexeme)))
                                            (map (fn [lexeme] (fs/match head-specification lexeme)) lexicon))
        random-verb (if (> (.size matching-lexical-heads) 0)
                              (nth matching-lexical-heads (rand-int (.size matching-lexical-heads))))
        obj-spec (fs/get-in random-verb '(:synsem :subcat :2))
        object-np
        (random-np (unify {:synsem (unify obj-spec
                                          {:subcat {:1 {:cat :det}}})}))
        subj-spec (fs/get-in random-verb '(:synsem :subcat :1))
        subject-np (random-subject-np subj-spec)]
    (let [unified (unify sentence-skeleton-1
                         {:head
                          (let [unified
                                (unify
                                 (fs/get-in sentence-skeleton-1 '(:head))
                                 {:head random-verb
                                  :comp object-np})]
                            (fs/merge
                             {:italian
                              (get-italian
                               (fs/get-in unified '(:1 :italian))
                               (fs/get-in unified '(:2 :italian)))
                              :english
                              (get-english
                               (fs/get-in unified '(:1 :english))
                               (fs/get-in unified '(:2 :english)))}
                             unified))}
                         {:comp subject-np})]
      (if (not (fs/fail? unified))
        (merge
         {:italian (get-italian
                    (fs/get-in unified '(:1 :italian))
                    (fs/get-in unified '(:2 :italian)))
          :english (get-english
                    (fs/get-in unified '(:1 :english))
                    (fs/get-in unified '(:2 :english)))}
         unified)
        unified))))

(defn random-sentences [n]
  (repeatedly n (fn [] (random-sentence))))
