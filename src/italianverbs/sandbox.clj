(ns italianverbs.sandbox
  [:use
   [clojure.core :exclude [find]]
   [italianverbs.lexiconfn]
   [italianverbs.lexicon]
   [italianverbs.morphology]
   [italianverbs.generate]
   [italianverbs.grammar]]
  [:require
   [italianverbs.fs :as fs]
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.tools.logging :as log]])

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

