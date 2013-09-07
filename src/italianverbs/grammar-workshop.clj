(ns italianverbs.grammar-workshop
  (:use [italianverbs.grammar]
        [clojure.set :only (union)]
        [italianverbs.generate :only (generate moreover-head moreover-comp gen14)]
        [italianverbs.lexicon :only (it1)]
        [italianverbs.lexiconfn :only (unify sem-impl)]
        [italianverbs.morphology :only (finalize fo italian-article)]
        [italianverbs.unify :only (copy fail? serialize)]))

(defn generate-sentences-with-subjects-1 [subcat-info subject-heads subject-comps]
  "filter subject-heads by subcat-info"
  (base-cc10 (fn [subject-head]
                 (not (fail? (unify subcat-info subject-head))))
                subject-heads))

(defn generate-sentences-with-subjects [subcat-infos subject-heads subject-comps]
  "generate subjects: filter heads by subcat-infos."
  (if (not (empty? subcat-infos))
    (lazy-cat (generate-sentences-with-subjects-1 (first subcat-infos) subject-heads subject-comps)
              (generate-sentences-with-subjects (rest subcat-infos) subject-heads subject-comps))))

(defn take-sentences-randomly [n]
  (take n
        (let [vps
              ;; head: VP -> V NP
              (gen15 (list hh21)
                     (filter (fn [candidate]
                               (and (not (= :notfound (get-in candidate '(:synsem :subcat :2 :cat) :notfound)))
                                    (= (get-in candidate '(:synsem :cat)) :verb)))
                             (shuffle hh21-heads)) ;; Verb
                     base-cc10-random)] ;; object NP
          (generate-sentences-with-subjects
            (map (fn [head-of-parent-cc10] ;; parent-cc10 is the top-level sentential-cc10.
                   (get-in head-of-parent-cc10 '(:synsem :subcat :1)))
                 vps)
            (shuffle cc10-heads) ;; Noun of subject
            (shuffle cc10-comps))))) ;; Det of subject

