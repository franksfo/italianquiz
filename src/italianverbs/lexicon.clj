(ns italianverbs.lexicon
  (:refer-clojure :exclude [get-in merge resolve find])
  (:require
   [clojure.set :refer (union)]
   [clojure.tools.logging :as log]
   [italianverbs.lexiconfn :refer (adjective 
                                   agreement-noun
                                   animal
                                   clear! 
                                   cat-of-pronoun
                                   common-noun
                                   countable-noun
                                   disjunctive-case-of-pronoun
                                   drinkable-noun
                                   feminine-noun
                                   human
                                   implied 
                                   intransitive lookup-in
                                   masculine-noun
                                   modal
                                   noun pronoun-acc
                                   pronoun-noun proper-noun
                                   sentential-adverb
                                   set-lexicon
                                   subcat0 transitive unify
                                   verb
                                   verb-aux-type
                                   verb-subjective
                                             )]
   [italianverbs.lexicon_a_noi :refer (lexicon-a-noi)]
   [italianverbs.unify :refer (fail? get-in serialize)]))

(clear!)

(set-lexicon

   ;; this filter is for debugging purposes to restrict lexicon to particular entries, if desired.
   ;; default shown is (not (nil? entry)) i.e. no restrictions except that an entry must be non-nil.
   ;;  (currently there is one nil below: "chiunque (anyone)").
   (filter (fn [entry]
             (or false
                 (not (nil? entry))))

           ;; TODO: move this fn to lexiconfn: keep any code out of the lexicon proper.
           ;; this (map) adds, to each lexical entry, a copy of the serialized form of the entry.
           (map (fn [entry]
                  (if (fail? entry)
                   (log/warn (str "Ignoring this lexeme because (fail?=true): " entry))
                   ;; else, not fail, so add to lexicon.
                   (do
                     (log/debug (str "serializing entry: " entry))
                     (let [italian (get-in entry '(:italian))
                           entry
                           (conj
                            {:italian (if (string? italian)
                                        {:italian italian}
                                        italian)}
                    (dissoc
                     (if (not (= :none (get entry :serialized :none)))
                       (conj {:serialized (serialize entry)}
                             entry)
                       (conj {:serialized (serialize (dissoc entry :serialized))}
                         entry))
                     :italian))]
                       (log/debug (str "successfully serialized: " entry))
                       (implied entry)))))
               (concat
                lexicon-a-noi
                ))))
