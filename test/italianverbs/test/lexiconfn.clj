(ns italianverbs.test.lexiconfn
  (:use [clojure.test]
        [italianverbs.lexiconfn])
  (:require
   [somnium.congomongo :as mongo]
   [italianverbs.fs :as fs]))

;; TODO: for testing, consider some kind of mongodb mocking of some kind,
;; so that no actual mongodb connection would be necessary.

;(deftest parlare ;; A lexical entry for the word: 'parlare'
;  (let [merge-and-parla
;        (let [verb {:cat :verb}
;              human-subj {:subj {:human true}}
;              third-sing {:subj {:number :singular :person :3rd}}])]
;   (= (is true true))))

(deftest minimal-insert-and-deserialize
  (let [testmap {:a 42}]
    (clear!)
    (add-lexeme testmap)
    (let [deserialized (fs/deserialize (:entry (mongo/fetch-one :lexicon)))]
      (is (not (nil? deserialized)))
      (is (= deserialized {:a 42}))
      (is (= deserialized testmap)))))

(deftest insert-and-deserialize-with-shared
  (let [ref1 (ref 42)
        testmap {:a ref1}]
    (clear!)
    (add-lexeme testmap)
    (let [deserialized (fs/deserialize (:entry (mongo/fetch-one :lexicon)))
          deserialized-ref (get deserialized :a)]
      (is (= (type deserialized-ref) clojure.lang.Ref))
      (let [dereferenced @deserialized-ref]
        (is (= dereferenced 42))))))

(deftest fatto
  (let [ref3 (ref "avere")
        ref2 (ref {:italian "fatto"})
        ref1 (ref {:infl :infinitive
                   :italian ref3})
        vp {:a ref1
            :b {:italian ref2
                :root {:infl :infinitive
                       :pass-prossimo ref2
                       :pass-prossimo-aux ref1}}
            :italian {:a ref3
                      :b ref2}
            :infl :infinitive}
        clear-lexicon (clear!)
        add-lexical-entry (add-lexeme vp)
        lookup-lexicon (mongo/fetch-one :lexicon)
        deserialized (fs/deserialize (:entry lookup-lexicon))
        ]
    (is (not (nil? vp)))
    (is (not (nil? add-lexical-entry)))
    (is (not (nil? lookup-lexicon)))
    (is (not (nil? deserialized)))
    (is (= (get (get deserialized "italian") "a")
           (get @(get deserialized "a") "italian")))))

(deftest compiti
  (let [compiti {:italian "compiti"
                 :english "homework"}
        add-lexical-entry (add-lexeme compiti)
        lookup-lexicon (mongo/fetch-one :lexicon)
        deserialized (fs/deserialize (:entry lookup-lexicon))]
  (is (not (nil? add-lexical-entry)))
  (is (not (nil? lookup-lexicon)))
  (is (not (nil? deserialized)))))
