(ns italianverbs.test.generate
  (:use [italianverbs.generate]
        [clojure.test])
  (:require
   [italianverbs.lexiconfn :as lexfn]
   [italianverbs.search :as search]
   [italianverbs.fs :as fs]))

(deftest first-person-singular-fare
  "The 1st person singular present form of 'fare' should be findable."
  (let [results (search/search {:root {:infl :infinitive
                                       :italian "fare"}
                                :person :1st
                                :number :singular})]
    (is (not (= nil results)))
    (is (not (= nil (first results))))
    (is (= (:italian (first results)) "facio"))))

(deftest facio
  (let [result (fs/unify {:root (first (search/search {:cat :verb :italian "fare" :infl :infinitive}))}
                         {:infl :present}
                         (select-keys
                          (first (search/search {:italian "io" :pronoun true}))
                          (list :person :number)))]
    (is (> (count result) 0))))

(deftest facio-2
  (let [result (search/search {:root {:cat :verb
                                      :italian "fare"
                                      :infl :infinitive}
                               :infl :present
                               :subj {:person :1st
                                      :number :singular}})]
    (is (not (= nil result)))
    (is (not (= nil (first result))))))

(deftest io-facio
  "Conjugate 'io' + 'fare' => 'io  facio'"
  (let [conjugated (conjugate-verb (fs/merge (search/search-one {:italian "fare" :infl :infinitive})
                                             {:infl :present})
                                   (search/search-one {:italian "io" :case :nom}))]
    (is (= (:italian conjugated) "facio"))))


(deftest merged-fs
  (let [resulting-fs
        (fs/unify {:root (first (search/search {:cat :verb :italian "fare" :infl :infinitive}))}
                  {:infl :present}
                  (select-keys
                   (first (search/search {:italian "io" :pronoun true}))
                   (list :person :number)))]
    (is (> (count resulting-fs) 0))))

(deftest lookup-irregular-verb
  (let [results
        (search/search {:root {:cat :verb
                               :italian "fare"
                               :infl :infinitive}
                        :infl :present
                        :subj {:person :1st
                               :number :singular}})]
    (is (not (= nil results)))
    (is (not (= nil (first results))))))

(deftest random-noun
  "Random noun, chosen with 'top' as its number value."
  (let [noun (search/random-lexeme {:number :top :cat :noun})]
    (is (not (= noun nil)))
    (is (not (= (:number noun) nil)))
    (is (not (= (:number noun) :top)))))

(deftest subject-1
  "Make sure subjects are all real lexical entries by checking for non-null :italian feature"
  (let [sentences (map (fn [sentence] (:italian (:subject sentence)))
                       (n-sentences 10))]
    (is (= 0 (.size (remove #(not (= nil %)) sentences))))))

(deftest subject-2
  "Make sure subject's :number value is valid (non-:fail)."
  (let [subjects (map (fn [sentence] (:subject sentence))
                       (n-sentences 10))]
    (is (= (.size subjects)
           (.size (remove (fn [subject] (= (:number subject) :fail)) subjects))))))

(deftest subject-3
  "Make sure subject's :case value is ok (non-:acc and non-:fail). nil is ok: common nouns (in italian
       and english) don't have case."
  (let [cases
        (map (fn [sentence] (:case (:subject sentence)))
             (n-sentences 10))]
    (is (= (.size cases)
           (.size (remove (fn [case] (or (= case :fail) (= case "acc"))) cases))))))

(deftest il-libro
  "Conjugate 'libro' + '{definite}' => 'il libro'."
  (let [conjugated (conjugate-np (lexfn/lookup "libro") {:def :def})]
    (is (= (:italian conjugated) "il libro"))))

(deftest i-libri
  "Conjugate 'libro' + '{definite,plural}' => 'i libri'."
  (let [conjugated (conjugate-np (fs/merge (lexfn/lookup "libro") {:number :plural}) {:def :def})]
    (is (= (:italian conjugated) "i libri"))))

(deftest le-sedie
  "Conjugate 'sedia' + '{definite,plural}' => 'le sedie'."
  (let [conjugated (conjugate-np (fs/merge (lexfn/lookup "sedia") {:number :plural}) {:def :def})]
    (is (= (:italian conjugated) "le sedie"))))

(deftest leggo-il-libro
  "Conjugate 'leggere/[1st sing]-il-libro' => 'leggo il libro'."
  (let [vp
        (let [root-verb (nth (search/search {:italian "leggere" :cat :verb :infl :infinitive}) 0)
              object (conjugate-np (nth (search/search {:italian "libro" :cat :noun}) 0) {:def :def})]
          (if root-verb
            (conjugate-vp (fs/unify root-verb {:infl :present})
                          (nth (search/search {:italian "io" :case :nom}) 0)
                          object)))]
    (is (= (:italian vp) "leggo il libro"))))

(deftest io-leggo-il-libro
  "Conjugate 'leggere/[1st sing]-il-libro' => 'leggo il libro' / 'io' => 'io leggo il libro'."
  (let [vp
        (let [root-verb (nth (search/search {:italian "leggere" :cat :verb :infl :infinitive}) 0)
              object (conjugate-np (nth (search/search {:italian "libro" :cat :noun}) 0) {:def :def})]
          (if root-verb
            (conjugate-vp (fs/unify root-verb {:infl :present})
                          (nth (search/search {:italian "io" :case :nom}) 0)
                          object)
            {:fail "verb 'leggere' not found."}))
        sentence (conjugate-sent vp (:subject vp))]
    (is (= (:italian sentence) "io leggo il libro"))))





  
