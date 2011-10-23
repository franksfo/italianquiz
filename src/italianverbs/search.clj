(ns italianverbs.search
  (:use [hiccup core page-helpers]
        [clojure.test]
        [delimc.core])
  (:require
   [somnium.congomongo :as congo]
   [clojure.contrib.string :as string]
   [italianverbs.html :as html]
   [italianverbs.lexicon :as lexicon]
   [italianverbs.lev :as lev]
   [italianverbs.grammar :as gram]
   [italianverbs.morphology :as morph]))

(defn search [constraints]
  (congo/fetch :lexicon :where constraints))

(defn searchq [search-exp attrs]
  "search with query."
  (string/join " "
               (map (fn [attr]
                      (let [constraints {(keyword attr) search-exp}
                            results (search constraints)]
                        (if results
                          (html/fs (gram/choose-lexeme constraints)))))
                    (string/split (java.util.regex.Pattern/compile " ") attrs))))

(defn search-ui [request]
  (html
   [:div#search-ui {:class "quiz-elem"}
    [:h2 "cerca"]
    [:div#searchbar
     [:input {:size "50" :id "search" :type "text"}]
     [:button {:onclick "search()"} "cerca"]]
    [:div#searchresults "" ]]))

(defn test []
  (list
     {:comment "show the first (database's choice) noun."
      :test (search {:cat :noun})}
     {:comment "show the first (database's choice) verb."
      :test (search {:cat :verb})}))

(defn infl [root infl]
  (merge
   infl
   {:root root
    :infl :present
    :english
    (if (get root :english) 
      (morph/conjugate-english-verb root infl {:infl :present})
      "??")
    :italian
    (if (get root :italian)
      (morph/conjugate-italian-verb-regular root infl)
      "??")}))

(defn infl-all-root [root]
  (list
   (infl root {:person :1st :number :singular})
   (infl root {:person :2nd :number :singular})
   (infl root {:person :3rd :number :singular})
   (infl root {:person :1st :number :plural})
   (infl root {:person :2nd :number :plural})
   (infl root {:person :3rd :number :plural})))

(defn infl-all []
  (mapcat (fn [root]
         (infl-all-root root))
       (search {:cat :verb :infl :infinitive})))

(defn search-over-infl [expr]
  (mapcat (fn [inflected]
            (if (= (get inflected :italian) expr)
              (list inflected)))
          (mapcat (fn [infinitive]
                    (infl-all-root infinitive))
                  (search {:infl :infinitive}))))

(def cont1 (atom nil))
(def cont2 (atom nil))
(def cont3 (atom nil))
(def cont4 (atom nil))

(reset (+ 10 (apply (fn [a b c]
                      (+ (shift k
                                (reset! cont1 k)
                                (k 2))
                         a b c))
                    3 4 (list 5))))


(defn useconts []
  (is (= (let [cc (atom nil)]
           [(reset
             (+ (shift k
                       (reset! cc k)
                       (k 1))
                2))
            (@cc 2)
            (@cc 3)])
         [3 4 5])))


(def cont1 (atom nil))
(def cont2 (atom nil))
(def cont3 (atom nil))
(def cont4 (atom nil))

(def cont5 (atom nil))
(reset (fn [a]
         (nth (search {:cat :noun}) a)))


(reset (+ 1 (apply (fn [a b c]
                     (+ (shift k
                          (reset! cont1 k)
                            (k 1))
                         a b c))
                    3 4 (list 5)))) ;; 14

(@cont1 2) ;; 15

(reset
  (+ 1 (reset (shift k
                (reset! cont2 k)
                (k 2)))
       (reset (shift k
                (reset! cont3 k)
                (k 3))))) ;; 6

(@cont2 4) ;; 8
(@cont3 10) ;; 15

(reset (str "Hello" (shift k
                      (reset! cont4 k)
                        (k ", today is "))
            "a nice day!")) ; "Hello, today is a nice day"

(@cont4 ", yesterday was ") ; "Hello, yesterday was a nice day"
