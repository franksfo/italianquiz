(ns italianverbs.engine
  (:refer-clojure :exclude [get-in merge])
  (:require
   [clojure.data.json :as json]
   [clojure.string :as string]
   [clojure.tools.logging :as log]

   [hiccup.page :refer (html5)]

   [italianverbs.cache :refer (create-index)]
   [italianverbs.html :refer (tablize)]
   [italianverbs.morphology :refer [fo fo-ps remove-parens]]
   [italianverbs.translate :refer [get-meaning]]
   [italianverbs.ug :refer (head-principle)]
   [italianverbs.unify :refer [get-in merge strip-refs unify]]

   [italianverbs.english :as en]
   [italianverbs.italiano :as it]))

(def mini-en-grammar
  (filter #(or (= (:rule %) "s-present")
               (and false (= (:rule %) "s-future"))
               (and false (= (:rule %) "s-conditional")))
          en/grammar))

(def mini-en-lexicon
  (into {}
        (for [[k v] en/lexicon]
          (let [filtered-v
                (filter #(or true
                             (= (get-in % [:synsem :sem :pred]) :antonio)
                             (= (get-in % [:synsem :sem :pred]) :dormire)
                             (= (get-in % [:synsem :sem :pred]) :bere))
                        v)]
            (if (not (empty? filtered-v))
              [k filtered-v])))))

(def mini-en-index (create-index mini-en-grammar (flatten (vals mini-en-lexicon)) head-principle))

(def mini-it-grammar
  (filter #(or (= (:rule %) "s-present")
               (and false (= (:rule %) "s-future"))
               (and false (= (:rule %) "vp-future"))
               (and false (= (:rule %) "vp-present"))
               (and false (= (:rule %) "s-conditional")))
          it/grammar))

(def mini-it-lexicon
  (into {}
        (for [[k v] it/lexicon]
          (let [filtered-v
                (filter #(or true
                             (= (get-in % [:synsem :sem :pred]) :antonio)
                             (= (get-in % [:synsem :sem :pred]) :dormire)
                             (= (get-in % [:synsem :sem :pred]) :bere))
                        v)]
            (if (not (empty? filtered-v))
              [k filtered-v])))))

(def mini-it-index (create-index mini-it-grammar (flatten (vals mini-it-lexicon)) head-principle))

(def source-language-grammar mini-en-grammar)
(def source-language-index mini-en-index)
(defn source-language-generate [spec]
  (en/generate spec {:grammar source-language-grammar
                     :index source-language-index}))

(def target-language-generate it/generate)
(def target-language-grammar mini-it-grammar)
(def target-language-index mini-it-index)

(def target-language-grammar-full it/grammar)
(def target-language-index-full
;  (create-index mini-it-grammar (flatten (vals mini-it-lexicon)) head-principle))
;  (create-index mini-it-grammar (flatten (vals it/lexicon)) head-principle))
  (create-index it/grammar (flatten (vals it/lexicon)) head-principle))

(def source-language-index-full
  (create-index en/grammar (flatten (vals en/lexicon)) head-principle))

;; TODO: move out of game ns.
(defn generate-expression [spec language grammar-and-lexicon]
  (let [spec (unify spec
                    {:synsem {:subcat '()}})]
    (cond (= language "it")
          (it/generate spec grammar-and-lexicon)
          (= language "en")
          (en/generate spec grammar-and-lexicon)
          true
          (log/error "cannot generate - language: " language " not supported."))))

;; TODO: move out of game ns.
(defn generate [request]
  (let [pred (keyword (get-in request [:params :pred]))
        lang (get-in request [:params :lang])]
    (log/info (str "generate with pred: " pred "; lang: " lang))
    (let [expression (generate-expression {:synsem {:sem {:pred pred}}} lang
                                          {:grammar target-language-grammar-full
                                           :index target-language-index-full})
          semantics (strip-refs (get-in expression [:synsem :sem]))]
      (log/info (str "fo of expression: " (fo expression)))
      (log/info (str "semantics of expression: " semantics))
      {:status 200
       :headers {"Content-Type" "application/json;charset=utf-8"
                 "Cache-Control" "no-cache, no-store, must-revalidate"
                 "Pragma" "no-cache"
                 "Expires" "0"}
       :body (json/write-str
              {:pred pred
               :semantics semantics
               :semantics_display (tablize semantics)
               (keyword lang) (fo expression)})})))

;; TODO: move out of game ns.
(defn generate-from-semantics [request]
  (let [semantics (get-in request [:params :semantics])
        semantics (json/read-str semantics
                                 :key-fn keyword
                                 :value-fn (fn [k v]
                                            (cond (string? v)
                                                  (keyword v)
                                                  :else v)))
        lang (get-in request [:params :lang])]
    (generate-expression {:synsem {:sem semantics}} lang {:grammar en/grammar
                                                          :index source-language-index-full})))

(def possible-preds [:top])

;; TODO: move out of game ns.
;; TODO: support multiple languages.
(defn lookup [request]
  (let [pred (if (not (= :null (get-in request [:params :pred] :null)))
               (keyword (get-in request [:params :pred])))
        results
        (into {}
              (for [[k v] en/lexicon]
                (let [filtered-v
                      (filter #(= (get-in % [:synsem :sem :pred]) pred)
                              v)]
                  (if (not (empty? filtered-v))
                    [k filtered-v]))))]
    {:status 200
     :headers {"Content-Type" "application/json;charset=utf-8"
               
               "Cache-Control" "no-cache, no-store, must-revalidate"
               "Pragma" "no-cache"
               "Expires" "0"}
     :body (json/write-str
            {:en (string/join "," (keys results))})}))
