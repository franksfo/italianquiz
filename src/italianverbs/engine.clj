(ns italianverbs.engine
  (:refer-clojure :exclude [get-in merge])
  (:require
   [clojure.data.json :as json]
   [clojure.string :as string]
   [clojure.tools.logging :as log]

   [hiccup.page :refer (html5)]

   [italianverbs.cache :refer (create-index)]
   [italianverbs.generate :as generate]
   [italianverbs.html :refer (tablize)]
   [italianverbs.morphology :refer [fo fo-ps remove-parens]]
   [italianverbs.translate :refer [get-meaning]]
   [italianverbs.ug :refer (head-principle)]
   [italianverbs.unify :refer [fail? get-in merge strip-refs unify unifyc]]

   [italianverbs.english :as en]
   [italianverbs.italiano :as it]))

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

(def target-language-generate it/generate)
(def target-language-grammar mini-it-grammar)
(def target-language-index mini-it-index)
(def target-language-lexicon mini-it-lexicon)
(def target-language-lexicon-full it/lexicon)

(def target-language-grammar-full it/grammar)
(def target-language-index-full
;  (create-index mini-it-grammar (flatten (vals mini-it-lexicon)) head-principle))
;  (create-index mini-it-grammar (flatten (vals it/lexicon)) head-principle))
  (create-index it/grammar (flatten (vals it/lexicon)) head-principle))

(defn generate [spec language-model]
  (let [spec (unify spec
                    {:synsem {:subcat '()}})]
    (generate/generate spec 
                       (:grammar language-model)
                       (:lexicon language-model)
                       (:index language-model))))

(defn generate-from-request [request]
  (let [pred (keyword (get-in request [:params :pred]))
        lang (get-in request [:params :lang])]
    (log/info (str "generate with pred: " pred "; lang: " lang))
    (let [expression (generate {:synsem {:sem {:pred pred}}}
                               it/small)
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

(defn generate-from-semantics [request]
  (let [semantics (get-in request [:params :semantics])
        semantics (json/read-str semantics
                                 :key-fn keyword
                                 :value-fn (fn [k v]
                                            (cond (string? v)
                                                  (keyword v)
                                                  :else v)))
        model (get-in request [:params :model])
        translation
        (generate {:synsem {:sem semantics}}
                  (cond (= model "en")
                        en/small
                        (= model "en-small")
                        en/small
                        (= model "it")
                        it/small
                        (= model "it-small")
                        it/small
                        true ;; TODO: throw exception if we got here.
                        en/small))]
    {:status 200
     :headers {"Content-Type" "application/json;charset=utf-8"
               "Cache-Control" "no-cache, no-store, must-revalidate"
               "Pragma" "no-cache"
               "Expires" "0"}
     :body (json/write-str
            {:semantics semantics
             :fo-ps (fo-ps translation)
             :response (fo translation)})}))

(def possible-preds [:top])

;; TODO: support multiple languages: for now, only works with english.
(defn lookup [request]
  (let [spec (if (not (= :null (get-in request [:params :spec] :null)))
               (json/read-str (get-in request [:params :spec])
                              :key-fn keyword
                              :value-fn (fn [k v]
                                          (cond (string? v)
                                                (keyword v)
                                                :else v)))
               :fail)
        results
        (into {}
              (for [[k v] en/lexicon]
                (let [filtered-v
                      (filter #(not (fail? (unifyc % spec)))
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
