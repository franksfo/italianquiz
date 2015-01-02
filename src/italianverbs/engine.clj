(ns italianverbs.engine
  (:refer-clojure :exclude [get-in merge])
  (:require
   [clojure.data.json :as json]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [compojure.core :as compojure :refer [GET PUT POST DELETE ANY]]
   [hiccup.page :refer (html5)]

   [italianverbs.cache :refer (create-index)]
   [italianverbs.forest :as forest]
   [italianverbs.html :refer (tablize)]
   [italianverbs.morphology :refer [fo fo-ps remove-parens]]
   [italianverbs.ug :refer (head-principle)]
   [italianverbs.unify :refer [fail? get-in merge strip-refs unify unifyc]]

   [italianverbs.english :as en]
   [italianverbs.italiano :as it]))

(declare lookup)
(declare generate-from-request)
(declare generate-from-semantics)
(declare resolve-model)

(def routes
  (compojure/routes
   (GET "/lookup" request
       (lookup request))

  (GET "/generate" request
       (generate-from-request request))

   (GET "/generate-from-semantics" request
       (generate-from-semantics request))))

(defn generate [spec language-model]
  (let [spec (unify spec
                    {:synsem {:subcat '()}})
        language-model (if (future? language-model)
                         @language-model
                         language-model)]
    (forest/generate spec 
                     (:grammar language-model)
                     (:lexicon language-model)
                     (:index language-model))))

(defn generate-from-request [request]
  "respond to an HTTP client's request with a generated sentence, given the client's desired spec, language name, and language model name."
  (let [pred (keyword (get-in request [:params :pred] :top))
        spec (get-in request [:params :spec])
        spec (if spec (json/read-str spec
                                     :key-fn keyword
                                     :value-fn (fn [k v]
                                                 (cond (string? v)
                                                       (keyword v)
                                                       :else v)))
                 :top)

        lang (get-in request [:params :lang])
        model (resolve-model (get-in request [:params :model]) lang)
        debug (get-in request [:params :debug] false)
        unified (unify {:synsem {:sem {:pred pred}}}
                       spec)

        ] ;; note that client's intended _true_ will be "true" rather than true.
    (log/info (str "generate with pred: " pred "; lang: " lang))
    (let [expression (generate unified model)
          semantics (strip-refs (get-in expression [:synsem :sem]))]
      (log/info (str "fo of expression: " (fo expression)))
      (log/info (str "semantics of expression: " semantics))
      {:status 200
       :headers {"Content-Type" "application/json;charset=utf-8"
                 "Cache-Control" "no-cache, no-store, must-revalidate"
                 "Pragma" "no-cache"
                 "Expires" "0"}
       :body (json/write-str
              (merge
               {:spec spec
                :pred pred
                (keyword lang) (fo expression)
                :semantics semantics}
               (if (or (= debug true)
                       (= debug "true"))
                 {:debug {:head (strip-refs (get-in expression [:head]))}}
                 {})))})))

(defn resolve-model [model lang]
  (cond 
        (= model "en-small")
        en/small
        (= model "it-small")
        it/small

        ;; defaults if no model is given
        (= lang "en")
        en/small

        (= lang "it")
        it/small

        true ;; TODO: throw exception "no language model" if we got here.
        en/small))

(def possible-preds [:top])

(defn lookup [request]
  (let [lang (get-in request [:params :lang] "en") ;; if no lang specified, use english.
        spec (if (not (= :null (get-in request [:params :spec] :null)))
               (json/read-str (get-in request [:params :spec])
                              :key-fn keyword
                              :value-fn (fn [k v]
                                          (cond (string? v)
                                                (keyword v)
                                                :else v)))
               :fail)
        results
        {(keyword lang)
         (string/join "," (keys
                           (into {}
                                 (for [[k v] @en/lexicon]
                                   (let [filtered-v
                                         (filter #(not (fail? (unifyc % spec)))
                                                 v)]
                                     (if (not (empty? filtered-v))
                                       [k filtered-v]))))))}]
    {:status 200
     :headers {"Content-Type" "application/json;charset=utf-8"
               
               "Cache-Control" "no-cache, no-store, must-revalidate"
               "Pragma" "no-cache"
               "Expires" "0"}
     :body (json/write-str results)}))
