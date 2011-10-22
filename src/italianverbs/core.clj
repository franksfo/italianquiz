(ns italianverbs.core
  (:use [compojure.core]
        [somnium.congomongo]
        [hiccup core page-helpers])
  (:require
   [clojure.string :as string]
   [compojure.route :as route]
   [compojure.handler :as handler]
   [italianverbs.test :as test]
   [italianverbs.generate :as gen]
   [italianverbs.lev :as lev]
   [italianverbs.xml :as xml]
   [base.html :as html]
   [italianverbs.html :as ihtml]
   [italianverbs.search :as search]
   [italianverbs.session :as session]
   [italianverbs.quiz :as quiz]
   ))

(defn title [request]
  (let [username (session/get-username request)]
    (str "Welcome to Italian Verbs" (if username (str ", " username)) ".")))

(defn printlex [lexeme]
  (ihtml/fs lexeme))

(defn wrap-div [string]
  (str "<div class='test'>" string "</div>"))

(defroutes main-routes
;   "A handler processes the request map and returns a response map."
; http://groups.google.com/group/compojure/browse_thread/thread/3c507da23540da6e
; http://brehaut.net/blog/2011/ring_introduction
  (GET "/" 
       request
       ;; response map
       {:status 302
        :headers {"Location" "/italian/quiz/"}})

  (GET "/preferiti/"
       request
       {:body (quiz/preferiti request)
        :status 200
        :headers {"Content-Type" "text/html;charset=utf-8"}})

  (GET "/about/"
       request
       {:body (html/page "About" (html/about) request)
        :status 200
        :headers {"Content-Type" "text/html;charset=utf-8"}})

  (GET "/quiz/"
       request
       {:body (quiz/quiz request)
        :status 200
        :headers {"Content-Type" "text/html;charset=utf-8"}})

  (GET "/quiz"
       request
       {:status 302
        :headers {"Location" "/italian/quiz/"}})

  (GET "/search/"
       request
       {:status 200
        :body (html/page "Search" (search/search-ui request) request)
        :headers {"Content-Type" "text/html;charset=utf-8"}})

  (GET "/search/q/"
       request
       {:status 200
        :body (search/searchq (get (get request :query-params) "search")
                              (get (get request :query-params) "attrs"))
        :headers {"Content-Type" "text/html;charset=utf-8"}})

  (GET "/lexicon/" 
       request
       ;; response map
       {
        :session (get request :session)
        :headers {"Content-Type" "text/html"}
        :body
        (do ;"reload lexicon into mongodb and then render it as HTML."
          (load-file "src/italianverbs/lexicon.clj")
          (html/page "Lexicon"
                     (string/join " "
                                  (map printlex
                                       (fetch :lexicon :sort {"italian" 1})))
                     request))})

  ;; show user's quiz filter preferences. format param (in request) controls output's formatting:
  ;; might be a comma-separated list of filters, a bunch of checkboxes, xml, json, etc.
  (GET "/quiz/filter/"
       request
       {
        :session (get request :session)
        :status 200
        :headers {"Content-Type" "text/html;charset=utf-8"}
        :body (quiz/show-controls (session/request-to-session request)
                                  (get request :query-params) "/italian/quiz/filter/" "Quiz")
        }
       )

  (POST "/quiz/filter/"
       request
       {
        :session (get request :session)
        :side-effect (quiz/set-filters (session/request-to-session request) request)
        :status 302
        :headers {"Location" "/italian/quiz/filter/"}
        }
       )
  
  (POST "/quiz/clear" 
       request
       {
        :session (get request :session)
        :side-effect (quiz/clear-questions (session/request-to-session request))
        :status 302
        :headers {"Location" "/quiz/"}
       }
       )

  (GET "/test/" 
       request
       {
        :headers {"Content-Type" "text/html"}
        :session (get request :session)
        :body (html/page "test"
                    (test/run-tests)
                    request)
        })

  (POST "/test/" 
       request
       {
        :session (get request :session)
        :headers {"Content-Type" "text/html"}
        :body (html/page "test" 
                    (map wrap-div 
                         test/tests)
                    request)
        })
  
;; TODO: make this a POST with 'username' and 'password' params.
  (GET "/session/set/"  
       request
       {
        :session (get request :session)
        :side-effect (session/register request)
        :status 302
        :headers {"Location" "/?msg=set"}
        })

  (GET "/session/clear/" 
       request 
       {
        :session (get request :session)
        :side-effect (session/unregister request)
        :status 302
        :headers {"Location" "/?msg=cleared"}
        })

  ;; xml is default, so just redirect to /guess/.
  ;; TODO: pass params along.
  (GET "/guess/xml/"
       request
       {:status 302
        :headers {"Location" "/guess/"}})

  (GET "/guess/"
       request
       {:body
        (let [type (if (quiz/question-type (get request :query-params))
                     (quiz/question-type (get request :query-params))
                     (quiz/random-guess-type (session/request-to-session request)))
              question (quiz/generate type)]
          (quiz/guess question request "xml"))
        :status 200
        :headers {"Content-Type" "text/xml;charset=utf-8"}
        })

  (GET "/types/"
       request
       {:body
        (quiz/types)
        :status 200
        :headers {"Content-Type" "text/xml;charset=utf-8"}
        })

  ;; create a new question, store in backing store, and return question's english form
  ;; to pose question to user.
  (GET "/quiz/question/"
       request
       {
        :body
        (quiz/question request)
        :status 200
        :headers {"Content-Type" "text/html;charset=utf-8"}
        })

  (GET "/guess/tr/"
       request
       {
        :body
        (let [type (quiz/random-guess-type)
              question (quiz/generate type)]
          (quiz/guess question request "tr"))
        :status 200
        :headers {"Content-Type" "text/html;charset=utf-8"}
        })

  (POST "/quiz/evaluate/"
       request
       {
        :body
        (quiz/evaluate request "tr")
        :status 200
        :headers {"Content-Type" "text/html;charset=utf-8"}
        })
  
  (route/resources "/")

  (route/not-found (html/page "Non posso trovare (page not found)." (str "Non passo trovare. Sorry, page not found. ")))
)


; http://weavejester.github.com/compojure/compojure.handler-api.html
; site function

;Usage: (site routes & [opts])

;Create a handler suitable for a standard website. This adds the
;following middleware to your routes:
;  - wrap-session
;  - wrap-cookies
;  - wrap-multipart-params
;  - wrap-params
;  - wrap-nested-params
;  - wrap-keyword-params
; A map of options may also be provided. These keys are provided:
;  :session - a map of session middleware options

(def app
  (handler/site main-routes))

