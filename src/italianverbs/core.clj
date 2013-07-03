(ns italianverbs.core
  (:use [compojure.core]
        [somnium.congomongo]
        [hiccup core])
  (:require
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [compojure.route :as route]
   [compojure.handler :as handler]
   [italianverbs.generate :as gen]
   [italianverbs.lev :as lev]
   [italianverbs.xml :as xml]
   [base.html :as html]
   [italianverbs.html :as ihtml]
   [italianverbs.search :as search]
   [italianverbs.workbook :as workbook]
   [italianverbs.sandbox :as sandbox]
   [italianverbs.session :as session]
   [italianverbs.quiz :as quiz]
   ))

(def server-hostname (.getHostName (java.net.InetAddress/getLocalHost)))

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

  ;; TODO: a lot of repeated request-to-session looking-up going on here.
  (GET "/quiz/"
       request
       {:body (if (session/request-to-session request)
                (quiz/quiz request))
        :status (if (session/request-to-session request)
                  200
                  (do (log/info "No existing session - you must be new here. Redirecting to /italian/session/set.")
                      302))
        :headers (if (session/request-to-session request)
                   {"Content-Type" "text/html;charset=utf-8"}
                   {"Location" "/italian/session/set/"})})

  (GET "/quiz"
       request
       {:status 302
        :headers {"Location" "/italian/quiz/"}})

  (GET "/search"
       request
       {:status 302
        :headers {"Location" "/italian/search/"}})

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

  ;; <workbook>
  (GET "/workbook"
       request
       {:status 302
        :headers {"Location" "/italian/workbook/"}})

  (GET "/workbook/"
       request
       {:status 200
        :body (html/page "Workbook" (workbook/workbook-ui request) request)
        :headers {"Content-Type" "text/html;charset=utf-8"}})

  (GET "/workbook/q/"
       request
       {:status 200
        :body (workbook/workbookq (get (get request :query-params) "search")
                                  (get (get request :query-params) "attrs"))
        :headers {"Content-Type" "text/html;charset=utf-8"}})
  ;; </workbook>

  (GET "/lexicon/"
       request
       ;; response map
       {
        :headers {"Content-Type" "text/html"}
        :body
        (do ;"reload lexicon into mongodb and then render it as HTML."
          (load-file "src/italianverbs/lexicon.clj")
          (html/page "Lexicon"
                     (string/join " "
                                  (map printlex
                                       (fetch :lexicon :sort {"italian" 1})))
                     request))})


  ;; show all the results of the sentence generation unit tests.
  (GET "/generate/"
       request
       ;; response map
       {
        :headers {"Content-Type" "text/html"}
        :body
        (html/pagemacro "Sentence Generation"
                        (gen/generate-signs))
        })


  ;; show user's quiz filter preferences. format param (in request) controls output's formatting:
  ;; might be a comma-separated list of filters, a bunch of checkboxes, xml, json, etc.
  (GET "/quiz/filter/"
       request
       {
        :status 200
        :headers {"Content-Type" "text/html;charset=utf-8"}
        :body (quiz/show-controls (session/request-to-session request)
                                  (get request :query-params) "/italian/quiz/filter/" "Quiz")
        }
       )

  (POST "/quiz/filter/"
       request
       {
        :side-effect (quiz/set-filters (session/request-to-session request) request)
        :status 302
        :headers {"Location" "/italian/quiz/filter/"}
        }
       )

  (POST "/quiz/clear"
       request
       {
        :side-effect (quiz/clear-questions (session/request-to-session request))
        :status 302
        :headers {"Location" "/quiz/"}
       }
       )

  ;; TODO: make this a POST with 'username' and 'password' params so that users can login.
  (GET "/session/set/"
       request
       {
        :side-effect (session/register request)
        :session (get request :session)
        :status 302
        :headers {"Location" "/italian/?msg=set"}
        })

  (GET "/session/clear/"
       request
       {
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
       {:body (quiz/question request)
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

  ;; workaround for 'lein ring server' which opens
  ;; browser at http://localhost:3000/italian/quiz:
  ;; Assuming that Apache Server Proxying is set
  ;; up (see README), redirect to http://localhost/italian/
  (GET "/italian/quiz/"
       request
       {:status 302
        :headers {"Location" (str "http://" server-hostname "/italian/")}})
  (GET "/italian/workbook/"
       request
       {:status 302
        :headers {"Location" (str "http://" server-hostname "/italian/")}})

  ;; TODO: how to show info about the request (e.g. request path)
  (route/not-found (html/page "Non posso trovare (page not found)." (str "Non posso trovare. Sorry, page not found. ")))
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
