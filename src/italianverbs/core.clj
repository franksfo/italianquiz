(ns italianverbs.core
  (:use [compojure.core]
        [somnium.congomongo]
        [base.html])
  (:require
   [clojure.string :as string]
   [compojure.route :as route]
   [compojure.handler :as handler]
   [italianverbs.test :as test]
   [italianverbs.generate :as gen]
   [italianverbs.lev :as lev]
   [italianverbs.xml :as xml]
   [italianverbs.html :as ihtml]
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
       ;; request map: access it with (get request X),
       ;; where X in {:session,:request-method,:uri,...}
       request

       ;; response map
       {
        :session (get request :session)
        :side-effect (quiz/set-filters (session/request-to-session request) request)
        :status 302
        :headers {"Location" "/italian/quiz/"}
       }
 )
  (GET "/italian/" 
       ;; request map: access it with (get request X),
       ;; where X in {:session,:request-method,:uri,...}
       request

       ;; response map
       {
        :session (get request :session)
        :side-effect (quiz/set-filters (session/request-to-session request) request)
        :status 302
        :headers {"Location" "/italian/quiz/"}
       }
 )

  (GET "/italian/lexicon/" 
       request
       ;; response map
       {
        :session (get request :session)
        :headers {"Content-type" "text/html"}
        :body
        (do ;"reload lexicon into mongodb and then render it as HTML."
          (load-file "src/italianverbs/lexicon.clj")
          (page "Lexicon"
                (string/join " "
                             (map printlex
                                  (fetch :lexicon :sort {"italian" 1})))
                request))
        }
       )

  ;; TO DO: /quiz/ and quiz/display/ should
  ;; be the same thing. for now we need this here
  ;; in order to initialize a new quiz.
  (GET "/italian/old/quiz/" 
       request
       ;; response map
       {
        :headers {"Content-type" "text/html"}
        :session (get request :session)
        :body (page "Quiz"
                    (quiz/run request)
                    request)
        }
       )

  (GET "/italian/old/quiz/display" 
       request
       ;; response map
       {
        :session (get request :session)
        :headers {"Content-type" "text/html"}
        :body (page "Quiz"
                    (quiz/display request)
                    request)
        }
       )
  
  (POST "/italian/old/quiz/"
       request
       ;; response map
       {
        :session (get request :session)
        :headers {"Content-type" "text/html"}
        :body (page "Quiz"
                    (quiz/run request)
                    request)
        }
       )

                                        ; TODO: when quiz/filter works, use this instead of the redirect.
                                        ; might also redirect after :side-effect instead of a separate filter page.
                                        ;  :body (page "Quiz"
                                        ;              (quiz/filter request)
                                        ;              request)
  
  (POST "/italian/quiz/filter"
       request
       {
        :session (get request :session)
        :side-effect (quiz/set-filters (session/request-to-session request) request)
        :status 302
        :headers {"Location" "/italian/quiz/display#controlbottom"}
        }
       )

  (GET "/italian/quiz/filter/ajax/"
       request
       {
        :session (get request :session)
        :status 200
        :headers {"Content-type" "text/html;charset=ISO-8859-1"}
        :body (quiz/ajax-controls (session/request-to-session request) "/italian/quiz/filter/ajax/")
        }
       )

  (POST "/italian/quiz/filter/ajax/"
       request
       {
        :session (get request :session)
        :side-effect (quiz/set-filters (session/request-to-session request) request)
        :status 302
        :headers {"Location" "/italian/quiz/filter/ajax/"}
        }
       )

  (GET "/italian/quiz/filter/iframe/"
       request
       {
        :session (get request :session)
        :status 200
        :headers {"Content-type" "text/html;charset=ISO-8859-1"}
        :body (quiz/iframe-controls (session/request-to-session request) "/italian/quiz/filter/iframe/")
        }
       )
  
  (POST "/italian/quiz/filter/iframe/"
       request
       {
        :session (get request :session)
        :side-effect (quiz/set-filters (session/request-to-session request) request)
        :status 302
        :headers {"Location" "/italian/quiz/filter/iframe/"}
        }
       )

  
  (POST "/italian/quiz/clear" 
       request
       {
        :session (get request :session)
        :side-effect (quiz/clear-questions (session/request-to-session request))
        :status 302
        :headers {"Location" "/italian/quiz/"}
       }
       )

  (GET "/italian/test/" 
       request
       {
        :headers {"Content-type" "text/html"}
        :session (get request :session)
        :body (page "test"
                    (test/run-tests)
                    request)
        })

  (POST "/italian/test/" 
       request
       {
        :session (get request :session)
        :headers {"Content-type" "text/html"}
        :body (page "test" 
                    (map wrap-div 
                         test/tests)
                    request)
        })
  
;; TODO: make this a POST with 'username' and 'password' params.
  (GET "/italian/session/set/"  
       request
       {
        :session (get request :session)
        :side-effect (session/register request)
        :status 302
        :headers {"Location" "/italian/?msg=set"}
        })

  (GET "/italian/session/clear/" 
       request 
       {
        :session (get request :session)
        :side-effect (session/unregister request)
        :status 302
        :headers {"Location" "/italian/?msg=cleared"}
        })

  ;; xml is default, so just redirect to /guess/.
  ;; TODO: pass params along.
  (GET "/italian/guess/xml/"
       request
       {:status 302
        :headers {"Location" "/italian/guess/"}})

  ;; TODO: move quiz/* (except for quiz/guess) calls to body of quiz/guess.
  ;; TODO: add POST equivalents for all of these.
  (GET "/italian/guess/"
       request
       {
        :body
        (let [type (quiz/random-guess-type (session/request-to-session request))
              question (quiz/generate type)]
          (quiz/guess question request "xml"))
        :status 200
        :headers {"Content-type" "text/xml;charset=ISO-8859-1"}
        })

  ;; create a new question, store in backing store, and return question's english form
  ;; to pose question to user.
  (GET "/italian/guess/question/"
       request
       {
        :body
        (quiz/question request)
        :status 200
        :headers {"Content-type" "text/html;charset=ISO-8859-1"}
        })

  (GET "/italian/guess/tr/"
       request
       {
        :body
        (let [type (quiz/random-guess-type)
              question (quiz/generate type)]
          (quiz/guess question request "tr"))
        :status 200
        :headers {"Content-type" "text/html;charset=ISO-8859-1"}
        })

  (GET "/italian/evaluate/tr/"
       request
       {
        :body
        (quiz/evaluate request "tr")
        :status 200
        :headers {"Content-type" "text/html;charset=ISO-8859-1"}
        })

  (POST "/italian/evaluate/tr/"
       request
       {
        :body
        (quiz/evaluate request "tr")
        :status 200
        :headers {"Content-type" "text/html;charset=ISO-8859-1"}
        })

  (GET "/italian/evaluate/xml/"
       request
       {
        :body
        (quiz/evaluate request "xml")
        :status 200
        :headers {"Content-type" "text/xml;charset=ISO-8859-1"}
        })

  (GET "/italian/evaluate/xmltr/"
       request
       {
        :body
        (quiz/evaluate request "xmltr")
        :status 200
        :headers {"Content-type" "text/xml;charset=ISO-8859-1"}
        })
  
  (GET "/italian/guess/xmltr/"
       request
       {
        :body
        (let [type (quiz/random-guess-type)
              question (quiz/generate type)]
          (quiz/guess question request "xmltr"))
        :status 200
        :headers {"Content-type" "text/xml;charset=ISO-8859-1"}
        })

  (GET "/italian/guess/html/"
       request
       {
        :body
        (let [type (quiz/random-guess-type)
              question (quiz/generate type)]
          (quiz/guess question request "html"))
        :status 200
        :headers {"Content-type" "text/html;charset=ISO-8859-1"}
        })

  (GET "/italian/quiz/minimal/"
       request
       {:body (quiz/minimal request)
        :status 200
        :headers {"Content-type" "text/html;charset=ISO-8859-1"}})

  (GET "/italian/quiz/"
       request
       {:body (quiz/quiz-with-prefs request)
        :status 200
        :headers {"Content-type" "text/html;charset=ISO-8859-1"}})
  
  (route/resources "/italian/")
  (route/not-found (page "Non posso trovare (page not found)." "Non passo trovare. Sorry, page not found.")))

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

