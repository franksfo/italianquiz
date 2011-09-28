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
  (GET "/" 
       ;; request map: access it with (get request X),
       ;; where X in {:session,:request-method,:uri,...}
       request

       ;; response map
       {
        :session (get request :session)
        :side-effect (quiz/set-filters (session/request-to-session request) request)
        :status 302
        :headers {"Location" "/quiz/"}
       }
 )

  (GET "/lexicon/" 
       request
       ;; response map
       {
        :session (get request :session)
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
  (GET "/quiz/" 
       request
       ;; response map
       {
        :session (get request :session)
        :body (page "Quiz"
                    (quiz/run request)
                    request)
        }
       )

  (GET "/quiz/display" 
       request
       ;; response map
       {
        :session (get request :session)
        :body (page "Quiz"
                    (quiz/display request)
                    request)
        }
       )
  
  (POST "/quiz/"
       request
       ;; response map
       {
        :session (get request :session)
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
  
  (POST "/quiz/filter" ;; for now just run quiz.
       request
       {
        :session (get request :session)
        :side-effect (quiz/set-filters (session/request-to-session request) request)
        :status 302
        :headers {"Location" "/quiz/display#controlbottom"}
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
        :session (get request :session)
        :body (page "test"
                    (test/run-tests)
                    request)
        })

  (POST "/test/" 
       request
       {
        :session (get request :session)
        :body (page "test" 
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

  (GET "/guess/"
       request
       {
        :body (xml-response "guess.."
                            (italianverbs.html/fs (italianverbs.quiz/generate :passato))
                            request)
        :status 200
        :headers {"Content-type" "text/xml"}
        })

  (route/resources "/")
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

