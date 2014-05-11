(ns italianverbs.core
  (:use [compojure.core]
        [hiccup core])
  (:require
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [compojure.route :as route]
   [compojure.handler :as handler]
   [italianverbs.db :refer (fetch)]
   [italianverbs.gen :as g]
   [italianverbs.generate :as gen]
   [italianverbs.lesson :as lesson]
   [italianverbs.xml :as xml]
   [italianverbs.html :as html]
   [italianverbs.search :as search]
   [italianverbs.workbook :as workbook]
   [italianverbs.session :as session]
   [italianverbs.quiz :as quiz]
   [italianverbs.verb :as verb]
   ))

(def server-hostname (.getHostName (java.net.InetAddress/getLocalHost)))

(defn title [request]
  (let [username (session/get-username request)]
    (str "Welcome to Italian Verbs" (if username (str ", " username)) ".")))

(defn printlex [lexeme]
  (html/tablize lexeme))

(defn wrap-div [string]
  (str "<div class='test'>" string "</div>"))

;; could be e.g. "/italian/" if you have a shared web host:
(def application-prefix "")

(defroutes main-routes
;   "A handler processes the request map and returns a response map."
; http://groups.google.com/group/compojure/browse_thread/thread/3c507da23540da6e
; http://brehaut.net/blog/2011/ring_introduction
  (GET "/"
       request
       ;; response map
       {:status 302
        :headers {"Location" "/quiz/"}})

  (GET "/generate/"
       request
       {:status 302
        :headers {"Location" "/generate"}})

  (GET "/generate"
       request
       {:body (html/page "Generate" (g/generate (session/request-to-session request) request) request)
        :status 200
        :headers {"Content-Type" "text/html;charset=utf-8"}})

  (GET "/generate/:tag/"
       [tag & other-params]
       {:body (g/page "Generate" (g/generate-from tag))})

  (GET "/lesson/"
       request
       {:status 302
        :headers {"Location" "/lesson"}})

  (GET "/lesson"
       request
       {:body (html/page "Lesson" (lesson/lesson (session/request-to-session request) request) request)
        :status 200
        :headers {"Content-Type" "text/html;charset=utf-8"}})

  (GET "/lesson/:tag/"
       request
       {:body (html/page "Lesson" (lesson/show (session/request-to-session request) (:tag (:route-params request))) request)})

  (POST "/lesson/:tag/new"
        [tag & other-params]
        (let [result (lesson/add-to-tag tag other-params)]
          {:status 302
           :headers {"Location" (str "/lesson/" tag "/")}}))

  (POST "/lesson/:tag/delete/:verb"
        [tag verb]
        (let [result (lesson/delete-from-tag tag verb)]
          {:status 302
           :headers {"Location" (str "/lesson/" tag "/")}}))

  (POST "/lesson/delete/:tag"
        [tag]
       (let [result (lesson/delete tag)]
         {:status 302
          :headers {"Location" (str "/lesson/?result=" (:message result))}}))

  (GET "/lesson/new"
       request
       {:body (html/page "New Lesson" (lesson/new (session/request-to-session request) request) request)
        :status 200
        :headers {"Content-Type" "text/html;charset=utf-8"}})
  (GET "/lesson/new/"
       request
       {:status 302
        :headers {"Location" "/lesson/new"}})

  (POST "/lesson/new"
       request
       (let [result (lesson/new (session/request-to-session request) request)]
       {:status 302
        :headers {"Location" (str "/lesson/?result=" (:message result))}}))
  (POST "/lesson/new/"
       request
       (let [result (lesson/new (session/request-to-session request) request)]
       {:status 302
        :headers {"Location" (str "/lesson/?result=" (:message result))}}))

  ;; TODO: make this a POST with 'username' and 'password' params so that users can login.
  (GET "/session/set/"
       request
       {
        :side-effect (session/register request)
        :session (get request :session)
        :status 302
        :headers {"Location" "/?msg=set"}
        })

   (GET "/session/clear/"
       request
       {
        :side-effect (session/unregister request)
        :status 302
        :headers {"Location" "/?msg=cleared"}
        })

  (GET "/generate/:tag/"
       [tag & other-params]
       {:body (g/page "Generate" (g/generate-from tag))})

  (GET "/lesson/"
       request
       {:status 302
        :headers {"Location" "/lesson"}})

  (GET "/lesson"
       request
       {:body (html/page "Lesson" (lesson/lesson (session/request-to-session request) request) request)
        :status 200
        :headers {"Content-Type" "text/html;charset=utf-8"}})

  (GET "/lesson/:tag/"
       request
       {:body (html/page "Lesson" (lesson/show (session/request-to-session request) (:tag (:route-params request))) request)})

  (POST "/lesson/:tag/new"
        [tag & other-params]
        (let [result (lesson/add-to-tag tag other-params)]
          {:status 302
           :headers {"Location" (str "/lesson/" tag "/")}}))


  (POST "/lesson/:tag/delete/:verb"
        [tag verb]
        (let [result (lesson/delete-from-tag tag verb)]
          {:status 302
           :headers {"Location" (str "/lesson/" tag "/")}}))

  (POST "/lesson/delete/:tag"
        [tag]
       (let [result (lesson/delete tag)]
         {:status 302
          :headers {"Location" (str "/lesson/?result=" (:message result))}}))

  (POST "/lesson/new/"
       request
       (let [result (lesson/new (session/request-to-session request) request)]
       {:status 302
        :headers {"Location" (str "/lesson/?result=" (:message result))}}))

  ;; TODO: make this a POST with 'username' and 'password' params so that users can login.
  (GET "/session/set/"
       request
       {
        :side-effect (session/register request)
        :session (get request :session)
        :status 302
        :headers {"Location" "/?msg=set"}
        })

  (GET "/verb/"
       request
       {:body (html/page "Verbs" (verb/select (session/request-to-session request) request) request)
        :status 200
        :headers {"Content-Type" "text/html;charset=utf-8"}})

  ;; TODO: figure out how to combine destructuring with sending request (which we need for the 
  ;; menubar and maybe other things like authorization.
  (GET "/verb/:verb/"
       [verb]
       {:body (html/page "Verbs" (verb/select-one verb) {:uri "/verb/"})
        :status 200
        :headers {"Content-Type" "text/html;charset=utf-8"}})

  (POST "/verb/:verb/delete/"
        [verb]
        (let [result (verb/delete verb)]
          {:status 302
           :headers {"Location" (str "/verb/?result=" (:message result))}}))
  (POST "/verb/new/"
       request
       (let [result (verb/new (session/request-to-session request) request)]
       {:status 302
        :headers {"Location" (str "/verb/?result=" (:message result))}}))
  (POST "/verb/update/"
        [id updated]
        (do
          (log/info (str "updating verb with id: " id))
          (log/info (str "updating verb with updated: " updated))
          (let [result (verb/update id updated)]
            {:status 302
             :headers {"Location" (str "/verb/" id "/?result=" (:message result))}})))


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

  (route/resources "/")

  ;; workaround for 'lein ring server' which opens
  ;; browser at http://localhost:3000/italian/quiz:
  ;; Assuming that Apache Server Proxying is set
  ;; up (see README), redirect to http://localhost/italian/

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

;; TODO: clear out cache of sentences-per-user session when starting up.
(def app
  (handler/site main-routes))
