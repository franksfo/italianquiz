(ns italianverbs.core
  (:require
   [cemerick.friend :as friend]
   (cemerick.friend [workflows :as workflows]
                    [credentials :as creds])
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [compojure.core :as compojure :refer [defroutes GET PUT POST DELETE ANY]]
   [compojure.handler :refer [site]]
   [compojure.route :as route]
   [compojure.handler :as handler]
   [environ.core :refer [env]]
   [hiccup.page :as h]
   [hiccup.element :as e]
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
   [ring.adapter.jetty :as jetty]
   [ring.middleware.basic-authentication :as basic]
   [ring.middleware.session :as rsession]
   [ring.middleware.session.cookie :as cookie]
   [ring.middleware.stacktrace :as trace]
   [ring.util.response :as resp]
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
        :headers {"Location" "/verb/"}})

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

  (POST "/lesson/:tag/new/"
        [tag & other-params]
        (let [result (lesson/add-to-tag tag other-params)]
          {:status 302
           :headers {"Location" (str "/lesson/" tag "/")}}))


  (POST "/lesson/:tag/delete/:verb/"
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

  (GET "/login" req
    (h/html5
     pretty-head
     (pretty-body
      (page-body
       "You tried to do something that required logging in - please do so now."
       req))))

  (route/resources "/")

  ;; workaround for 'lein ring server' which opens
  ;; browser at http://localhost:3000/italian/quiz:
  ;; Assuming that Apache Server Proxying is set
  ;; up (see README), redirect to http://localhost/italian/

  ;; TODO: how to show info about the request (e.g. request path)
  (route/not-found (html/page "Non posso trovare (page not found)." (str "Non posso trovare. Sorry, page not found. ")))
)

(def main-site (apply compojure/routes
                 main-routes
                 (route/resources "/" {:root "META-INF/resources/webjars/foundation/4.0.4/"})
                 nil))

(def users (atom {"friend" {:username "friend"
                            :password (creds/hash-bcrypt "clojure")
                            :roles #{::user}}
                  "friend-admin" {:username "friend-admin"
                                  :password (creds/hash-bcrypt "clojure")
                                  :roles #{::admin}}}))

(def pretty-head
  [:head [:link {:href "/css/normalize.css" :rel "stylesheet" :type "text/css"}]
         [:link {:href "/css/foundation.min.css" :rel "stylesheet" :type "text/css"}]
         [:style {:type "text/css"} "ul { padding-left: 2em }"]
         [:script {:src "/js/foundation.min.js" :type "text/javascript"}]])

(defn pretty-body
  [& content]
  [:body {:class "row"}
   (into [:div {:class "columns small-12"}] content)])

(defn logged-in-content [req identity]
  (h/html5
   [:div
    [:p
     (apply str "Logged in, with these roles: "
            (-> identity friend/current-authentication :roles))]
    [:p (e/link-to (str "/" "logout") "Click here to log out") "."]]))

(def login-form
  [:div {:class "row"}
   [:div {:class "columns small-12"}
    [:h3 "Login"]
    [:div {:class "row"}
     [:form {:method "POST" :action "/login" :class "columns small-4"}
      [:div "Username" [:input {:type "text" :name "username"}]]
      [:div "Password" [:input {:type "password" :name "password"}]]
      [:div [:input {:type "submit" :class "button" :value "Login"}]]]]]])

(defn page-body [content req]
  (h/html5
   pretty-head
   (pretty-body

    [:h2 "The italian app"]

    (if-let [identity (friend/identity req)]
      (logged-in-content req identity)
      login-form)

    content

    [:ul 
     [:li (e/link-to (str "/" "role-user") "Requires the `user` role")]
     [:li (e/link-to (str "/" "role-admin") "Requires the `admin` role")]
     [:li (e/link-to (str "/" "requires-authentication")
                     "Requires any authentication, no specific role requirement")]])))

;; TODO: clear out cache of sentences-per-user session when starting up.
(def app
  (handler/site 
   (friend/authenticate
    main-site
    {:allow-anon? true
     :login-uri "/login"
     :default-landing-uri "/"
     :unauthorized-handler #(-> 
                             (page-body (str "You do not have sufficient privileges to access " (:uri %) ".") %)
                             resp/response
                             (resp/status 401))
     :credential-fn #(creds/bcrypt-credential-fn @users %)
     ;; in the above, the @users map functions as 1-arg fn: (fn [user]) that returns a user's
     ;; authentication and authorization info, so if you "call" @users with a given argument, (i.e. get the given
     ;; key in the @users map, e.g.:
     ;; (@users "friend")
     ;; the "return value" (i.e. value for that key) is:
     ;; {:username "friend", 
     ;;  :password "$2a$10$48TyZw9Ii6bpc.uwJtoXuuMHiRtwNPgC3yczPcpTLao0m0kaIVo02", 
     ;;  :roles #{:friend-interactive-form.users/user}}
     :workflows [(workflows/interactive-form)]})))

(defn wrap-error-page [handler]
  (fn [req]
    (try (handler req)
         (catch Exception e
           {:status 500
            :headers {"Content-Type" "text/html"}
            :body (slurp (io/resource "500.html"))}))))

(defn -main [& [port]]
  (let [port (Integer. (or port (env :port) 5000))
        ;; TODO: heroku config:add SESSION_SECRET=$RANDOM_16_CHARS
        store (cookie/cookie-store {:key (env :session-secret)})]
    (jetty/run-jetty (-> #'app
                         ((if (env :production)
                            wrap-error-page
                            trace/wrap-stacktrace))
                         (site {:session {:store store}}))
                     {:port port :join? false})))

;; For interactive development:
;; (.stop server)
;; (def server (-main))

