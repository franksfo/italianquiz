(ns italianverbs.core
  (:require
   [compojure.core :refer [context defroutes GET PUT POST DELETE ANY]]
   [italianverbs.html :as html]
   [italianverbs.tour :as tour]
   [ring.util.response :as resp]

   [cemerick.friend :as friend]
   (cemerick.friend [workflows :as workflows]
                    [credentials :as creds])
   [clojure.java.io :as io]
   [clojure.tools.logging :as log]
   [compojure.core :refer [context defroutes GET PUT POST DELETE ANY]]
   [compojure.route :as route]
   [compojure.handler :as handler]
   [environ.core :refer [env]]
   [hiccup.page :as h]
   [italianverbs.auth :as auth :refer [confirm-and-create-user get-user-id haz-admin is-authenticated]]
   [italianverbs.html :as html]
   [italianverbs.session :as session]
   [italianverbs.tour :as tour]
   [italianverbs.verb :as verb]
   [ring.adapter.jetty :as jetty]
   [ring.middleware.session.cookie :as cookie]
   [ring.middleware.stacktrace :as trace]
))

;; not used at the moment, but might be handy someday:
(def server-hostname (.getHostName (java.net.InetAddress/getLocalHost)))

(defroutes main-routes
  ;; top-level page: currently redirects to the "Cloud" game.
  (GET "/" request
       ;; response map
       {:status 302
        :headers {"Location" "/tour"}})


  (context "/gen" []
           verb/routes)

  (context "/tour" []
           tour/routes)

  (GET "/about" request
       {:status 200
        :body (html/page "Welcome to Verbcoach"
                         (html/about)
                         request)})

  (POST "/login" request
        (resp/redirect "/"))

  (GET "/logout" request
    (friend/logout* (resp/redirect "/login")))

  ;; TODO: make this a POST with 'username' and 'password' params so that users can login.
  (GET "/session/set/" request
       {:side-effect (session/register request)
        :session (get request :session)
        :status 302
        :headers {"Location" "/?msg=set"}})

  (GET "/session/clear/" request
       {:side-effect (session/unregister request)
        :status 302
        :headers {"Location" "/?msg=cleared"}})

  (route/resources "/webjars" {:root "META-INF/resources/webjars/foundation/4.0.4/"})
  (route/resources "/")

  ;; TODO: how to show info about the request (e.g. request path and error code).

  ;; currently, we show this '404' error regardless of what the error actually is (might be 500 or otherwise).
;  (route/not-found (html/page "Non posso trovare questa pagina (page not found)." (str "Non posso trovare questa pagina. Sorry, page not found. ")))
)

;; <BEGIN TEST AUTHENTICATION/AUTHORIZATION>
;; TODO: move to dedicated namespace.
(def users (atom {"franco" {:username "franco"
                            :password (creds/hash-bcrypt "franco")
                            :roles #{::user ::admin}}
                  "gino" {:username "gino"
                          :password (creds/hash-bcrypt "gino")
                          :roles #{::user}}}))
(derive ::admin ::user)
;; </BEGIN TEST AUTHENTICATION/AUTHORIZATION>

;; TODO: clear out cache of sentences-per-user session when starting up.
(def app
  (handler/site 
   (friend/authenticate
    main-routes
    {:allow-anon? true
     :login-uri "/login"
     :default-landing-uri "/"
     :unauthorized-handler #(-> 
                             (html/page "Unauthorized" (h/html5 

                                                        [:div {:class "major tag"}
                                                         [:h2 "Unauthorized"]
                                                         [:p "You do not have sufficient privileges to access " (:uri %) "."]]) %)
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
                         (handler/site {:session {:store store}}))
                     {:port port :join? false})))

;; For interactive development:
;; (.stop server)
;; (def server (-main))

