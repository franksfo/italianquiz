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
   [italianverbs.studenttest :as stest]
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

;; not used at the moment, but might be handy someday:
(def server-hostname (.getHostName (java.net.InetAddress/getLocalHost)))

(defn haz-admin []
  (if (not (nil? (friend/current-authentication)))
    (not (nil?
          (:italianverbs.core/admin
           (:roles (friend/current-authentication)))))))

(defroutes main-routes
  (GET "/" request
       ;; response map
       {:status 302
        :headers {"Location" "/verb/"}})

  (GET "/generate/" request
       {:status 302
        :headers {"Location" "/generate"}})

  (GET "/generate" request
       (friend/authorize #{::admin}
                         {:body (html/page "Generate" (g/generate (session/request-to-session request) request) request)
                          :status 200
                          :headers {"Content-Type" "text/html;charset=utf-8"}}))

  (GET "/generate/:tag/" request
       (friend/authorize #{::admin}
                         (let [tag (:tag (:route-params request))]
                           {:status 200
                            :body (html/page "Generate" (g/generate-from tag) request)})))

  (GET "/lesson/" request
       {:status 302
        :headers {"Location" "/lesson"}})

  (GET "/lesson" request
       {:body (html/page "Groups" (lesson/lesson (session/request-to-session request) request haz-admin) request)
        :status 200
        :headers {"Content-Type" "text/html;charset=utf-8"}})

  (POST "/lesson/delete/:tag" request
        (friend/authorize #{::admin}
                          (let [tag (:tag (:route-params request))]
                            (let [result (lesson/delete tag)]
                              {:status 302
                               :headers {"Location" (str "/lesson/?result=" (:message result))}}))))

  (GET "/lesson/new" request
        (friend/authorize #{::admin}
                          {:status 302
                           :headers {"Location" "/lession"}}))

  (GET "/lesson/new/" request
       {:status 302
        :headers {"Location" "/lesson/new"}})

  (POST "/lesson/new" request
        (friend/authorize #{::admin}
                          (let [result (lesson/new (session/request-to-session request) request (haz-admin))]
                            {:status 302
                             :headers {"Location" (str "/lesson/?result=" (:message result))}})))

  (POST "/lesson/new/" request
        (friend/authorize #{::admin}
                          (let [result (lesson/new (session/request-to-session request) request)]
                            {:status 302
                             :headers {"Location" (str "/lesson/?result=" (:message result))}})))



  (GET "/lesson/:tag/" request
       (friend/authorize #{::admin}
                         {:body (html/page "Groups" (lesson/show (session/request-to-session request) 
                                                                 (:tag (:route-params request))
                                                                 (haz-admin)) request)}))

  (POST "/lesson/:tag/delete/:verb" request
       (friend/authorize #{::admin}
                         (let [tag (:tag (:route-params request))
                               verb (:verb (:route-params request))]
                           (let [result (lesson/delete-from-tag tag verb)]
                             {:status 302
                              :headers {"Location" (str "/lesson/" tag "/")}}))))

  (GET "/lesson/:tag/new/" request
        (friend/authorize #{::admin}
                          (let [tag (:tag (:route-params request))]
                            (let [result (lesson/add-to-tag tag request)]
                              {:status 302
                               :headers {"Location" (str "/lesson/" tag "/")}}))))
  (POST "/lesson/:tag/new/" request
        (friend/authorize #{::admin}
                          (let [tag (:tag (:route-params request))]
                            (let [result (lesson/add-to-tag tag request)]
                              {:status 302
                               :headers {"Location" (str "/lesson/" tag "/")}}))))


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

  (GET "/lesson/" request
       {:status 302
        :headers {"Location" "/lesson"}})

  (GET "/lesson" request
       {:body (html/page "Groups" (lesson/lesson (session/request-to-session request) request haz-admin) request)
        :status 200
        :headers {"Content-Type" "text/html;charset=utf-8"}})

  (GET "/login" request
       {:status 401
        :body (html/page "Unauthenticated: please login or register."
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

  (GET "/test" request
       (friend/authorize #{::admin}
                         {:status 200
                          :headers {"Content-Type" "text/html;charset=utf-8"}
                          :body (html/page "Tests" (stest/show))}))



  ;; bounced after authentication: would be better to re-submit post if possible.
  (GET "/test/new/" request
       (friend/authorize #{::admin}
                         {:status 302
                          :headers {"Location" "/"}}))

  (POST "/test/new/" request
        (friend/authorize #{::admin}
                          (let [result (stest/new (session/request-to-session request) request)]
                            {:status 302
                             :headers {"Location" (str "/test/" (:message result))}})))





  (GET "/verb/" request
       (if-let [identity (friend/identity request)]
         {:body (html/page 
               "Verbs" 

                (verb/select (session/request-to-session request) 
                             request
                             haz-admin)
                request)

        :status 200
        :headers {"Content-Type" "text/html;charset=utf-8"}}
         {:status 303
          :headers {"Location" (str "/login")}}))

  ;; TODO: figure out how to combine destructuring with sending request (which we need for the
  ;; menubar and maybe other things like authorization.
  (GET "/verb/:verb/" request
       (let [verb (:verb (:route-params request))]
         {:body (html/page "Verbs"
                           (verb/select-one verb (haz-admin))
                           request)
          :status 200}))

  (POST "/verb/:verb/delete/" request
        (friend/authorize #{::admin}
                          (let [verb (:verb (:route-params request))]
                            (let [result (verb/delete verb)]
                              {:status 302
                               :headers {"Location" (str "/verb/?result=" (:message result))}}))))

  (POST "/verb/new/" request
        (friend/authorize #{::admin}
                          (let [result (verb/new (session/request-to-session request) request)]
                            {:status 302
                             :headers {"Location" (str "/verb/?result=" (:message result))}})))

  (POST "/verb/:id/update/" request
        (friend/authorize #{::admin}
                          (let [id (:id (:route-params request))
                                updated (get (:form-params request) "updated")]
                            (log/info (str "updating verb with id: " id))
                            (log/info (str "keys of form-params:" (keys (:form-params request))))
                            (log/info (str "updating verb with form-params: " (:form-params request)))
                            (log/info (str "updating verb with updated: " updated))
                            (let [result (verb/update id updated)]
                              {:status 302
                               :headers {"Location" (str "/verb/" id "/?result=" (:message result))}}))))

  (GET "/verb/:id/update/" request
       (friend/authorize #{::admin}
                         (let [id (:id (if (and request (:route-params request))
                                         (:route-params request)))
                               updated (if (and request (:form-params request))
                                         (get (:form-params request) "updated"))]
                           (if (nil? id)
                             (let [result {:message "nosuchverb"}]
                               {:status 302
                                :headers {"Location" (str "/verb/")}})
                             (if (nil? updated)
                               (let [result {:message "noupdate"}]
                                 {:status 302
                                  :headers {"Location" (str "/verb/" id "/")}})
                               (do
                                 (log/info (str "updating verb with id: " id))
                                 (log/info (str "keys of form-params:" (keys (:form-params request))))
                                 (log/info (str "updating verb with form-params: " (:form-params request)))
                                 (log/info (str "updating verb with updated: " updated))
                                 (let [result (verb/update id updated)]
                                   {:status 302
                                    :headers {"Location" (str "/verb/" id "/?result=" (:message result))}})))))))

  (GET "/workbook/" request
       {:status 200
        :body (html/page "Workbook" (workbook/workbook-ui request) request)
        :headers {"Content-Type" "text/html;charset=utf-8"}})

  (GET "/workbook/q/" request
       {:status 200
        :body (workbook/workbookq (get (get request :query-params) "search")
                                  (get (get request :query-params) "attrs"))
        :headers {"Content-Type" "text/html;charset=utf-8"}})

  (GET "/requires-authentication" request
    (friend/authenticated
     (resp/redirect "/")))

  (GET "/role-user" request
    (friend/authorize #{::user}
                      {:body (html/page "You're a user." 
                                        (h/html5
                                         [:h2
                                          "You're a user."])
                                        request)}))

  (GET "/role-admin" request
    (friend/authorize #{::admin}
                      {:body (html/page "You're an admin.." 
                                        (h/html5
                                         [:h2
                                          "You're an admin."])
                                        request)}))

  (route/resources "/webjars" {:root "META-INF/resources/webjars/foundation/4.0.4/"})
  (route/resources "/")

  ;; TODO: how to show info about the request (e.g. request path)
  (route/not-found (html/page "Non posso trovare (page not found)." (str "Non posso trovare. Sorry, page not found. "))))


(def main-site main-routes)


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
    main-site
    {:allow-anon? true
     :login-uri "/login"
     :default-landing-uri "/"
     :unauthorized-handler #(-> 
                             (html/page "Unauthorized" (h/html5 [:h2 "You do not have sufficient privileges to access " (:uri %) "."]) %)
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

