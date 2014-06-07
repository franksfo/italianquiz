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
   [italianverbs.class :as vc-class]
   [italianverbs.gen :as g]
   [italianverbs.generate :as gen]
   [italianverbs.lesson :as lesson]
   [italianverbs.xml :as xml]
   [italianverbs.html :as html]
   [italianverbs.search :as search]
   [italianverbs.session :as session]
   [italianverbs.studenttest :as stest]
   [italianverbs.test_submit :as tsubmit]
   [italianverbs.question :as question]
   [italianverbs.quiz :as quiz]
   [italianverbs.verb :as verb]
   [italianverbs.workbook :as workbook]
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

  (GET "/about" request
       {:status 200
        :body (html/page "Welcome to Verbcoach"
                         (html/about)
                         request)})

  (GET "/class/" request
       {:status 302
        :headers {"Location" "/class"}})

  (GET "/class" request
       {:body (html/page "Classes" (vc-class/class (session/request-to-session request) request (haz-admin)) request)
        :status 200
        :headers {"Content-Type" "text/html;charset=utf-8"}})

  (POST "/class/delete/:class" request
        (friend/authorize #{::admin}
                          (let [tag (:class (:route-params request))]
                            (let [result (vc-class/delete tag)]
                              {:status 302
                               :headers {"Location" (str "/class/?result=" (:message result))}}))))

  ;; for now, just redirect GET /class/new -> GET /class
  (GET "/class/new" request
        (friend/authorize #{::admin}
                          {:status 302
                           :headers {"Location" "/class"}}))
  (GET "/class/new/" request
       {:status 302
        :headers {"Location" "/class/new"}})

  (POST "/class/new" request
        (friend/authorize #{::admin}
                          (let [result (vc-class/new (session/request-to-session request) request (haz-admin))]
                            {:status 302
                             :headers {"Location" (str "/class/?result=" (:message result))}})))

  (POST "/class/new/" request
        (friend/authorize #{::admin}
                          (let [result (vc-class/new (session/request-to-session request) request)]
                            {:status 302
                             :headers {"Location" (str "/class/?result=" (:message result))}})))

  (GET "/class/:class" request
       {:body (html/page "Classes" (vc-class/show (session/request-to-session request) 
                                               (:class (:route-params request))
                                               (haz-admin)) request)})
  (GET "/class/:class/" request
       {:body (html/page "Classes" (vc-class/show (session/request-to-session request) 
                                               (:class (:route-params request))
                                               (haz-admin)) request)})

  (GET "/class/:class/delete/:student/" request
       (friend/authorize #{::admin}
                         (let [tag (:class (:route-params request))
                               verb (:verb (:route-params request))]
                           (let [result {:message "redirected-no-effect"}]
                             {:status 302
                              :headers {"Location" (str "/class/" tag "/")}}))))

  (POST "/class/:class/delete/:student" request
       (friend/authorize #{::admin}
                         (let [tag (:class (:route-params request))
                               student (:student (:route-params request))]
                           (let [result (vc-class/delete-from-class class student)]
                             {:status 302
                              :headers {"Location" (str "/class/" tag "/")}}))))
  (POST "/class/:class/delete/:student/" request
       (friend/authorize #{::admin}
                         (let [tag (:class (:route-params request))
                               student (:student (:route-params request))]
                           (let [result (vc-class/delete-from-class class student)]
                             {:status 302
                              :headers {"Location" (str "/class/" tag "/")}}))))
  (POST "/class/:class/add/:student" request
        (friend/authorize #{::admin}
                          (let [tag (:class (:route-params request))]
                            (let [result (vc-class/add-to-tag tag request)]
                              {:status 302
                               :headers {"Location" (str "/class/" tag "/")}}))))
  (POST "/class/:class/add/:student/" request
        (friend/authorize #{::admin}
                          (let [tag (:class (:route-params request))]
                            (let [result (vc-class/add-to-tag tag request)]
                              {:status 302
                               :headers {"Location" (str "/class/" tag "/")}}))))

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
       {:body (html/page "Groups" (lesson/lesson (session/request-to-session request) request (haz-admin)) request)
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
       {:body (html/page "Groups" (lesson/show (session/request-to-session request) 
                                               (:tag (:route-params request))
                                               (haz-admin)) request)})

  (GET "/lesson/:tag/delete/:verb/" request
       (friend/authorize #{::admin}
                         (let [tag (:tag (:route-params request))
                               verb (:verb (:route-params request))]
                           (let [result {:message "redirected-no-effect"}]
                             {:status 302
                              :headers {"Location" (str "/lesson/" tag "/")}}))))

  (POST "/lesson/:tag/delete/:verb" request
       (friend/authorize #{::admin}
                         (let [tag (:tag (:route-params request))
                               verb (:verb (:route-params request))]
                           (let [result (lesson/delete-from-tag tag verb)]
                             {:status 302
                              :headers {"Location" (str "/lesson/" tag "/")}}))))
  (POST "/lesson/:tag/delete/:verb/" request
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

  (GET "/session/clear/" request
       {:side-effect (session/unregister request)
        :status 302
        :headers {"Location" "/?msg=cleared"}})

  (POST "/question/new" request
        (let [testid (if request (get (:form-params request) "testid"))
              new-question (question/new (:form-params request))]
          (friend/authorize #{::admin}
                            {:status 302
                             :headers {"Location" (str "/test/" testid "?result=added+question")}})))

  (POST "/question/:id/delete" request
        (friend/authorize #{::admin}
                          (let [question (:id (:route-params request))
                                debug (log/info (str "THE REQUEST IS: " request))
                                test (get (:form-params request) "test")]
                            (let [result (stest/delete-question question)]
                              {:status 302
                               :headers {"Location" (str "/test/" test "?result=" (:message result))}}))))


  ;; TODO: make this a POST with 'username' and 'password' params so that users can login.
  (GET "/session/set/" request
       {:side-effect (session/register request)
        :session (get request :session)
        :status 302
        :headers {"Location" "/?msg=set"}})

   (GET "/test" request
        (friend/authenticated
         {:status 200
          :headers {"Content-Type" "text/html;charset=utf-8"}
          :body (html/page "Tests" (stest/show request (haz-admin)) request)}))

   (GET "/test" request
        {:status 200
         :headers {"Content-Type" "text/html;charset=utf-8"}
         :body (html/page "Tests" (stest/show request (haz-admin)) request)})

  (GET "/test/" request
       (friend/authorize #{::admin}
                         {:status 302
                          :headers {"Location" "/test"}}))

  (GET "/tests" request
       (friend/authorize #{::admin}
                         {:status 302
                          :headers {"Location" "/test"}}))
  (GET "/tests/" request
       (friend/authorize #{::admin}
                         {:status 302
                          :headers {"Location" "/test"}}))

  (GET "/test/new" request
       (friend/authorize #{::admin}
                         (let [test (:id (:route-params request))]
                           {:status 200
                            :headers {"Content-Type" "text/html;charset=utf-8"}
                            :body (html/page "Create a New Test" (stest/new-form request) request)})))

  (POST "/test/new" request
        (friend/authorize #{::admin}
                          (stest/new request)))

  (POST "/test/new/" request
        (friend/authorize #{::admin}
                          (stest/new request)))

   (GET "/test/:id" request
        (friend/authenticated
         (let [test (:id (:route-params request))]
           {:status 200
            :headers {"Content-Type" "text/html;charset=utf-8"}
            :body (html/page "Tests" (stest/show-one test (haz-admin)) request)})))

   (GET "/test/:id/edit" request
        (friend/authorize #{::admin}
         (let [test (:id (:route-params request))]
           {:status 200
            :headers {"Content-Type" "text/html;charset=utf-8"}
            :body (html/page "Tests" (stest/edit-one test) request)})))
   (POST "/test/:id/edit" request
         (friend/authorize #{::admin}
          (let [test (:id (:route-params request))]
            (let [result (stest/edit-all-from-form test (:form-params request))]
            {:status 302
             :headers {"Location" (str "/test/" test "?result=" (:message result))}}))))

   (GET "/test/:id/delete" request ;; simply redirect to try again.
         (friend/authorize #{::admin}
                           (let [test (:id (:route-params request))]
                             {:status 302
                              :headers {"Location" (str "/test/" test)}})))
   (POST "/test/:id/delete" request
         (friend/authorize #{::admin}
                           (let [test (:id (:route-params request))]
                             (let [result (stest/delete test)]
                               {:status 302
                                :headers {"Location" (str "/test?result=" (:message result))}}))))

   (GET "/test/:id/generate" request
        {:status 302
         :headers {"Location" (str "/test/" (:id (:route-params request)))}})

   (POST "/test/:id/generate" request
         (friend/authorize #{::admin}
                           (let [test (:id (:route-params request))
                                 group (get (:form-params request) "group")
                                 this-many (get (:form-params request) "num-questions")]
                             (let [result (stest/generate test group (Integer. this-many))]
                               {:status 302
                                :headers {"Location" (str "/test/" test "?result=" (:message result))}}))))

   (GET "/test/:id/rename" request
        {:status 302
         :headers {"Location" (str "/test/" (:id (:route-params request)))}})

   (POST "/test/:id/rename" request
         (friend/authorize #{::admin}
                           (let [test (:id (:route-params request))
                                 name (get (:form-params request) "name")]
                             (let [result (stest/rename test name)]
                               {:status 302
                                :headers {"Location" (str "/test/" test "?result=" (:message result))}}))))

   (GET "/test/:id/take" request
        (friend/authenticated
         (let [test (:id (:route-params request))]
           (stest/test-submit-form test request))))

   (POST "/test/:id/take" request
        (friend/authenticated
         (let [test (:id (:route-params request))]
           (let [result (tsubmit/new test
                                     (stest/group-by-question (:form-params request))
                                     (friend/current-authentication))]
             {:status 302
              :headers {"Location" (str "/test/" test "?result=" (:message result))}}))))
   
   (GET "/verb" request
        {:body (html/page 
                "Verbs" 
                (verb/select (session/request-to-session request) 
                             request
                             (haz-admin))
                request)
         :status 200
         :headers {"Content-Type" "text/html;charset=utf-8"}})
   (GET "/verb/" request
        {:status 302
         :headers {"Location" "/verb"}})
   
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
                                :headers {"Location" (str "/verb?result=" (:message result))}}))))

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

