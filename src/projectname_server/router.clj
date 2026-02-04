(ns projectname-server.router
  (:require
   [reitit.ring :as ring]
   [reitit.ring.middleware.dev]
   [ring.util.response :as ring-response]
   [ring.middleware.reload :refer [wrap-reload]]
   [projectname-server.config :as config]))

(defn create-app []
  (ring/ring-handler
   (ring/router
    [["/" {:get {:middleware []
                 :handler (fn [req] (ring-response/file-response "public/resources/index.html"))}}]
     ["/resources/*" {:get {:handler (ring/create-file-handler {:root "public/resources"})}}]
     {:reitit.middleware/transform (if (and config/debug? config/middleware-diff-logging)
                                     reitit.ring.middleware.dev/print-request-diffs
                                     identity)
      :data {:middleware [(when config/debug? {:name :debug-reload-middleware
                                               :wrap wrap-reload})]}}])
   (ring/routes
    (ring/redirect-trailing-slash-handler)
    (ring/create-default-handler
     {:not-found (constantly {:status 404 :body "404 - Not found"})}))))
