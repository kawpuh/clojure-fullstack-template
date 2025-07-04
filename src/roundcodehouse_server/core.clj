(ns roundcodehouse-server.core
  (:require
   [clojure.tools.cli :as cli]
   [org.httpkit.server :as http]
   [roundcodehouse-server.config :as config]
   [roundcodehouse-server.router :as router]
   [taoensso.telemere :as t]))

(defn parse-port [port-arg]
  (cond
    (integer? port-arg) port-arg
    (string? port-arg) (Integer/parseInt port-arg)
    :else (throw (IllegalArgumentException. "Port must be an integer or string"))))

(def cli-options
  [["-p" "--port PORT" "Port number"
    :default 8070
    :parse-fn parse-port
    :validate [#(< 0 % 65536) "Must be a number between 0 and 65536"]]])

(defn -main [& args]
  (let [{:keys [options errors]} (cli/parse-opts args cli-options)]
    (when errors
      (t/log! {:level :fatal
               :id :cli-parse-opts-error
               :data {:errors errors}})
      (System/exit 1))
    (t/log! {:level :info
             :id :announce-main-options
             :data options})
    (when config/debug?
      (t/set-min-level! :debug))
    (http/run-server
     (router/create-app)
     {:port (:port options)})))
