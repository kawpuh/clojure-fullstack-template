(ns roundcodehouse-client.core
  (:require
   [reagent.dom :as rdom]
   [re-frame.core :as rf]
   [roundcodehouse-client.views :as views]
   [roundcodehouse-client.config :as config]))

(rf/reg-fx :call
  (fn [[f & args]]
    (apply f args)))

(rf/reg-sub :get
  (fn [db [_ k default]]
    (if (vector? k)
      (get-in db k default)
      (get db k default))))

(rf/reg-event-db :assoc
  (fn [db [_ path value]]
    (if (vector? path)
      (assoc-in db path value)
      (assoc db path value))))

(rf/reg-event-db :update
  (fn [db [_ path f & args]]
    (if (vector? path)
      (apply update-in db path f args)
      (apply update db path f args))))

(defn dev-setup []
  (when config/debug?
    (println "dev mode")))

(defn ^:dev/after-load mount-root []
  (rf/clear-subscription-cache!)
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [views/main-panel] root-el)))

(defn init []
  (dev-setup)
  (mount-root))
