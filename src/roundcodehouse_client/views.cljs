(ns roundcodehouse-client.views
  (:require
   [roundcodehouse-client.card :refer [playing-card]]))

(defn main-panel []
  [:div {:class "flex flex-wrap gap-4 p-4 bg-green-700"} ; Simulate a table
   [playing-card :hearts :ace]
   [playing-card :diamonds 10]
   [playing-card :spades :queen]
   [playing-card :clubs 7]
   [playing-card :hearts :king]
   ;; Example of an invalid card (shows defaults)
   ; [playing-card :foo 99]
   ])

