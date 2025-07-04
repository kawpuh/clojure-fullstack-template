(ns roundcodehouse-client.views
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [roundcodehouse-lang.core :as scheme]))

(defn scheme-repl-component []
  (let [input (r/atom "")
        output (r/atom "Welcome to Scheme REPL! Try: (+ 1 2)\n")
        history (r/atom [])]
    (fn []
      [:div {:style {:font-family "monospace"
                     :max-width "800px"
                     :margin "20px auto"
                     :padding "20px"
                     :border "1px solid #ccc"
                     :border-radius "5px"}}
       [:h2 "Scheme REPL"]
       [:div {:style {:background "#f5f5f5"
                      :padding "10px"
                      :border-radius "3px"
                      :margin-bottom "10px"
                      :min-height "200px"
                      :white-space "pre-wrap"
                      :overflow-y "auto"}}
        @output]
       [:div {:style {:display "flex"
                      :gap "10px"}}
        [:input {:type "text"
                 :value @input
                 :placeholder "Enter Scheme expression..."
                 :style {:flex "1"
                         :padding "8px"
                         :font-family "monospace"
                         :border "1px solid #ccc"
                         :border-radius "3px"}
                 :on-change #(reset! input (-> % .-target .-value))
                 :on-key-press #(when (= (.-key %) "Enter")
                                  (let [expr @input]
                                    (when-not (empty? expr)
                                      (swap! history conj expr)
                                      (swap! output str "scheme> " expr "\n")
                                      (try
                                        (let [result (scheme/scheme-eval-string expr)]
                                          (swap! output str result "\n"))
                                        (catch js/Error e
                                          (swap! output str "Error: " (.-message e) "\n")))
                                      (reset! input ""))))}]
        [:button {:on-click #(let [expr @input]
                               (when-not (empty? expr)
                                 (swap! history conj expr)
                                 (swap! output str "scheme> " expr "\n")
                                 (try
                                   (let [result (scheme/scheme-eval-string expr)]
                                     (swap! output str result "\n"))
                                   (catch js/Error e
                                     (swap! output str "Error: " (.-message e) "\n")))
                                 (reset! input "")))
                  :style {:padding "8px 16px"
                          :background "#007acc"
                          :color "white"
                          :border "none"
                          :border-radius "3px"
                          :cursor "pointer"}}
         "Eval"]
        [:button {:on-click #(reset! output "REPL cleared.\n")
                  :style {:padding "8px 16px"
                          :background "#666"
                          :color "white"
                          :border "none"
                          :border-radius "3px"
                          :cursor "pointer"}}
         "Clear"]]
       [:div {:style {:margin-top "10px"
                      :font-size "12px"
                      :color "#666"}}
        "Press Enter or click Eval to execute. Available functions: +, -, *, /, <, >, =, cons, car, cdr, list, define, lambda, if, cond, and, or, begin, let, map, filter, and more!"]])))

(defn main-panel []
  [:div
   [scheme-repl-component]])

