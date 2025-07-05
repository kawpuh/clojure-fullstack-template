(ns roundcodehouse-client.views
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [roundcodehouse-lang.core :as scheme]
            [roundcodehouse-client.paredit :as paredit]))

(defn make-apply-paredit-fn [input cursor-pos input-ref]
  (fn [action-fn & args]
    (let [result (apply action-fn @input @cursor-pos args)]
      (cond
        ;; Handle map results (text modifications)
        (map? result)
        (do
          (reset! input (:text result))
          (when (:cursor-pos result)
            (reset! cursor-pos (:cursor-pos result)))
          (js/setTimeout
            (fn []
              (when @input-ref
                (.focus @input-ref)
                (let [pos (or (:cursor-pos result) @cursor-pos)]
                  (set! (.-selectionStart @input-ref) pos)
                  (set! (.-selectionEnd @input-ref) pos))))
            0))
        ;; Handle direct position results (cursor movements)
        (number? result)
        (do
          (reset! cursor-pos result)
          (js/setTimeout
            (fn []
              (when @input-ref
                (.focus @input-ref)
                (set! (.-selectionStart @input-ref) result)
                (set! (.-selectionEnd @input-ref) result)))
            0))
        ;; Always focus even if no result
        :else
        (js/setTimeout
          (fn []
            (when @input-ref
              (.focus @input-ref)))
          0)))))

(defn scheme-repl-component []
  (let [input (r/atom "")
        output (r/atom "Welcome to Scheme REPL! Try: (+ 1 2)\n")
        history (r/atom [])
        cursor-pos (r/atom 0)
        input-ref (r/atom nil)
        apply-paredit (make-apply-paredit-fn input cursor-pos input-ref)]
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
       ;; Paredit button bar for mobile
       [:div {:style {:display "flex"
                      :flex-wrap "wrap"
                      :gap "5px"
                      :margin-bottom "10px"
                      :padding "8px"
                      :background "#f9f9f9"
                      :border-radius "3px"
                      :border "1px solid #ddd"}}
        [:button {:on-click #(apply-paredit paredit/wrap-with-parens nil nil)
                  :style {:padding "4px 8px"
                          :background "#4CAF50"
                          :color "white"
                          :border "none"
                          :border-radius "3px"
                          :cursor "pointer"
                          :font-size "12px"}}
         "()"]
        [:button {:on-click #(apply-paredit paredit/slurp-forward)
                  :style {:padding "4px 8px"
                          :background "#2196F3"
                          :color "white"
                          :border "none"
                          :border-radius "3px"
                          :cursor "pointer"
                          :font-size "12px"}}
         "→)"]
        [:button {:on-click #(apply-paredit paredit/barf-forward)
                  :style {:padding "4px 8px"
                          :background "#FF9800"
                          :color "white"
                          :border "none"
                          :border-radius "3px"
                          :cursor "pointer"
                          :font-size "12px"}}
         "(←"]
        [:button {:on-click #(apply-paredit paredit/delete-sexp)
                  :style {:padding "4px 8px"
                          :background "#f44336"
                          :color "white"
                          :border "none"
                          :border-radius "3px"
                          :cursor "pointer"
                          :font-size "12px"}}
         "Del"]
        [:button {:on-click #(apply-paredit paredit/move-to-prev-sexp)
                  :style {:padding "4px 8px"
                          :background "#9C27B0"
                          :color "white"
                          :border "none"
                          :border-radius "3px"
                          :cursor "pointer"
                          :font-size "12px"}}
         "←"]
        [:button {:on-click #(apply-paredit paredit/move-to-next-sexp)
                  :style {:padding "4px 8px"
                          :background "#9C27B0"
                          :color "white"
                          :border "none"
                          :border-radius "3px"
                          :cursor "pointer"
                          :font-size "12px"}}
         "→"]]
       [:div {:style {:display "flex"
                      :gap "10px"}}
        [:input {:type "text"
                 :value @input
                 :placeholder "Enter Scheme expression... (Ctrl+Shift+0: wrap parens, Ctrl+←/→: barf/slurp, Alt+←/→: move by sexp)"
                 :style {:flex "1"
                         :padding "8px"
                         :font-family "monospace"
                         :border "1px solid #ccc"
                         :border-radius "3px"}
                 :ref #(reset! input-ref %)
                 :on-change #(do
                               (reset! input (-> % .-target .-value))
                               (reset! cursor-pos (-> % .-target .-selectionStart)))
                 :on-click #(reset! cursor-pos (-> % .-target .-selectionStart))
                 :on-key-up #(reset! cursor-pos (-> % .-target .-selectionStart))
                 :on-key-down #(let [paredit-result (paredit/handle-paredit-key
                                                     @input
                                                     @cursor-pos
                                                     nil nil %)]
                                 (when paredit-result
                                   (.preventDefault %)
                                   (when (:text paredit-result)
                                     (reset! input (:text paredit-result)))
                                   (when (:cursor-pos paredit-result)
                                     (reset! cursor-pos (:cursor-pos paredit-result))
                                     (js/setTimeout
                                       (fn []
                                         (when @input-ref
                                           (set! (.-selectionStart @input-ref) (:cursor-pos paredit-result))
                                           (set! (.-selectionEnd @input-ref) (:cursor-pos paredit-result))))
                                       0))))
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
                                      (reset! input "")
                                      (reset! cursor-pos 0))))}]
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
        "Press Enter or click Eval to execute. Available functions: +, -, *, /, <, >, =, cons, car, cdr, list, define, lambda, if, cond, and, or, begin, let, map, filter, and more!"]
       [:div {:style {:margin-top "5px"
                      :font-size "11px"
                      :color "#888"}}
        "Paredit keys: Ctrl+Shift+0 (wrap parens), Ctrl+← (barf), Ctrl+→ (slurp), Alt+←/→ (move by sexp), Ctrl+Shift+K (delete sexp), ( auto-closes"]])))

(defn main-panel []
  [:div
   [scheme-repl-component]])

