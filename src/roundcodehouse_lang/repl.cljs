(ns roundcodehouse-lang.repl
  (:require [roundcodehouse-lang.core :as scheme]))

(defn start-repl []
  (scheme/scheme-repl))

(defn eval-scheme [input]
  (try
    (scheme/scheme-eval-string input)
    (catch js/Error e
      (str "Error: " (.-message e)))))

(defn ^:export init []
  (println "Scheme interpreter loaded!")
  (js/console.log "Use (js/roundcodehouse_lang.repl.eval_scheme \"(+ 1 2)\") to evaluate Scheme expressions")
  (js/console.log "Use (js/roundcodehouse_lang.repl.start_repl) to start the REPL"))

(defn ^:export eval-scheme-js [input]
  (eval-scheme input))

(defn ^:export start-repl-js []
  (start-repl))

(set! js/window.schemeEval eval-scheme-js)
(set! js/window.schemeRepl start-repl-js)