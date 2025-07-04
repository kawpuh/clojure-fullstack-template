(ns roundcodehouse-lang.parser
  (:require [cljs.reader :as reader]
            [cljs.tools.reader :as tools-reader]
            [cljs.tools.reader.reader-types :as rt]
            [clojure.string :as str]))

(defn- normalize-scheme-syntax
  "Convert common Scheme syntax to ClojureScript-compatible forms"
  [s]
  (-> s
      (str/replace #"#t" "true")
      (str/replace #"#f" "false")
      (str/replace #"#\\" "\\")
      (str/replace #"'([^()\s]+)" "(quote $1)")
      (str/replace #"'(\([^)]*\))" "(quote $1)")))

(defn parse-scheme-expression
  "Parse a single Scheme expression using ClojureScript's reader"
  [s]
  (try
    (-> s
        normalize-scheme-syntax
        reader/read-string)
    (catch js/Error e
      (throw (js/Error. (str "Parse error: " (.-message e)))))))

(defn parse-scheme-program
  "Parse multiple Scheme expressions from a string"
  [s]
  (try
    (let [normalized (normalize-scheme-syntax s)
          reader (rt/string-push-back-reader normalized)]
      (loop [exprs []]
        (let [expr (try
                     (tools-reader/read reader)
                     (catch js/Error e
                       (when-not (str/includes? (.-message e) "EOF")
                         (throw e))
                       ::eof))]
          (if (= expr ::eof)
            exprs
            (recur (conj exprs expr))))))
    (catch js/Error e
      (throw (js/Error. (str "Parse error: " (.-message e)))))))

(defn scheme-syntax?
  "Check if an expression uses Scheme-specific syntax"
  [expr]
  (cond
    (symbol? expr)
    (let [name (name expr)]
      (or (str/starts-with? name "#")
          (str/includes? name "?")))

    (seq? expr)
    (let [op (first expr)]
      (contains? #{'define 'lambda 'let 'let* 'letrec 'cond 'case 'do
                   'delay 'force 'call-with-current-continuation 'call/cc}
                 op))

    :else false))

(defn pretty-print-scheme
  "Pretty print a Scheme expression"
  [expr]
  (cond
    (true? expr) "#t"
    (false? expr) "#f"
    (nil? expr) "()"
    (symbol? expr) (name expr)
    (string? expr) (pr-str expr)
    (number? expr) (str expr)
    (seq? expr) (str "(" (str/join " " (map pretty-print-scheme expr)) ")")
    (vector? expr) (str "[" (str/join " " (map pretty-print-scheme expr)) "]")
    :else (str expr)))

(defn validate-scheme-syntax
  "Validate that a parsed expression has valid Scheme syntax"
  [expr]
  (cond
    (symbol? expr) true
    (or (number? expr) (string? expr) (boolean? expr) (nil? expr)) true

    (seq? expr)
    (let [op (first expr)]
      (case op
        (quote) (= (count expr) 2)
        (if) (or (= (count expr) 3) (= (count expr) 4))
        (define) (and (>= (count expr) 3)
                      (or (symbol? (second expr))
                          (and (seq? (second expr))
                               (symbol? (first (second expr))))))
        (lambda) (and (>= (count expr) 3)
                      (or (seq? (second expr))
                          (symbol? (second expr))))
        (let let* letrec) (and (>= (count expr) 3)
                               (seq? (second expr))
                               (every? #(and (seq? %) (= (count %) 2))
                                       (second expr)))
        (cond) (every? #(and (seq? %) (>= (count %) 2)) (rest expr))
        (and or begin) true
        true))

    (vector? expr) (every? validate-scheme-syntax expr)
    :else false))

(defn scheme-to-cljs
  "Convert Scheme expression to ClojureScript-compatible form"
  [expr]
  (cond
    (symbol? expr)
    (let [name (name expr)]
      (cond
        (str/ends-with? name "?") (symbol name)
        (str/ends-with? name "!") (symbol name)
        :else expr))

    (seq? expr)
    (let [op (first expr)]
      (case op
        lambda (cons 'fn (rest expr))
        expr))

    :else expr))

(defn read-scheme-file
  "Read and parse a Scheme file"
  [filename]
  (try
    (let [content (slurp filename)]
      (parse-scheme-program content))
    (catch js/Error e
      (throw (js/Error. (str "Error reading file " filename ": " (.-message e)))))))

(defn scheme-comment?
  "Check if a line is a Scheme comment"
  [line]
  (let [trimmed (str/trim line)]
    (or (str/starts-with? trimmed ";")
        (str/starts-with? trimmed "#|")
        (str/ends-with? trimmed "|#")
        (str/blank? trimmed))))

(defn remove-scheme-comments
  "Remove Scheme-style comments from code"
  [code]
  (-> code
      (str/replace #";[^\n]*" "")
      (str/replace #"#\|[\s\S]*?\|#" "")
      str/trim))

(defn tokenize-scheme
  "Tokenize a Scheme string into tokens"
  [s]
  (let [s (remove-scheme-comments s)]
    (-> s
        (str/replace #"[()]" " $& ")
        (str/replace #"[']" " $& ")
        (str/split #"\s+")
        (->> (filter #(not (str/blank? %)))))))

(defn scheme-special-form?
  "Check if a symbol is a Scheme special form"
  [symbol]
  (contains? #{'quote 'if 'define 'lambda 'let 'let* 'letrec 'cond 'case 'and 'or 'begin
               'do 'delay 'force 'call-with-current-continuation 'call/cc 'set! 'macro}
             symbol))
