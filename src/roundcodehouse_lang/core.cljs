(ns roundcodehouse-lang.core
  (:require [clojure.string :as str]
            [cljs.reader :as reader]
            [cljs.tools.reader :as tools-reader]
            [cljs.tools.reader.reader-types :as rt]))

(declare scheme-eval)

(def ^:private scheme-env
  (atom {'+ +
         '- -
         '* *
         '/ /
         '< <
         '> >
         '<= <=
         '>= >=
         '= =
         'eq? =
         'equal? =
         'not not
         'null? nil?
         'pair? seq?
         'list? list?
         'symbol? symbol?
         'number? number?
         'string? string?
         'boolean? boolean?
         'procedure? fn?
         'car first
         'cdr rest
         'cons cons
         'list list
         'append concat
         'length count
         'reverse reverse
         'map map
         'filter filter
         'reduce reduce
         'fold-left reduce
         'fold-right #(reduce %1 %3 (reverse %2))
         'abs #(js/Math.abs %)
         'max max
         'min min
         'sqrt #(js/Math.sqrt %)
         'expt #(js/Math.pow %1 %2)
         'floor #(js/Math.floor %)
         'ceiling #(js/Math.ceil %)
         'round #(js/Math.round %)
         'sin #(js/Math.sin %)
         'cos #(js/Math.cos %)
         'tan #(js/Math.tan %)
         'log #(js/Math.log %)
         'exp #(js/Math.exp %)
         'random #(js/Math.random)
         'display println
         'newline #(println)
         'write pr
         'read-string reader/read-string
         'string->number js/parseFloat
         'number->string str
         'string-append str
         'string-length count
         'substring subs
         'string=? =
         'string<? <
         'string>? >
         'string<=? <=
         'string>=? >=
         'char? char?
         'string->list seq
         'list->string #(apply str %)
         'vector vector
         'make-vector #(vec (repeat %1 %2))
         'vector-length count
         'vector-ref nth
         'vector-set! assoc!
         'vector->list vec
         'list->vector vec
         'apply apply
         'eval #(scheme-eval % @scheme-env)
         'load-string #(scheme-eval (reader/read-string %) @scheme-env)
         'error #(throw (js/Error. (str %)))
         'type type
         'deref deref
         'reset! reset!
         'swap! swap!}))

(defn- scheme-quote [expr]
  expr)

(defn- scheme-if [test then else env]
  (if (scheme-eval test env)
    (scheme-eval then env)
    (scheme-eval else env)))

(defn- scheme-cond [clauses env]
  (loop [clauses clauses]
    (when-let [clause (first clauses)]
      (let [test (first clause)
            exprs (rest clause)]
        (if (or (= test 'else) (scheme-eval test env))
          (last (map #(scheme-eval % env) exprs))
          (recur (rest clauses)))))))

(defn- scheme-and [exprs env]
  (loop [exprs exprs
         result true]
    (if (empty? exprs)
      result
      (let [val (scheme-eval (first exprs) env)]
        (if val
          (recur (rest exprs) val)
          false)))))

(defn- scheme-or [exprs env]
  (loop [exprs exprs]
    (if (empty? exprs)
      false
      (let [val (scheme-eval (first exprs) env)]
        (if val
          val
          (recur (rest exprs)))))))

(defn- scheme-begin [exprs env]
  (last (map #(scheme-eval % env) exprs)))

(defn- scheme-let [bindings body env]
  (let [names (map first bindings)
        values (map #(scheme-eval (second %) env) bindings)
        new-env (merge env (zipmap names values))]
    (scheme-eval body new-env)))

(defn- scheme-let* [bindings body env]
  (reduce (fn [env [name value]]
            (let [val (scheme-eval value env)]
              (assoc env name val)))
          env
          bindings)
  (scheme-eval body env))

(defn- scheme-letrec [bindings body env]
  (let [names (map first bindings)
        new-env (merge env (zipmap names (repeat nil)))
        values (map #(scheme-eval (second %) new-env) bindings)
        final-env (merge new-env (zipmap names values))]
    (scheme-eval body final-env)))

(defn- scheme-define [name value env]
  (let [val (scheme-eval value env)]
    (swap! scheme-env assoc name val)
    name))

(defn- scheme-set! [name value env]
  (let [val (scheme-eval value env)]
    (if (contains? env name)
      (swap! scheme-env assoc name val)
      (throw (js/Error. (str "Undefined variable: " name))))
    name))

(defn- scheme-lambda [params body env]
  (fn [& args]
    (let [arg-bindings (zipmap params args)
          new-env (merge env arg-bindings)]
      (scheme-eval body new-env))))

(defn- scheme-macro [params body env]
  (fn [& args]
    (let [arg-bindings (zipmap params args)
          new-env (merge env arg-bindings)
          expanded (scheme-eval body new-env)]
      (scheme-eval expanded env))))

(defn scheme-eval [expr env]
  (cond
    (symbol? expr)
    (if-let [val (get env expr)]
      val
      (throw (js/Error. (str "Undefined variable: " expr))))

    (not (seq? expr))
    expr

    :else
    (let [op (first expr)
          args (rest expr)]
      (case op
        quote (scheme-quote (first args))
        if (apply scheme-if (concat args [env]))
        cond (scheme-cond args env)
        and (scheme-and args env)
        or (scheme-or args env)
        begin (scheme-begin args env)
        let (scheme-let (first args) (second args) env)
        let* (scheme-let* (first args) (second args) env)
        letrec (scheme-letrec (first args) (second args) env)
        define (scheme-define (first args) (second args) env)
        set! (scheme-set! (first args) (second args) env)
        lambda (scheme-lambda (first args) (second args) env)
        macro (scheme-macro (first args) (second args) env)

        ;; Function application
        (let [proc (scheme-eval op env)
              evaluated-args (map #(scheme-eval % env) args)]
          (if (fn? proc)
            (apply proc evaluated-args)
            (throw (js/Error. (str "Not a procedure: " op)))))))))

(defn scheme-read [s]
  (try
    (reader/read-string s)
    (catch js/Error e
      (throw (js/Error. (str "Parse error: " (.-message e)))))))

(defn scheme-eval-string [s]
  (let [expr (scheme-read s)]
    (scheme-eval expr @scheme-env)))

(defn scheme-repl []
  (println "Welcome to the Scheme interpreter!")
  (println "Type :quit to exit")
  (letfn [(repl-loop []
            (let [input (js/prompt "scheme> ")]
              (when (and input (not= input ":quit"))
                (try
                  (let [result (scheme-eval-string input)]
                    (println result))
                  (catch js/Error e
                    (println "Error:" (.-message e))))
                (repl-loop))))]
    (repl-loop)))

(defn scheme-load [code]
  (let [exprs (try
                (let [reader (rt/string-push-back-reader code)]
                  (loop [exprs []]
                    (let [expr (try
                                 (tools-reader/read reader)
                                 (catch js/Error e
                                   (if (re-find #"EOF" (.-message e))
                                     ::eof
                                     (throw e))))]
                      (if (= expr ::eof)
                        exprs
                        (recur (conj exprs expr))))))
                (catch js/Error e
                  (throw (js/Error. (str "Parse error: " (.-message e))))))]
    (doseq [expr exprs]
      (scheme-eval expr @scheme-env))))

(defn reset-env! []
  (reset! scheme-env
          {'+ +
           '- -
           '* *
           '/ /
           '< <
           '> >
           '<= <=
           '>= >=
           '= =
           'eq? =
           'equal? =
           'not not
           'null? nil?
           'pair? seq?
           'list? list?
           'symbol? symbol?
           'number? number?
           'string? string?
           'boolean? boolean?
           'procedure? fn?
           'car first
           'cdr rest
           'cons cons
           'list list
           'append concat
           'length count
           'reverse reverse
           'map map
           'filter filter
           'reduce reduce
           'fold-left reduce
           'fold-right #(reduce %1 %3 (reverse %2))
           'abs #(js/Math.abs %)
           'max max
           'min min
           'sqrt #(js/Math.sqrt %)
           'expt #(js/Math.pow %1 %2)
           'floor #(js/Math.floor %)
           'ceiling #(js/Math.ceil %)
           'round #(js/Math.round %)
           'sin #(js/Math.sin %)
           'cos #(js/Math.cos %)
           'tan #(js/Math.tan %)
           'log #(js/Math.log %)
           'exp #(js/Math.exp %)
           'random #(js/Math.random)
           'display println
           'newline #(println)
           'write pr
           'read-string reader/read-string
           'string->number js/parseFloat
           'number->string str
           'string-append str
           'string-length count
           'substring subs
           'string=? =
           'string<? <
           'string>? >
           'string<=? <=
           'string>=? >=
           'char? char?
           'string->list seq
           'list->string #(apply str %)
           'vector vector
           'make-vector #(vec (repeat %1 %2))
           'vector-length count
           'vector-ref nth
           'vector-set! assoc!
           'vector->list vec
           'list->vector vec
           'apply apply
           'eval #(scheme-eval % @scheme-env)
           'load-string #(scheme-eval (reader/read-string %) @scheme-env)
           'error #(throw (js/Error. (str %)))
           'type type
           'deref deref
           'reset! reset!
           'swap! swap!}))

(defn get-env []
  @scheme-env)

(defn set-env! [env]
  (reset! scheme-env env))
