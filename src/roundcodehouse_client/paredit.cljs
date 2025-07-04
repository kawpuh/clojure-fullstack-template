(ns roundcodehouse-client.paredit
  (:require [clojure.string :as str]))

;; Paredit-like functionality for Scheme REPL
;; Provides structural editing operations for S-expressions

(defn find-matching-paren
  "Find the matching closing paren for an opening paren at given position"
  [text start-pos]
  (loop [pos (inc start-pos)
         depth 1]
    (when (< pos (count text))
      (let [ch (nth text pos)]
        (cond
          (= ch \() (recur (inc pos) (inc depth))
          (= ch \)) (if (= depth 1)
                     pos
                     (recur (inc pos) (dec depth)))
          :else (recur (inc pos) depth))))))

(defn find-matching-open-paren
  "Find the matching opening paren for a closing paren at given position"
  [text end-pos]
  (loop [pos (dec end-pos)
         depth 1]
    (when (>= pos 0)
      (let [ch (nth text pos)]
        (cond
          (= ch \)) (recur (dec pos) (inc depth))
          (= ch \() (if (= depth 1)
                     pos
                     (recur (dec pos) (dec depth)))
          :else (recur (dec pos) depth))))))

(defn balanced-parens?
  "Check if parentheses are balanced in the text"
  [text]
  (loop [pos 0
         depth 0]
    (if (>= pos (count text))
      (= depth 0)
      (let [ch (nth text pos)]
        (cond
          (= ch \() (recur (inc pos) (inc depth))
          (= ch \)) (if (= depth 0)
                     false
                     (recur (inc pos) (dec depth)))
          :else (recur (inc pos) depth))))))

(defn find-sexp-bounds
  "Find the start and end positions of the S-expression at cursor position"
  [text cursor-pos]
  (let [before-cursor (subs text 0 cursor-pos)
        after-cursor (subs text cursor-pos)]
    ;; Find the nearest opening paren before cursor
    (loop [pos (dec cursor-pos)]
      (when (>= pos 0)
        (let [ch (nth text pos)]
          (if (= ch \()
            (if-let [end-pos (find-matching-paren text pos)]
              [pos end-pos]
              nil)
            (recur (dec pos))))))))

(defn wrap-with-parens
  "Wrap the current selection or S-expression with parentheses"
  [text cursor-pos selection-start selection-end]
  (if (and selection-start selection-end)
    ;; Wrap selection
    (let [before (subs text 0 selection-start)
          selected (subs text selection-start selection-end)
          after (subs text selection-end)]
      {:text (str before "(" selected ")")
       :cursor-pos (inc selection-end)})
    ;; Wrap current S-expression or insert empty parens
    (if-let [bounds (find-sexp-bounds text cursor-pos)]
      (let [[start end] bounds
            before (subs text 0 start)
            sexp (subs text start (inc end))
            after (subs text (inc end))]
        {:text (str before "(" sexp ")")
         :cursor-pos (inc (inc end))})
      ;; Insert empty parens
      (let [before (subs text 0 cursor-pos)
            after (subs text cursor-pos)]
        {:text (str before "()" after)
         :cursor-pos (inc cursor-pos)}))))

(defn slurp-forward
  "Slurp the next S-expression into the current one"
  [text cursor-pos]
  (when-let [bounds (find-sexp-bounds text cursor-pos)]
    (let [[start end] bounds
          after-sexp (subs text (inc end))
          ;; Find the next non-whitespace character
          next-sexp-start (loop [pos 0]
                           (when (< pos (count after-sexp))
                             (if (not (str/blank? (str (nth after-sexp pos))))
                               pos
                               (recur (inc pos)))))]
      (when next-sexp-start
        (let [abs-next-start (+ (inc end) next-sexp-start)]
          (if (= (nth text abs-next-start) \()
            ;; Next item is an S-expression
            (when-let [next-end (find-matching-paren text abs-next-start)]
              (let [before (subs text 0 end)
                    next-sexp (subs text abs-next-start (inc next-end))
                    after (subs text (inc next-end))]
                {:text (str before " " next-sexp ")" after)
                 :cursor-pos cursor-pos}))
            ;; Next item is an atom
            (let [atom-end (loop [pos abs-next-start]
                            (if (or (>= pos (count text))
                                   (str/blank? (str (nth text pos)))
                                   (contains? #{\( \)} (nth text pos)))
                              pos
                              (recur (inc pos))))
                  before (subs text 0 end)
                  atom (subs text abs-next-start atom-end)
                  after (subs text atom-end)]
              {:text (str before " " atom ")" after)
               :cursor-pos cursor-pos})))))))

(defn barf-forward
  "Barf the last S-expression out of the current one"
  [text cursor-pos]
  (when-let [bounds (find-sexp-bounds text cursor-pos)]
    (let [[start end] bounds
          sexp-content (subs text (inc start) end)
          ;; Find the last S-expression or atom
          trimmed (str/trim sexp-content)
          last-item-start (loop [pos (dec (count trimmed))]
                           (when (>= pos 0)
                             (let [ch (nth trimmed pos)]
                               (cond
                                 (= ch \)) ;; Find matching open paren
                                 (find-matching-open-paren trimmed pos)
                                 (str/blank? (str ch))
                                 (inc pos)
                                 :else (recur (dec pos))))))]
      (when (and last-item-start (> last-item-start 0))
        (let [before-sexp (subs text 0 (inc start))
              remaining-content (subs trimmed 0 last-item-start)
              last-item (subs trimmed last-item-start)
              after-sexp (subs text (inc end))]
          {:text (str before-sexp (str/trim remaining-content) ")" " " last-item after-sexp)
           :cursor-pos cursor-pos})))))

(defn delete-sexp
  "Delete the current S-expression"
  [text cursor-pos]
  (when-let [bounds (find-sexp-bounds text cursor-pos)]
    (let [[start end] bounds
          before (subs text 0 start)
          after (subs text (inc end))]
      {:text (str before after)
       :cursor-pos start})))

(defn delete-paren-pair
  "Delete a matching pair of parentheses"
  [text open-pos close-pos]
  (let [before (subs text 0 open-pos)
        middle (subs text (inc open-pos) close-pos)
        after (subs text (inc close-pos))]
    {:text (str before middle after)
     :cursor-pos open-pos}))

(defn handle-delete-paren
  "Handle deletion of parentheses while maintaining structure"
  [text cursor-pos delete-forward?]
  (let [target-pos (if delete-forward? cursor-pos (dec cursor-pos))]
    (when (and (>= target-pos 0) (< target-pos (count text)))
      (let [ch (nth text target-pos)]
        (cond
          ;; Deleting an opening paren
          (= ch \()
          (when-let [matching-pos (find-matching-paren text target-pos)]
            (delete-paren-pair text target-pos matching-pos))

          ;; Deleting a closing paren
          (= ch \))
          (when-let [matching-pos (find-matching-open-paren text target-pos)]
            (delete-paren-pair text matching-pos target-pos))

          :else nil)))))

(defn handle-backspace
  "Handle backspace key with paredit behavior"
  [text cursor-pos]
  (cond
    ;; At beginning of text
    (<= cursor-pos 0)
    nil

    ;; Check if we're deleting a paren
    :else
    (let [prev-char (nth text (dec cursor-pos))]
      (if (contains? #{\( \)} prev-char)
        (handle-delete-paren text cursor-pos false)
        ;; Normal backspace
        (let [before (subs text 0 (dec cursor-pos))
              after (subs text cursor-pos)]
          {:text (str before after)
           :cursor-pos (dec cursor-pos)})))))

(defn handle-delete
  "Handle delete key with paredit behavior"
  [text cursor-pos]
  (cond
    ;; At end of text
    (>= cursor-pos (count text))
    nil

    ;; Check if we're deleting a paren
    :else
    (let [current-char (nth text cursor-pos)]
      (if (contains? #{\( \)} current-char)
        (handle-delete-paren text cursor-pos true)
        ;; Normal delete
        (let [before (subs text 0 cursor-pos)
              after (subs text (inc cursor-pos))]
          {:text (str before after)
           :cursor-pos cursor-pos})))))

(defn move-to-next-sexp
  "Move cursor to the beginning of the next S-expression"
  [text cursor-pos]
  (let [remaining (subs text cursor-pos)]
    (loop [pos 0]
      (when (< pos (count remaining))
        (let [ch (nth remaining pos)]
          (if (= ch \()
            (+ cursor-pos pos)
            (recur (inc pos))))))))

(defn move-to-prev-sexp
  "Move cursor to the beginning of the previous S-expression"
  [text cursor-pos]
  (let [before (subs text 0 cursor-pos)]
    (loop [pos (dec (count before))]
      (when (>= pos 0)
        (let [ch (nth before pos)]
          (if (= ch \()
            pos
            (recur (dec pos))))))))

(defn handle-paredit-key
  "Handle paredit keyboard shortcuts"
  [text cursor-pos selection-start selection-end key-event]
  (let [key (.-key key-event)
        ctrl? (.-ctrlKey key-event)
        alt? (.-altKey key-event)
        shift? (.-shiftKey key-event)]
    (cond
      ;; Ctrl+Shift+0 - wrap with parens
      (and ctrl? shift? (= key "0"))
      (wrap-with-parens text cursor-pos selection-start selection-end)

      ;; Ctrl+Right - slurp forward
      (and ctrl? (= key "ArrowRight"))
      (slurp-forward text cursor-pos)

      ;; Ctrl+Left - barf forward
      (and ctrl? (= key "ArrowLeft"))
      (barf-forward text cursor-pos)

      ;; Ctrl+Shift+K - delete S-expression
      (and ctrl? shift? (= key "K"))
      (delete-sexp text cursor-pos)

      ;; Alt+Right - move to next S-expression
      (and alt? (= key "ArrowRight"))
      {:cursor-pos (or (move-to-next-sexp text cursor-pos) cursor-pos)}

      ;; Alt+Left - move to previous S-expression
      (and alt? (= key "ArrowLeft"))
      {:cursor-pos (or (move-to-prev-sexp text cursor-pos) cursor-pos)}

      ;; Auto-close parens
      (= key "(")
      (let [before (subs text 0 cursor-pos)
            after (subs text cursor-pos)]
        {:text (str before "()" after)
         :cursor-pos (inc cursor-pos)})

      ;; Skip over closing paren if it's the next character
      (and (= key ")") (< cursor-pos (count text)) (= (nth text cursor-pos) \)))
      {:cursor-pos (inc cursor-pos)}

      ;; Handle backspace
      (= key "Backspace")
      (handle-backspace text cursor-pos)

      ;; Handle delete
      (= key "Delete")
      (handle-delete text cursor-pos)

      :else nil)))
