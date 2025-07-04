(ns shufflefast-client.card
  (:require
   [reagent.core :as r]
   [clojure.string :as str]))

(def suit-details
  "Map suits to their symbol and Tailwind color class."
  {:hearts   {:symbol "♥" :color "text-red-600"}
   :diamonds {:symbol "♦" :color "text-red-600"}
   :clubs    {:symbol "♣" :color "text-black"}
   :spades   {:symbol "♠" :color "text-black"}})

(def rank-details
  "Map ranks (keywords or numbers) to their display string."
  {:ace   "A"
   2      "2"
   3      "3"
   4      "4"
   5      "5"
   6      "6"
   7      "7"
   8      "8"
   9      "9"
   10     "10"
   :jack  "J"
   :queen "Q"
   :king  "K"})

;; --- Helper Functions ---

(defn- get-suit-detail [suit key default]
  (get-in suit-details [suit key] default))

(defn- get-rank-display [rank]
  (get rank-details rank "?"))

(defn- card-name
  "Creates a human-readable card name, e.g., 'Ace of Spades'."
  [suit rank]
  (let [rank-name (if (keyword? rank)
                    (str/capitalize (name rank))
                    (str rank))
        suit-name (str/capitalize (name suit))]
    (str rank-name " of " suit-name)))

;; --- Drag and Drop Component ---

(defn playing-card
  "A draggable playing card component."
  [suit rank on-drop]
  (let [dragging? (r/atom false)
        drag-start-mouse-pos (r/atom {:x 0 :y 0})
        offset (r/atom {:x 0 :y 0})
        animate-back? (r/atom false)

        handle-drag-move (fn [event]
                           (when @dragging?
                             (.preventDefault event)
                             (let [touch (first (.-touches event))
                                   current-x (if touch (.-clientX touch) (.-clientX event))
                                   current-y (if touch (.-clientY touch) (.-clientY event))
                                   start @drag-start-mouse-pos]
                               (when (and current-x current-y start)
                                 (reset! offset {:x (- current-x (:x start))
                                                 :y (- current-y (:y start))})))))

        handle-drag-end (fn handle-drag-end [event]
                          (when @dragging?
                            (when (some? (.-preventDefault event))
                              (.preventDefault event))

                            (reset! dragging? false)

                            (.removeEventListener js/window "mousemove" handle-drag-move)
                            (.removeEventListener js/window "mouseup" handle-drag-end)
                            (.removeEventListener js/window "touchmove" handle-drag-move)
                            (.removeEventListener js/window "touchend" handle-drag-end)
                            (set! (.-userSelect js/document.body.style) "")

                            (if (and (some? on-drop) (on-drop suit rank))
                              (println "Drop accepted for" (card-name suit rank))
                              (do
                                (println "Drop rejected for" (card-name suit rank))
                                (reset! animate-back? true)
                                (js/setTimeout
                                 #(do
                                    (reset! offset {:x 0 :y 0})
                                    (js/setTimeout (fn [] (reset! animate-back? false)) 300))
                                 0)))))

        handle-drag-start (fn [event]
                            (.preventDefault event)
                            (set! (.-userSelect js/document.body.style) "none")

                            (reset! dragging? true)
                            (reset! animate-back? false)
                            (reset! offset {:x 0 :y 0})

                            (let [touch (first (.-touches event))
                                  start-x (if touch (.-clientX touch) (.-clientX event))
                                  start-y (if touch (.-clientY touch) (.-clientY event))]
                              (reset! drag-start-mouse-pos {:x start-x :y start-y}))

                            (.addEventListener js/window "mousemove" handle-drag-move)
                            (.addEventListener js/window "mouseup" handle-drag-end)
                            (.addEventListener js/window "touchmove" handle-drag-move {:passive false})
                            (.addEventListener js/window "touchend" handle-drag-end))]

    (fn [suit rank on-drop]
      (let [color-class (get-suit-detail suit :color "text-black")
            suit-symbol (get-suit-detail suit :symbol "?")
            rank-display (get-rank-display rank)
            readable-name (card-name suit rank)
            current-offset @offset
            is-dragging @dragging?
            is-animating-back @animate-back?

            style {:transform (str "translate(" (:x current-offset) "px, "
                                   (:y current-offset) "px)")
                   :transition (when is-animating-back "transform 0.3s ease-out")
                   :z-index (when is-dragging 1000)
                   :cursor (if is-dragging "grabbing" "grab")
                   :touch-action "none"}]

        [:div
         {:title readable-name
          :class ["bg-white rounded-lg shadow-md"
                  "w-24 h-36"
                  "px-2 py-1 border border-gray-300"
                  "flex flex-col justify-between"
                  "font-sans font-bold"
                  "select-none"
                  "box-border"
                  color-class
                  (when is-dragging "shadow-xl cursor-grabbing")
                  (when (not is-dragging) "cursor-grab")]
          :style style
          :on-mouse-down handle-drag-start
          :on-touch-start handle-drag-start}

         [:div {:class "flex justify-between items-start"}
          [:div {:class "flex flex-col items-center"}
           [:div {:class "text-xl leading-none"} rank-display]
           [:div {:class "text-lg leading-none"} suit-symbol]]]
         [:div {:class "flex justify-end"}
          [:div {:class "flex flex-col items-center"}
           [:div {:class "text-xl leading-none"} rank-display]
           [:div {:class "text-lg leading-none"} suit-symbol]]]]))))
