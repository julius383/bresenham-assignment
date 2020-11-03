(ns bresenham.core
  (:require ))

(enable-console-print!)

(println "This text is printed from src/bresenham/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"}))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )

(defn sign
  "Returns the sign of the integer. 0 if it is a zero, -1 for negative numbers and 1 for other natural numbers"
  [x]
  (cond
    (< x 0)   -1
    (zero? x) 0
    (> x 0)   1))

(defn bresenhams
  "Returns a 2-tuple list of points between any 2 coords (start and end)"
  [start end]
  (let [[x1 y1] start [x2 y2] end
        dx      (Math/abs (- x2 x1))
        dy      (Math/abs (- y2 y1))
        x-diff  (sign (- x2 x1))
        y-diff  (sign (- y2 y1))
        change? (> dy dx)
        p       (if change? (- (* 2 dx) dy) (- (* 2 dy) dx))
        a       (if change?  (* 2 dx) (* 2 dy))
        b       (if change?  (* 2 (- dx dy)) (* 2 (- dy dx)))]
    (->> (range 0 (if change? dy dx))
         (reduce
           (fn [{p :p [x-prev y-prev] :prev coords :coords} _]
             (if (< p 0)
               (if change?
                 {:p (+ p a) :prev [x-prev (+ y-prev y-diff)] :coords (conj coords  [x-prev (+ y-prev y-diff)])}
                 {:p (+ p a) :prev [(+ x-prev x-diff) y-prev] :coords (conj coords [(+ x-prev x-diff) y-prev])})
               (let [x (+ x-prev x-diff) y (+ y-prev y-diff)]
                 {:p      (+ p b)
                  :prev   [x y]
                  :coords (conj coords [x y])})))
           {:p p :prev [x1 y1] :coords [[x1 y1]]})
         (:coords))))
