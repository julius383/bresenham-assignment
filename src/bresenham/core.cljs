(ns bresenham.core
  (:require [quil.core :as q]
            [quil.middleware :as middleware]))

(enable-console-print!)

(def body (.-body js/document))

(defn create-plane
  [xdim ydim]
  (into []
        (for [_ (range ydim)]
          (into [] (take xdim (repeat 0))))))

(defn index-plane
  "Convert cartesian position to position on our plane"
  [[x y] plane]
  (let [x-points (count (first plane))
        y-points (count plane)]
    [(+ (Math/floor (/ x-points 2)) x)
     (dec (- (Math/floor (/ y-points 2)) y))]))

(defn reverse-index-plane
  "Convert plane position to a Cartesian position"
  [[x-idx y-idx] plane]
  [(- x-idx (Math/floor (/ (count (first plane)) 2)))
   (- (Math/floor (/ (count plane) 2)) y-idx)])

(defn plane-pos
  "Convert mouse points to plane position"
  [mouse-x mouse-y x-size y-size]
  [(int (/ mouse-x x-size)) (inc (int (/ mouse-y y-size)))])

(defn update-plane
  "Turns on pixel on plane corresponding to pos"
  [pos plane]
  (for [y (range (count plane))]
    (for [x (range (count (first plane)))]
      (if (or
           (pos? (nth (nth plane y) x))
           (= [x y] (index-plane pos plane)))
        1
        0))))

(defn reset-plane [plane]
  (into [] (for [_ (range (count plane))]
             (into [] (for [_ (range (count (nth plane 0)))]
                        0)))))
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

(defn sketch-setup []
  (q/frame-rate 30)
  {:plane    (create-plane 40 40)     ; each item represents 'pixel' with 0 being off and 1 on
   :line     (bresenhams [0 1] [6 4]) ; stores points for bresenhams
   :running? true                     ; set to false to stop animation
   :counter  0                        ; keeps track of currently drawing point on :line
   })

(defn sketch-update [state]
  (if (:running? state)
    (if (< (:counter state) (count (:line state)))
      (let [cur (nth (:line state) (:counter state))]
        (-> state
            (assoc :plane (update-plane cur (:plane state)))
            (update :counter inc)))
      (-> state
          (assoc :counter 0)
          (assoc :plane (reset-plane (:plane state)))))
    state))

(defn mouse-clicked
  "Updates the state of the line to be drawn. It will change to plot from
  the last coordinate to the clicked cell"
  [state]
  (let [x-size   (/ (q/width) (count (first (:plane state))))
        y-size   (/ (q/height) (count (:plane state)))
        last-pos (last (:line state))
        cur-pos  (plane-pos (q/mouse-x) (q/mouse-y) x-size y-size)]
    (assoc state :plane (reset-plane (:plane state)))
    (assoc state :line (bresenhams last-pos (reverse-index-plane cur-pos (:plane state))))))

(defn draw-axes []
  (q/stroke-weight 2)
  (let [half-height (Math/floor (/ (q/height) 2))
        half-width  (Math/floor (/ (q/width) 2))]
    (q/stroke 255 0 0)
    (q/line [0 half-height] [(q/width) half-height])
    (q/line [half-width 0] [half-width (q/height)])))

(defn draw-points [state]
  (q/stroke-weight 1)
  (q/stroke 0 0 0)
  (let [points       (:plane state)
        point-height (/ (q/height) (count points))
        point-width  (/ (q/width) (count (nth points 0)))]
    (doseq [i (range (count points))]
      (doseq [j (range (count (nth points 0)))]
        (if (pos? (nth (nth points i) j))
          (q/fill 0 255 0)
          (q/fill 255 255 255))
        (q/rect (* j point-width) (* i point-height)
                point-width point-height)))))

(defn sketch-draw [state]
  (q/background 240)
  (draw-points state)
  (draw-axes))

(defn create [canvas]
  (q/sketch
   :host canvas
   :size [1000 800]
   :draw #'sketch-draw
   :setup #'sketch-setup
   :update #'sketch-update
   :mouse-clicked #'mouse-clicked
   :middleware [middleware/fun-mode]
   :settings (fn []
               (q/random-seed 666)
               (q/noise-seed 666))))

(defonce sketch (create "sketch"))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )

