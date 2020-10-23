(ns autopeli
  (:require [clojure.math.numeric-tower :as math]
            [clojure.string :as s]
            [complex.core :as c]
            [quil.core :as q]
            [quil.middleware :as qm]))

(def size [500 500])
(def road-width 70)
(def r-w-h (/ road-width 2))
(def car-width 10)
(def car-height 30)
(def rotation-val 0.03)
(def offroad-max-speed 0.8)
(def acc-val 0.05)
(def change-val 0.1)
(def N-shift-val 0.07)

(def background-color 200)
(def road-color 100)
(def car-color [0 255 0])
(def goal-arrow-color 255)

(def pause-c-c 100)
(def background-color-p (- background-color pause-c-c))
(def road-color-p (- road-color pause-c-c))
(def car-color-p [0 (- (get car-color 1) pause-c-c) 0])
(def goal-arrow-color-p (- goal-arrow-color pause-c-c))
(def p-t-c 100)

(defn cheat-gen [points index result]
  (if (= index (- (count points) 1))
    (conj result [(get points index) (get points 0)])
    (recur points (inc index) (conj result [(get points index) (get points (inc index))]))))

(defn track-generator [points]
  (cheat-gen points 1 [[(get points 0) (get points 1)]]))

(def track-p [[0 300] [0 -300] [100 -400] [300 -500] [500 -400] [400 -300] 
              [200 -200] [200 0] [300 100] [500 100] [600 200] [500 300]
              [600 400] [500 500] [400 500] [300 300] [200 500] [100 600]])
(def track (track-generator track-p))
(def track-len-1 (- (count track) 1))
(def debug [])

(defn setup []
  {:game-mode :drive
   :x-axis 0
   :y-axis 0
   :angle 0
   :car-location 0
   :speed 0
   :up-pressed false
   :down-pressed false
   :last-up-down nil 
   :left-pressed false
   :right-pressed false
   :last-left-right nil
   :final-road-visited false
   :timer 0
   :best-time nil
   :pause-time nil})

(defn check-acceleration [{:keys [up-pressed down-pressed last-up-down]}]
  (cond 
    (and up-pressed (= last-up-down :up)) 1
    (and down-pressed (= last-up-down :down)) -1
    up-pressed 1
    down-pressed -1
    :else nil))
    
(defn check-rotation [{:keys [left-pressed right-pressed last-left-right speed]}] 
  (cond 
    (= speed 0) 0
    (and left-pressed (= last-left-right :left)) 1
    (and right-pressed (= last-left-right :right)) -1
    left-pressed 1
    right-pressed -1 
    :else 0))

(defn sign [number]
  (if (neg? number) -1 1))

(defn check-on-road [state now-road first-checked]
  (let [now-road (if (= now-road (count track)) 0 now-road)
        p1 (first (get track now-road))
        p2 (second (get track now-road))
        p1-trans [(+ (first p1) (:x-axis state)) (+ (second p1) (:y-axis state))]
        p2-trans [(+ (first p2) (:x-axis state)) (+ (second p2) (:y-axis state))]
        dist-prev (apply q/dist 0 0 p1-trans)
        dist-road (apply q/dist (first p1-trans) (second p1-trans) p2-trans)
        dist-next (apply q/dist 0 0 p2-trans)
        reset-timer (and (= now-road 0) (:final-road-visited state) (< dist-next dist-prev))
        p1-c (c/* (apply c/complex p1-trans) (c/exp (c/complex 0 (:angle state))))
        p2-c (c/* (apply c/complex p2-trans) (c/exp (c/complex 0 (:angle state))))
        road-sign (* (sign (c/imaginary-part p1-c)) (sign (c/imaginary-part p2-c)))]
    (if (< dist-prev 2)
      [true now-road reset-timer]
      (let [value (/ (+ (q/sq dist-prev) (q/sq dist-road) (* -1 (q/sq dist-next))) (* 2 dist-prev dist-road))
            angle-alpha (q/acos (* (sign value) (min (q/abs value) 1)))
            dist-origo (* dist-prev (q/sin angle-alpha))]
        (if (and (<= dist-origo r-w-h) (< road-sign 0))
          [true now-road reset-timer]
          (if (and first-checked (= now-road (:car-location state))) 
            [false now-road reset-timer] 
            (recur state (inc now-road) true)))))))

(defn check-speed [{:keys [speed]} acceleration on-road]
  (let [speed (if speed speed 0)
        speed-a (q/abs speed)
        speed-s (sign speed)
        force-val (when acceleration (if (= acceleration speed-s) acc-val change-val))]
    (cond 
      (and on-road acceleration) (+ speed (* acceleration force-val))
      (and on-road (not acceleration)) 
        (if (< speed-a 0.05) nil (- speed (* speed-s N-shift-val)))
      (and (not on-road) acceleration)
        (if (> speed-a (+ offroad-max-speed 0.01))
          (- speed (* speed-s 0.1 speed-a))
          (* acceleration (min (q/abs (+ speed (* acceleration force-val))) offroad-max-speed)))
    :else (if (< speed-a 0.05) nil (- speed (* speed-s 0.1 speed-a))))))

(defn check-final-road [{:keys [car-location final-road-visited]} reset]
  (cond 
    (= car-location track-len-1) true 
    reset false
    :else final-road-visited))

(defn check-best-time [{:keys [timer best-time]} change-round time-now]
  (if change-round
    (let [round-time (- time-now timer)]
      (if (not best-time) 
        round-time  
        (if (< round-time best-time)
          round-time
          nil))) 
    nil))

(defn update-drive [state]
  (let [acceleration (check-acceleration state)
        rotation (check-rotation state)
        road-checked (check-on-road state (:car-location state) false)
        now-loc (get road-checked 1)
        speed (check-speed state acceleration (get road-checked 0))
        change-round (get road-checked 2)
        final-road (check-final-road state change-round)
        time-now (q/millis)
        timer (when change-round time-now)
        best-time (check-best-time state change-round time-now)]
    (cond-> state
        speed (update-in [:x-axis] + (* (q/sin (:angle state)) speed))
        speed (update-in [:y-axis] + (* (q/cos (:angle state)) speed))
        speed (update-in [:angle] + (* rotation rotation-val))
        (not= now-loc (:car-location state)) (assoc-in [:car-location] now-loc)
        (not= speed (:speed state)) (assoc-in [:speed] speed)
        (not= final-road (:final-road-visited state)) (assoc-in [:final-road-visited] final-road)
        change-round (assoc-in [:timer] timer)
        best-time (assoc-in [:best-time] best-time))))

(defn update-state [state]
  (case (:game-mode state)
    :countdown state 
    :drive (update-drive state)
    :pause state))

(defn draw-road-with-changes [game-mode]
  (let [r-c (if (= game-mode :pause) road-color-p road-color)
        g-a-c (if (= game-mode :pause) goal-arrow-color-p goal-arrow-color)]
    ;;First draw road 
    (q/stroke-weight road-width)
    (q/stroke r-c)
    (q/stroke-cap :round)
    (dotimes [i (count track)]
      (apply q/line (get track i)))
    ;;Second draw goal line and arrow
    (q/stroke-weight 4)
    (q/stroke g-a-c)
    (q/stroke-cap :square)
    (q/line [(* -1 r-w-h) 0] [r-w-h 0])
    (q/line [0 0] [0 -40])
    (q/line [0 -40] [-10 -30])
    (q/line [0 -40] [10 -30])))

(defn draw-road [{:keys [angle x-axis y-axis game-mode]}]
  (q/background background-color)
  (q/with-rotation [angle] 
    (q/with-translation [x-axis y-axis] 
      (draw-road-with-changes game-mode))))

(defn draw-car [{:keys [game-mode]}]
  (let [c-c (if (= game-mode :pause)  car-color-p car-color)]
    (q/no-stroke)
    (apply q/fill c-c)
    (q/rect-mode :center)
    (q/rect 0 0 car-width car-height)))

(defn draw-road-car [state]
  (draw-road state)
  (draw-car state))

(defn add-zeros [minutes seconds milliseconds]
  (let [a (if (> minutes 9) minutes (str "0" minutes))
        b (if (> seconds 9) seconds (str "0" seconds))
        c (if (< milliseconds 100) 
            (if (< milliseconds 10) 
              (str "00" milliseconds)
              (str "0" milliseconds))
            milliseconds)]
    (s/join ":" [a b c])))

(defn convert-mils [mils]
  (when mils
    (let [minutes (math/floor (/ mils 60000))
          seconds (math/floor (- (/ mils 1000) (* minutes 60)))
          milliseconds (- mils (+ (* seconds 1000) (* minutes 60000)))] 
      (add-zeros minutes seconds milliseconds))))

(defn draw-timer [{:keys [game-mode timer pause-time]}]
  (let [t (if (= game-mode :drive) (- (q/millis) timer) pause-time)]  
    (q/text-size 30)
    (q/text (convert-mils t) 10 30)))

(defn draw-best-time [{:keys [best-time]}]
  (q/text-size 30)
  (q/text (str "Best time: " (convert-mils best-time)) 10 70))

(defn draw-state [state]
  (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
    (draw-road-car state))
  (draw-timer state)
  (draw-best-time state))

(defn k-p-drive [state event]
  (case (:key event) 
    :up (-> state 
            (assoc-in [:up-pressed] true)
            (assoc-in [:last-up-down] :up))
    :down (-> state 
              (assoc-in [:down-pressed] true)
              (assoc-in [:last-up-down] :down))
    :left (-> state 
              (assoc-in [:left-pressed] true)
              (assoc-in [:last-left-right] :left))
    :right (-> state 
               (assoc-in [:right-pressed] true)
               (assoc-in [:last-left-right] :right))
    :p (-> state 
           (assoc-in [:game-mode] :pause)
           (assoc-in [:pause-time] (- (q/millis) (:timer state))))
    :r (assoc-in state [:best-time] nil)
    state))

(defn k-p-pause [state event]
  (if (= (:key event) :p)
    (-> state 
           (assoc-in [:game-mode] :drive)
           (assoc-in [:timer] (- (q/millis) (:pause-time state))))
    state))

(defn key-pressed [state event]
  (case (:game-mode state)
    :drive (k-p-drive state event)
    :pause (k-p-pause state event)))

(defn key-released [state event]
  (case (:key event) 
    :up (assoc-in state [:up-pressed] false)
    :down (assoc-in state [:down-pressed] false)
    :left (assoc-in state [:left-pressed] false)
    :right (assoc-in state [:right-pressed] false)
    state))

(q/defsketch autopeli 
  :size size
  :setup setup
  :update update-state 
  :draw draw-state
  :key-pressed key-pressed
  :key-released key-released 
  :middleware [qm/fun-mode])
