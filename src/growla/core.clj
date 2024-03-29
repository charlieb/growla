(ns growla.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [kdtree :as kd]
            ))

(defn prnt [x] (println x) x)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def radius 2.)
(defn mk-circle [x y r parent kind] {:x x :y y :r r :parent parent :kind kind})
(defn to-v2 [c] [(:x c) (:y c)])
(defn angle-to-vec [rads] [(q/cos rads) (q/sin rads)])

(def pentagon-angles (range 0.0 (* 2 Math/PI) (/ (* 2 Math/PI) 5)))
(def hexagon-angles  (range 0.0 (* 2 Math/PI) (/ (* 2 Math/PI) 6)))
(def pentagon-points (map angle-to-vec pentagon-angles))
(def hexagon-points  (map angle-to-vec hexagon-angles))

;;;;;;;;;;;;;; SEEDS ;;;;;;;;;;;;
(defn seeds-kind [n w h]
  (map (fn [_]
         (let [kind-offset 
               ; (rand-nth pentagon-angles)
               (q/random (- Math/PI) Math/PI)
               ]
           (mk-circle (q/random (- (/ w 2)) (/ w 2))
                      (q/random (- (/ h 2)) (/ h 2))
                      radius
                      nil
                      (map #(angle-to-vec (+ % kind-offset)) hexagon-angles))))
       (range n)))

(defn seeds-numeric-kind [n w h]
  (map #(mk-circle (q/random (- (/ w 2)) (/ w 2))
                   (q/random (- (/ h 2)) (/ h 2))
                   radius
                   nil
                   %)
       (range n))
  [(mk-circle 0. 0. radius nil 0)
   (mk-circle 5. 0. radius nil 1)
   (mk-circle 5. 5. radius nil 2)
   (mk-circle 0. 5. radius nil 3)]
  )

(defn seeds-center [_ _ _] [(mk-circle 0. 0. radius nil nil)])
(def seeds seeds-kind)

;;;;;;;;;;;;;; SEEDS ;;;;;;;;;;;;

(defn init-growla []
  (let [
        cs (seeds 50 (/ (q/width) 2) (/ (q/height) 2)) ; [c0 c1]
        tree (kd/build-tree (vec (map #(with-meta (to-v2 %) %) cs)))
        ]
    {:circles cs :tree tree}
    ))

;;;;;;;;;;;; GROW ;;;;;;;;;;;;;;
(defn grow-kind2 [cs tree]
  (let [candidate (rand-nth cs)
        pos (to-v2 candidate)
        new-pos (vec (map #(+ %1 (* 2 radius %2)) pos (q/random-2d)))
        nears (kd/nearest-neighbor tree new-pos 20)]
    ;(println (:kind candidate))
    ;(doseq [near nears](println "   " (:kind (meta near))))
    (when (and (= pos (:point (first nears)))
               (some #(not (= (:kind candidate) (:kind (meta %)))) nears))
        (mk-circle (new-pos 0) (new-pos 1) radius candidate (:kind candidate)))))

(defn grow-kind [cs tree]
  (let [candidate (rand-nth cs)
        pos (to-v2 candidate)
        new-pos (vec (map #(+ %1 (* 2 radius %2)) pos 
                          (rand-nth (:kind candidate))))]
    (when (= pos (:point (kd/nearest-neighbor tree new-pos)))
      (mk-circle (new-pos 0) (new-pos 1) radius candidate (:kind candidate)))))

(defn grow-random [cs tree]
  (let [candidate (rand-nth cs)
        pos (to-v2 candidate)
        new-pos (vec (map #(+ %1 (* 2 radius %2)) pos (q/random-2d)))]
    (when (= pos (:point (kd/nearest-neighbor tree new-pos)))
      (mk-circle (new-pos 0) (new-pos 1) radius candidate (:kind candidate)))))

;;;;;;;;;; CONFIG ;;;;;;;;;;
(def grow grow-kind)
;;;;;;;;;;;; ;;;;;;;;;;;;;;

(defn growtimes [state n]
  (loop [cs (:circles state)
         tree (:tree state)
         n n
         failures 0]
    (println n)
    (if (or (> failures 1000) (zero? n))
      {:circles cs :tree tree}
      (if-let [c (grow cs tree)]
        (recur (conj cs c) (kd/insert tree (with-meta (to-v2 c) c)) (dec n) 0)
        (recur cs tree n (inc failures))))))

;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;
(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :rgb)
  (growtimes (init-growla) 1000))

(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  state)

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)
  ; Set circle color.
  ;(q/fill 150 150 150)
  (q/no-fill)
  (q/stroke 50 50 150)
  ; Move origin point to the center of the sketch.
  (q/with-translation [(/ (q/width) 2)
                       ;30
                       (/ (q/height) 2)
                       ]
    (doseq [c (:circles state)]
  ;    (when (nil? (:parent c))
  ;      (q/ellipse (:x c) (:y c) (* 1.0 (:r c)) (* 1.0 (:r c))))
      (when (:parent c)
  ;      (q/ellipse (:x c) (:y c) (* 1.0 (:r c)) (* 1.0 (:r c)))
        ; Draw the circle.
        ;(println (c 0) (c 1))
        ;        (q/ellipse (c 0) (c 1) 2 2)
        ;(when (or true (< (:layer c) (:lit-layer state)))
        (q/line (:x c)
                (:y c)
                (:x (:parent c)) 
                (:y (:parent c)))
        ;  )

        ))
    (q/save "out.png")))


(q/defsketch growla
  :title "You spin my circle right round"
  :size [500 500]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
