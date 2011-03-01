(ns evolve
  "Clojure version of Evolution. Direct port of evolution game described in Land
of Lisp by Conrad Barski."
  (:use clojure.contrib.math))
(def *width* 100)
(def *height* 30)
(def *jungle* '(45 10 10 10))
(def *plant-energy* 80)
(def *plants* (ref #{}))

(defn random-plant [left top width height]
  (let [pos (list (+ left (rand-int width)) (+ top (rand-int height)))]
    (dosync (alter *plants* conj pos))))

(defn add-plants []
  (apply #'random-plant *jungle*)
  (random-plant 0 0 *width* *height*))

(def *animals*
  (list {:x (round (/ *width* 2))
         :y (round (/ *height* 2))
         :energy 1000
         :dir 0
         :genes (vec (repeatedly 8 #(inc (rand-int 10))))}))

(defn move [{:keys [x y dir] :as animal}]
    {:x (mod (+ x
             (cond 
               (and (>= dir 2) (< dir 5)) 1
               (or (= dir 1) (= dir 5)) 0
               :else -1)) ; note clojure has else form and less ()s
        *width*)
      :y (mod (+ y
             (cond (and (>= dir 0) (< dir 3)) -1
               (and (>= dir 4) (< dir 7)) 1
               :else 0))
        *height*)
      :energy (dec (:energy animal))
      :dir (:dir animal)
      :genes (:genes animal)}) ; return new animal rather than change given

(defn turn [{:keys [genes dir] :as animal}]
  (let [x (rand-int (apply #'+ genes))]
    (defn angle [g y]
      (let [xnu (- y (first g))]
        (if (< xnu 0)
          0
          (+ 1 (angle (rest g) xnu)))))
     ; use assoc to return updated animal
    (assoc animal :dir (mod (+ dir (angle genes x)) 8))))

(defn eat [{:keys [x y energy] :as animal}]
  (let [pos (list x y)]
    (dosync
      (if (contains? @*plants* pos)
        (do
          ; get rid of the plant first
          (alter *plants* disj pos)
          ; so we can return the updated animal
          (assoc animal :energy (+ energy *plant-energy*)))
        animal))))

(def *reproduction-energy* 200)

; we have to do reproduce differently - 
; return a list with original animal and any new animal
(defn reproduce [{:keys [energy genes] :as animal}]
    (if (>= energy *reproduction-energy*)
      (let [mutation (rand-int 8)]
        (list (assoc animal :energy (floor (/ energy 2)))
          (assoc 
            (assoc animal :genes 
              (assoc genes mutation 
                (max 1 (+ (nth genes mutation) (rand-int 3) -1)))) 
            :energy (floor (/ energy 2)))))
      (list animal)))

(defn remove-dead [animal-list]
  (remove #(<= (:energy %) 0) animal-list))

(def testanimals (list 
                   {:x 5 :y 5 :energy 0 :dir 4 :genes [1 2 3 4 5 6 7 8]}
                   {:x 6 :y 6 :energy 100 :dir 4 :genes [1 2 3 4 5 6 7 8]}
                   {:x 7 :y 7 :energy 200 :dir 4 :genes [1 2 3 4 5 6 7 8]}))


(defn update-world [animals]
  (do
    (add-plants) ;different sequence, but easier this way
    (flatten (map #(reproduce (eat (move (turn %)))) (remove-dead animals)))))

(defn draw-world [animals plants]
  (doseq []
    (loop [y 0]
      (when (< y *height*)
        (println)
        (print "|")
        (loop [x 0]
          (when (< x *width*)
            (print (cond (some #(and (= (:x %) x)
                                  (= (:y %) y))
                           animals)
                     "M"
                     (contains? plants (list x y)) "*"
                     :else " "))
            (recur (inc x))))
        (print "|")
        (recur (inc y))))))

;Helper method takes string and returns integer equivalent or 0 if can't be
;parsed
(defn parseInput [x]
  (try (Integer/parseInt x) (catch Exception e 0)))
  
(defn evolution [animals]
  (do 
    (draw-world animals @*plants*)
    (println) ; don't do 'print "\n"' or 'newline' as won't flush
    (let [line (read-line)] ;str is a function in clojure
      (cond (= line "quit") ()
        (= line "p") (do (println animals) (evolution animals))
        :else (evolution 
                (let [x (parseInput line)]
                  (nth (iterate update-world animals) (inc x))))))))

(evolution *animals*)        

