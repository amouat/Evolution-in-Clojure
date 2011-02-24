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
    (dosync (ref-set *plants* (conj @*plants* pos)))))

(defn add-plants []
  (apply #'random-plant *jungle*)
  (random-plant 0 0 *width* *height*))

(defstruct animal :x :y :energy :dir :genes)

(def *animals*
  (list (struct animal
          (round (/ *width* 2)) ; x
          (round (/ *height* 2)) ; y
          1000 ; energy
          0 ; dir
          (loop [genes [] i 8]
            (if (zero? i)
              genes
              (recur (conj genes (+ 1 (rand-int 10))) (dec i)))))))

(defn move [orig] ; don't use animal here!
  (let [dir (:dir orig)
        x (:x orig)
        y (:y orig)]
    (struct animal
      (mod (+ x
             (cond 
               (and (>= dir 2) (< dir 5)) 1
               (or (= dir 1) (= dir 5)) 0
               :else -1) ; note clojure has else form and less ()s
             *width*)
        *width*) ; x
      (mod (+ y
             (cond (and (>= dir 0) (< dir 3)) -1
               (and (>= dir 4) (< dir 7)) 1
               :else 0)
             *height*)
        *height*) ; y
      (dec (:energy orig)) ; energy
      (:dir orig)
      (:genes orig)))) ; return new animal rather than change given

(defn turn [orig]
  (let [x (rand-int (apply #'+ (:genes orig)))]
    (defn angle [genes y]
      (let [xnu (- y (first genes))]
        (if (< xnu 0)
          0
          (+ 1 (angle (rest genes) xnu)))))
     ; use assoc to return updated animal
    (assoc orig :dir (mod (+ (:dir orig) (angle (:genes orig) x)) 8))))

(defn eat [orig]
  (let [pos (list (:x orig) (:y orig))]
    (if (contains? @*plants* pos)
       ; get rid of the plant first
      (dosync (ref-set *plants* (disj @*plants* pos))
         ; so we can return the updated animal
        (assoc orig :energy (+ (:energy orig) *plant-energy*)))
      orig)))

(def *reproduction-energy* 200)

; we have to do reproduce differently - 
; return a list with orig animal and any new animal
(defn reproduce [orig]
  (let [e (:energy orig)]
    (if (>= e *reproduction-energy*)
      (let [mutation (rand-int 8)
            orig-genes (:genes orig)]
        (list (assoc orig :energy (floor (/ e 2)))
          (assoc 
            (assoc orig :genes 
              (assoc orig-genes mutation 
                (max 1 (+ (nth orig-genes mutation) (rand-int 3) -1)))) 
            :energy (floor (/ e 2)))))
      (list orig))))

(defn remove-dead [animal-list]
  (remove #(<= (:energy %) 0) animal-list))

(def testanimals (list 
                   (struct animal 5 5 0 4 [1 2 3 4 5 6 7 8]) 
                   (struct animal 6 6 100 4 [1 2 3 4 5 6 7 8]) 
                   (struct animal 7 7 200 4 [1 2 3 4 5 6 7 8])))


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
                  (loop [i 0 state animals]
                    (if (< i x)
                      (recur (inc i) (update-world state))
                      (update-world state)))))))))

(evolution *animals*)        

