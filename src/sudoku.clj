(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [row col]]
  (set (get board row)))

(defn col-values [board [row col]]
  (set (for [r board]
         (get r col))))

(defn coord-pairs [coords]
  (for [c coords
        r coords]
    [c r]
    ))

(defn upper-left [board [row col]]
  (let [place (fn [x] (cond 
                        (< x 3) 0
                        (< x 6) 3
                        :else 6))
        r (place row)
        c (place col)]
    [r c]))

(defn block-values [board coord]
    (set (flatten (for [r (take 3 (drop (first (upper-left board coord)) board))]
      (take 3 (drop (second (upper-left board coord)) r))))))

(defn valid-values-for [board coord]
  (if
    (has-value? board coord) #{}
    (set/difference 
     #{1 2 3 4 5 6 7 8 9} 
     (set/union 
      (row-values board coord) 
      (col-values board coord) 
      (block-values board coord)))))

(defn filled? [board]
  (not (contains? (set (flatten board)) 0)))

(defn rows [board]
  (for [r board]
    (set r)))

(defn valid-rows? [board]
  (every? #(= #{1 2 3 4 5 6 7 8 9} %) (rows board)))

(defn cols [board]
  (for [i (range 9)]
    (set (col-values board [1 i]))))

(defn valid-cols? [board]
  (every? #(= #{1 2 3 4 5 6 7 8 9} %) (cols board)))

(defn blocks [board]
  (for [i [0 3 6]
        j [0 3 6]]
    (set (block-values board [i j]))))

(defn valid-blocks? [board]
  (every? #(= #{1 2 3 4 5 6 7 8 9} %) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [b board
         row (first board)
         i 0
         j 0]
  (cond
    (= [[]] board) []
    (empty? row) (recur (rest board) (second board) (inc i) 0) 
    (zero? (first row)) [i j]
    :else (recur b (rest row) i (inc j)))))

(defn solve [board]
    (if (empty? (find-empty-point board))
      (if (valid-solution? board) 
        board 
        [])
      (let [block (find-empty-point board)]
        (for [val (valid-values-for board block)
              solution (solve (set-value-at board block val))]
          solution
          ))
      ))
