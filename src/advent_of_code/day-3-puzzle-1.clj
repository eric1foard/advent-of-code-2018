(ns advent-of-code.day-3-puzzle-1
  (:gen-class)
  (:require [clojure.string :as str]))

(require '[clojure.string :as str])

(defn file-to-list
  "given a file, returns list of file contents delimited by newline characters"
  [file]
  (let [str-nums (str/split (slurp file) #"\n")]
    str-nums))

(defn get-pos
  [line]
  (let [pos-str (subs line (inc (str/index-of line "@")) (str/index-of line ":"))]
    (vec (map (fn [x] (read-string x)) (str/split pos-str #",")))))

(defn get-dims
  [line]
  (let [dims-str (subs line (inc (str/index-of line ":")) (count line))]
    (vec (map (fn [x] (read-string x)) (str/split dims-str #"x")))))

(defn pos-map
  [line]
  (let [line-num (subs line 1 (dec (str/index-of line "@")))
        pos (get-pos line)
        dimensions (get-dims line)]
    { :line-num line-num :pos pos :dimensions dimensions }))

(defn pos-map-list
  [lines pos-list]
  (if (empty? lines)
    pos-list
    (recur (rest lines) (conj pos-list (pos-map (first lines))))))

(defn init-grid-dimensions
  [pos-vec]
  (reduce
   (fn [acc elt]
            (let [[pos-x pos-y] (:pos elt)
                  [dim-x dim-y] (:dimensions elt)]
              {:max-x (max (:max-x acc) (+ pos-x dim-x))
               :max-y (max (:max-y acc) (+ pos-y dim-y))}))
   {:max-x -1 :max-y -1}
   pos-vec))

(defn fill-grid
  [x y init-val]
  (vec (repeat y (vec (repeat x init-val)))))

(defn init-grid
  [pos-vec]
  (let [dims (init-grid-dimensions pos-vec)]
    (fill-grid (:max-x dims) (:max-y dims) 0)))

(defn update-row
  ([row pos] (update-row row pos (nth (:pos pos) 0)))
  ([row pos vec-pos]
  (let [[x y] (:pos pos)
        [width height] (:dimensions pos)]
    (if (>= vec-pos (+ x width))
      row
      (recur (assoc row vec-pos (+ 1 (get row vec-pos))) pos (inc vec-pos))))))

(defn next-grid
  ([grid pos] (next-grid grid pos (nth (:pos pos) 1)))
  ([grid pos curr-row]
  (let [[x y] (:pos pos)
        [width height] (:dimensions pos)]
    ;;(do (println "y!!!! ", y "height!!!! ", height, "curr-row ", curr-row)
    (if (>= curr-row (+ y height))
      grid
      (recur (assoc grid curr-row (update-row (nth grid curr-row) pos)) pos (inc curr-row))))))

(defn row-count
  [row curr-pos curr-count]
  (cond
    (>= curr-pos (count row)) curr-count
    (> (nth row curr-pos) 1) (recur row (inc curr-pos) (inc curr-count))
    :else (recur row (inc curr-pos) curr-count)))

(defn grid-count
  [grid]
  (reduce + (map (fn [row] (row-count row 0 0)) grid)))

(defn populated-grid
  [pos-vec]
  (let [grid (init-grid pos-vec)
      [x y] (:pos pos-vec)
      [width height] (:dimensions pos-vec)]
    (reduce (fn [acc elt] (next-grid acc elt)) grid pos-vec)))

(defn overlap
  [claim-vec]
  (grid-count (populated-grid claim-vec)))

;; PUZZLE 1 SOLUTION
(overlap (pos-map-list (file-to-list "/Users/ericfoard/code/misc/advent-of-code/src/advent_of_code/day-3-input.txt") (list)))

(defn row-sum
  [row low high]
  (let [ans (reduce + (map (fn [x] (nth row x)) (range low high)))]
  (do (println "row ", row, "low ", low, "high ", high, "row sum ", ans) ans)))


(defn subgrid-sum
  [grid claim]
  (let [[x y] (:pos claim)
        [width height] (:dimensions claim)]
    (do (println "x ", x, "y ", y, "width ", width, "height ", height)
    (reduce + (map (fn [row] (row-sum (nth grid row) x (+ x width))) (range y (+ y height)))))))

(defn has-no-overlap?
  [grid claim]
  (let [[width height] (:dimensions claim)
        target-sum (* width height)
        sum (subgrid-sum grid claim)]
    (do (println "target ", target-sum, "sum ", sum))(= target-sum sum)))

(defn find-no-overlap
  [grid claims]
  (filter (fn [c] (has-no-overlap? grid c)) claims))

;; PUZZLE 2 SOLUTION
(def claim-vec (pos-map-list (file-to-list "/Users/ericfoard/code/misc/advent-of-code/src/advent_of_code/day-3-input.txt") (list)))
(def grid (populated-grid claim-vec))

;; (populated-grid claim-vec)

(find-no-overlap grid claim-vec)



