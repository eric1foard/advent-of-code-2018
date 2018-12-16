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
    (reverse (vec (map (fn [x] (read-string x)) (str/split dims-str #"x"))))))

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

(defn grid-sum
  [grid i sum]
  (if (>= i (count grid))
    sum
    (recur grid (inc i) (+ sum (reduce + (filter (fn [x] (if (>= x 2) true false)) (nth grid i)))))))

(defn overlap
  [pos-vec]
  (let [grid (init-grid pos-vec)
        [x y] (:pos pos-vec)
        [width height] (:dimensions pos-vec)]
    (grid-sum (reduce (fn [acc elt] (next-grid acc elt)) grid pos-vec) 0 0)))

(overlap (pos-map-list (file-to-list "/Users/ericfoard/code/misc/advent-of-code/src/advent_of_code/day-3-input.txt") (list)))


