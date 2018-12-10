(ns advent-of-code.day-2-puzzle-1
  (:gen-class)
  (:require [clojure.string :as str]))

(defn file-to-list
  "given a file, returns list of file contents delimited by newline characters"
  [file]
  (apply list (str/split (slurp file) #"\n")))

(defn char-count
  [char-list char-map]
    (if (empty? char-list)
      char-map
      (let [curr-char-count (get char-map (first char-list))]
        (recur (rest char-list)
               (assoc
                char-map
                (first char-list)
                (if (nil? curr-char-count)
                  1
                  (inc curr-char-count)))))))

(defn has-correct-count
  [char-list n char-map]
  (let [populated-map (char-count char-list char-map)]
    (some  #(= n %) (vals populated-map))))

(defn char-count-list
  "given a list of strings and target count, return number of strings
  which have a subset of duplicate chars exactly equal to target count"
  [word-list n]
  (reduce + (map (fn [x] (if (has-correct-count (apply list (str/split x #"")) n {}) 1 0)) word-list)))

(defn checksum
  "given list of stings, find those with exactly 2 repeated chars
  and those with exactly 3 repeated chars
  and multiply the counts of each"
  [word-list]
  (* (char-count-list word-list 2) (char-count-list word-list 3)))

;; solution
(checksum (file-to-list "/Users/ericfoard/code/misc/advent-of-code/src/advent_of_code/day-2-input.txt"))
