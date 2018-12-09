(ns advent-of-code.day-1-puzzle-1
  (:gen-class)
  (:require [clojure.string :as str]))

(require '[clojure.string :as str])

(defn file-to-list
  "given a file, returns list of file contents delimited by newline characters"
  [file]
  (let [str-nums (str/split (slurp file) #"\n")]
    (reduce + (map (fn [x] (read-string x)) str-nums))))

(file-to-list "/Users/ericfoard/code/misc/advent-of-code/src/advent_of_code/day-1-puzzle-1-input.txt")


