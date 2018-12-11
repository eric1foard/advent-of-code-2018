(ns advent-of-code.day-2-puzzle-2
  (:gen-class)
  (:require [clojure.string :as str]))

(defn file-to-list
  "given a file, returns list of file contents delimited by newline characters"
  [file]
  (let [str-nums (str/split (slurp file) #"\n")]
    (vec str-nums)))

(defn diff-words
  [w1 w2 word-len diff-count]
  (if (< word-len 0)
    diff-count
    (diff-words
       w1
       w2
       (dec word-len)
       (if (= (nth w1 word-len) (nth w2 word-len))
         diff-count
         (inc diff-count)))))

(defn compare-word
  [word word-list index]
  (if (>= index (count word-list))
    nil
    (let [diff (diff-words word (nth word-list index) (dec (count word)) 0)]
      (if (= 1 diff)
        [word (nth word-list index)]
        (recur word word-list (inc index))))))

(defn compare-words
  [word-list index]
  (if (>= index (count word-list))
    nil
    (let [pair (compare-word (nth word-list index) word-list 0)]
      (if (not (nil? pair))
        pair
      (recur word-list (inc index))))))

(defn common-chars
  [w1 w2 i]
  (cond
    (>= i (count w1)) nil
    (not (= (nth w1 i) (nth w2 i))) (str (subs w1 0 i) (subs w1 (inc i) (count w1)))
    :else (recur w1 w2 (inc i))))

(defn find-id-pair
  "given list of strings, find pair that differs by exactly 1 char"
  [word-list]
  (let [[w1 w2] (compare-words word-list 0)]
    (common-chars w1 w2 0)))

  (def WORD_LIST (file-to-list "/Users/ericfoard/code/misc/advent-of-code/src/advent_of_code/day-2-input.txt"))

  (find-id-pair WORD_LIST)

