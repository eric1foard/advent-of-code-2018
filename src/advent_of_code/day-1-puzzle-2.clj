(ns advent-of-code.day-1-puzzle-2
  (:gen-class)
  (:require [clojure.string :as str]))

(use '[clojure.test :only [is]])

(defn file-to-list
  "given a file, returns list of file contents delimited by newline characters"
  [file]
  (let [str-nums (str/split (slurp file) #"\n")]
    (map (fn [x] (read-string x) ) str-nums)))


(defn dup-sum
  "given a list of numbers, return first sum that appears twice"
  [all-nums curr-nums sum accum-set]
  (if (empty? all-nums)
    nil
    (if (empty? curr-nums)
      (recur all-nums all-nums sum accum-set)
      (let [next-sum (+ sum (first curr-nums))]
        (if (contains? accum-set next-sum)
          next-sum
          (recur all-nums (rest curr-nums) next-sum (conj accum-set next-sum)))))))


(defn dup-sum-test
  [nums]
  (dup-sum nums nums 0 #{0}))

;; tests
(def EMPTY_LIST (list))
(def LIST_1 (list 1 -1))
(def LIST_2 (list 3 3 4 -2 -4))
(def LIST_3 (list -6 3 8 5 -6))
(def LIST_4 (list 7 7 -2 -7 -4))

(is (= (dup-sum-test EMPTY_LIST) nil))
(is (= (dup-sum-test LIST_1) 0))
(is (= (dup-sum-test LIST_2) 10))
(is (= (dup-sum-test LIST_3) 5))
(is (= (dup-sum-test LIST_4) 14))

;; puzzle solution
(def INPUT_FILE "/Users/ericfoard/code/misc/advent-of-code/src/advent_of_code/day-1-puzzle-1-input.txt")
(def CHALLENGE_LIST (file-to-list INPUT_FILE))
(dup-sum-test CHALLENGE_LIST)
 
