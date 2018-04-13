;; Jonathan Gregory
;; CS 441
;; Final Project
;; Functional Programming / Concurrency

;; This program takes in a series of numbers from an external text file and performs a mergesort on it
;; It repeates the task 7 times with 1, 2, 4, 8, 16, 32, and 64 threads and times each one
;; Many citations are found in the program itself while others are found in the summary and explination paper
(ns cs441
  (:require [clojure.java.io :as io]) ;; Use clojure.java.io to read in resource
  (:require [clojure.string :as str]) ;; Clojure String utilities that splits string on a regular expression
  (:import (java.util.concurrent Executors))) ;; Group of concurrency utilities in the JDK

;; This is a merge sort that was borrowed from the following website
;; https://gist.github.com/alco/2135276
(defn merge-seqs
  "Merges two sorted sequences into a single sorted sequence"
  ([left right]
   (merge-seqs (list left right)))
  ([[left right]]
   (loop [l left, r right, result []]
     (let [lhead (first l), rhead (first r)]
       (cond
         (nil? lhead)     (concat result r)
         (nil? rhead)     (concat result l)
         (<= lhead rhead) (recur (rest l) r (conj result lhead))
         true             (recur l (rest r) (conj result rhead)))))))

(defn mergesort
  "Produces a sorted sequence from an input sequence.
  Works best with vectors (since it uses 'count' internally)."
  [xs]
  ((fn mergesort-counted [xs n]
     (if (<= n 1)
       xs
       (let [middle (bit-shift-right n 1)]  ; fast division by 2
         (merge-seqs (map mergesort-counted
                          (split-at middle xs)        ; two halves
                          [middle (- n middle)])))))  ; count of each half
    xs (count xs)))

;; This function gets the lines from the file
;; It takes the name of the file in as a parameter
(defn getLines [fname]  ;; defines the function as getLines and takes the file name in
    (map read-string ;; Returns a lazy sequence consisting of each line from the file as a string
       (str/split-lines ;; Splits each line into seperate strings based on the new line
         (slurp fname)))) ;; Opens a reader on fname and reads all its contents, returning a string.

;;This function called threads takes in the sequence of lines and the number of desired threads and returns a sequences of list
(defn threads [lineSeq threadCount]
  (partition (/ (count lineSeq) threadCount) lineSeq))

;; This is a loop that that prints out the time it takes each for the merge sort with each thread
;; inspiration for the doseq loop instead of using a for loop was from
;; http://stackoverflow.com/questions/6520699/how-do-i-print-a-list-of-numbers-on-each-line-in-clojure
;; Inspiration for using a sequesnce of numbers for the numberOfThreads was from
;; https://clojuredocs.org/clojure.core/doseq
(doseq [numberOfThreads [1 2 4 8 16 32 64]] ;; loops through each of the required thread iterations
      (print "Now performing mergesort with" numberOfThreads "number of threads: ") ;; prints out the threads it is using
      (time (reduce merge-seqs (pmap mergesort (threads (getLines "numbers_txt.txt" )numberOfThreads))))) ;; prints out the time the sort takes for each thread

(print "Program Has Finished. Thank You.")