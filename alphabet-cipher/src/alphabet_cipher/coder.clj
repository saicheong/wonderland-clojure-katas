(ns alphabet-cipher.coder
  (:require [clojure.string :as s]))

;; generate string "abc...": (apply str (for [x (range (int \a) (+ (int \z) 1))] (char x)))
(def alphabets "abcdefghijklmnopqrstuvwxyz")

;; The 26 x 26 substitution table
;; Note that the table is symmetrical on left to right diagonal
;; a b c d
;; b c d
;; c d e
;; d . . g
;; . . . . i
;; that is: (= (get-in subst-table 3 5) (get-in subst-table 5 3)
(def subst-table (into [] (take 26 (map #(into [] %) (partition 26 1 (cycle alphabets))))))

;; 'Printable substitution table
(defn view-subst-table1 [tab]
  (map #(s/join " " %) tab))

(defn- char->int [c]
  (- (int c) (int \a)))

(defn- int->char [i]
  (char (+ i (int \a))))

;; lifted from v1.8 of clojure.string starts-with?
(defn- starts-with? [^String x ^String substr]
  (.startsWith x substr))

(defn- repeating?
  "Test if the items in xs are repeating secret keys"
  [xs]
  (cond
    (nil? xs) nil
    (= (count xs) 0) nil
    :else (let [x (first xs)]
            (cond
              (= (count xs) 1) x
              (apply = xs) x
              (and (apply = (butlast xs))
                   (starts-with? x (last xs))) x
              :else nil))))

(defn- repeating-seq
  "Get the 'repeating' - eg ('abc' 'abc' 'abc') or ('abc' 'ab') or ('abc')"
  [s]
  ;; generate sequences of all possible sizes and find the first sequence that is repeating
  (some repeating? (->> (range 1 (inc (count s)))
                        (map #(partition-all % s))
                        (map (fn [x] (map #(apply str %) x))))))


(defn encode [keyword message]
  (apply str (map (fn [k w]
                    (let [k (char->int k)
                          w (char->int w)]
                      (get-in subst-table [k w])))
                  (cycle keyword) message)))

(defn decode [keyword message]
  (apply str (map (fn [k w]
                    (let [k (char->int k)]
                      (int->char (.indexOf (subst-table k) w))))
                  (cycle keyword) message)))


(defn decipher [cipher message]
  (repeating-seq (apply str (mapcat (fn [c m]
                                      (keep-indexed (fn [idx item]
                                                      (when (= c (item (char->int m)))
                                                        (int->char idx))) subst-table))
                                    cipher message))))

