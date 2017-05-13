(ns alphabet-cipher.coder
  (:require [clojure.string :as s]))

;; Look up the substitution table using key 'C' and letter 'E' to get the encoding 'g' is equivalent to this:
;;   A B C
;;   a b c
;;       c d e f [g] h ... z a b
;;       A B C D  E   F ... X Y Z
;;                ^
;; It is akin to offsetting the A-Z window by the position of the key ('C' in the example):

;;   0 1 2 3 4 5  6
;;   a b c d e f  g  h ... z a b
;;   --->|
;;       A B C D  E
;;       0 1 2 3  4
;;
;; So the encoding of 'e' (at index 4) using key 'c' (at index 2) is 'g' (at index 4 + 2 = 6)
;; The decoding of 'g' using the key 'c' is just the reverse, yielding 'e' (at index 6 - 2 = 4)
;;
;; In general, the index of cipher k, message m, and cipher c is related as follows:
;; c = m + k  (encoding operation)
;; m = c - k  (decoding operation)
;; k = c - m  (decipher operation)


(defn- index-of [c]
  "The index position of the character c"
  (- (int c) (int \a)))

(defn- char-at [i]
  "The character at index i. The indices 0, 26, 52 etc all refer to 'a'.
  Specifically, the remainder of i/26 is used to look up the character."
  (let [j (rem (+ 26 i) 26)]
    (char (+ j (int \a)))))

(defn- starts-with? [^String x ^String substr]
  (.startsWith x substr))

(defn- repeating?
  "Test if the items in xs are repeating secret keys"
  [xs]
  (if-let [x1 (first xs)]
    (cond
      (apply = xs) x1                                        ; all items are the same (= first)
      (and (apply = (butlast xs))                            ; all items except last are the same (= first)
           (starts-with? x1 (last xs))) x1                   ; and last item is x1 truncated
      :else nil)))

(defn- secret-key
  "The secret key used to encode/decode message"
  [s]
  ;; generate sequences of all possible sizes and find the first sequence that is repeating
  ;; (make up of repeating secret key
  (some repeating? (->> (range 1 (inc (count s)))
                        (map #(partition-all % s))
                        (map (fn [x] (map #(apply str %) x))))))

(defn encode [keyword message]
  (let [keyword (map index-of keyword)
        message (map index-of message)]
    (->> (map #(+ %1 %2) (cycle keyword) message)
         (map char-at)
         (s/join))))

(defn decode [keyword message]
  (let [keyword (map index-of keyword)
        message (map index-of message)]
    (->> (map #(- %1 %2) message (cycle keyword))
         (map char-at)
         (s/join))))

(defn decipher [cipher message]
  (let [cipher (map index-of cipher)
        message (map index-of message)]
    (->> (map #(- %1 %2) cipher message)
         (map char-at)
         (s/join)
         (secret-key))))