(ns fox-goose-bag-of-corn.puzzle
  (:require [clojure.set :as set]
            [astar.core :refer [route]]))

(def start [#{:fox :goose :corn :you}  #{:boat}  #{}                      ])
(def end   [#{}                        #{:boat}  #{:fox :goose :corn :you}])

(def bad-pairings [#{:fox :goose} #{:goose :corn}])

(defn onto-boat
  "The valid ways to get onto boat"
  [bank boat]
  (let [bank (disj bank :you)
        boat (conj boat :you)]
    (filter (fn [c] (not-any? #(= % (first c)) bad-pairings))
            (cons (list bank boat)
                  (for [item bank]
                    (list (disj bank item) (conj boat item)))))))

(defn off-boat [bank boat]
  "The valid ways to get off boat"
  (let [items (disj boat :boat :you)
        bank (conj bank :you)]
    (list (apply conj bank items) #{:boat})))

(defn from-left-bank
  "The valid ways to get stuff on boat from left bank"
  [step]
  (let [[left boat right] step]
    (map #(vector (first %) (second %) right) (onto-boat left boat))))

(defn on-boat
  "The valid ways to get stuff off boat to either banks"
  [step]
  (let [[left boat right] step
        [left1 boat1] (off-boat left boat)
        [right2 boat2] (off-boat right boat)]
    (list (vector left1 boat1 right)
          (vector left boat2 right2))))

(defn from-right-bank
  "The valid ways to get stuff on boat from right bank"
  [step]
  (let [[left boat right] step]
    (map #(vector left (second %) (first %)) (onto-boat right boat))))

(defn graph [step]
  (let [[left boat right] step]
    (cond
      (some #{:you} left) (from-left-bank step)
      (some #{:you} boat) (on-boat step)
      (some #{:you} right) (from-right-bank step))))

(defn dist [fr to]
  (count (set/difference (last to) (last fr))))

(defn get-h [goal]
  (fn [node]
    (dist node goal)))

(defn river-crossing-plan []
  (map (partial map vec)                                    ;convert crossing plan to vector of vector form
       (cons start (route graph dist (get-h end) start end))))
