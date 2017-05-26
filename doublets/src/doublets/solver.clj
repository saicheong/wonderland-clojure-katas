(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [astar.core :refer [route]]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn words-of-length [n words]
  (filter #(= (count %) n) words))

(defn link-word? [w1 w2]
  (when (= (count w1) (count w2))
    (->> (map #(if (not= %1 %2) 1) w1 w2)
         (keep identity )
         count
         (= 1))))

(defn link-words [w words] (filter #(link-word? w %) words))

(defn get-graph [word]
  (let [wl (words-of-length (count word) words)]
    (fn [fr] (link-words fr wl))))

(defn dist [fr to] 1)

(defn get-h [goal]
  (fn [word]
    (when (= (count word) (count goal))
      (->> (map #(if (not= %1 %2) 1) word goal)
           (keep identity )
           count))))

(defn doublets [word1 word2]
  (if (= (count word1) (count word2))
    (let [r (route (get-graph word1) dist (get-h word2) word1 word2)]
      (if (nil? r) []
                   (into [] (cons word1 r))))
    []))


;; use this to load a bigger file (one word per line)
;; eg from /usr/share/dict/words
(defn load-dict [file]
  (with-open [rdr (io/reader file)]
    (let [data (transient [])]
      (doseq [line (line-seq rdr)]
        (conj! data line))
      (persistent! data))))


;; =====================================================================================
;; my own implementation - which works only on sample list
;; use this to compare and appreciate good algorithms!
;; =====================================================================================

(defn create-links [link words]
  (let [w1 (peek link)
        w2 (peek (pop link))]
    (map #(conj link %)
         (remove #{w2} (link-words w1 words)))))

(defn list-all [w words]
  (let [wl (words-of-length (count w) words)]
    (loop [links (list [w])
           found (list)]
      (if-let [next-link (first links)]
        (let [new-links (create-links next-link wl)]
          (if (= (count new-links) 0)
            (recur (next links) (conj found next-link))
            (recur (apply conj (next links) new-links) found)))
        found))))

(defn my-doublets [word1 word2]
  (if (= (count word1) (count word2))
    (let [wl (words-of-length (count word1) words)]
      (first (loop [links (list [word1])
                    found (list)]
               (if-let [next-link (first links)]
                 (let [new-links (create-links next-link wl)
                       done (filter #(= (peek %) word2) new-links)
                       not-done (filter #(not= (peek %) word2) new-links)]
                   (recur (apply conj (next links) not-done) (apply conj found done)))
                 found))))
    []))