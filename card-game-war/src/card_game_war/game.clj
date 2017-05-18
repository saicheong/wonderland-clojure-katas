(ns card-game-war.game)

;; feel free to use these cards or use your own data structure
(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(def cards
  (for [suit suits
        rank ranks]
    [suit rank]))

(defn value
  "Returns the value of card that can be used for comparison and sorting."
  [card]
  [(.indexOf ranks (nth card 1)) (.indexOf suits (nth card 0))])

(defn higher? [higher-card lower-card]
  (> (compare (value higher-card) (value lower-card)) 0))

(defn highest-value
  "Returns the highest value card"
  [cards]
  (last (sort #(compare (value %1) (value %2)) cards)))

(defn deal
  "Distribute cards evenly into n vectors.
  A deck of 52 cards will be distributed into 3 decks of 18 17 17 cards respectively"
  [n cards]
  (loop [idx 0                                              ; track which hand is to receive the next card
         cards (seq cards)
         ;; todo - find a better way to do this
         hands (into [] (map (fn [i] (vector)) (range n)))]
    (if cards
      (let [card (first cards)
            hand (nth hands idx)
            new-hand (conj hand card)]
        (recur (rem (inc idx) n) (next cards) (assoc hands idx new-hand)))
      hands)))

;;
;; Added support for more than 2 players, just for fun
;; ---------------------------------------------------

(defn play-round
  "Returns the index of the winning card"
  ([c1 c2] (if (< (compare (value c1) (value c2)) 0) 1 0))
  ([c1 c2 & more]
    (let [cs (conj more c2 c1)
          highest (highest-value cs)]
      (first (keep-indexed #(if (= highest %2) %1) cs)))))

(defn play-game
  "Accepts 2 or more arrays of cards. The game ends when one or more players
  have no more cards"
  [cards1 cards2 & more]
  (let [players (conj more cards2 cards1)
        losers (keep-indexed #(when (= (count %2) 0) %1) players)]
    (println (map count players))
    (if (> (count losers) 0)
      (do (println "Player"
                   (map inc losers)
                   (if (= 1 (count losers)) "is" "are")
                   "out!")
          losers)
      ;otherwise play another round
      (let [cards (into [] (map peek players))              ; collect cards from all
            winner (apply play-round cards)
            winnings (shuffle cards)]                       ; shuffle winnings
        (->> players
             (map pop)                                      ; remove played cards
             (map-indexed #(if (not= winner %1) %2 (apply conj winnings %2)))
             (apply play-game))))))
