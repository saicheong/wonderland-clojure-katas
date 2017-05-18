(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))


;; fill in  tests for your game
(deftest test-play-round
  (testing "the highest rank wins the cards in the round"
    (is (= 0
           (play-round [:heart :ace] [:club :king])))
    (is (= 1
           (play-round [:club :king] [:heart :ace])))
    (is (= 1
           (play-round [:club :king] [:heart :ace] [:club :ace]))))

  (testing "queens are higher rank than jacks"
    (is (= true
           (higher? [:spade :queen] [:club :jack]))))

  (testing "kings are higher rank than queens"
    (is (= true
           (higher? [:spade :king] [:club :queen]))))

  (testing "aces are higher rank than kings"
    (is (= true
           (higher? [:spade :ace] [:club :king]))))

  (testing "if the ranks are equal, clubs beat spades"
    (is (= true
           (higher? [:club 10] [:spade 10]))))

  (testing "if the ranks are equal, diamonds beat clubs"
    (is (= true
           (higher? [:diamond 10] [:club 10]))))

  (testing "if the ranks are equal, hearts beat diamonds"
    (is (= true
           (higher? [:heart 10] [:diamond 10])))))

(deftest test-play-game
  (testing "the player loses when they run out of cards"
    (is (= '(0)
           (play-game [] [[:heart 10]])))
    (is (= '(1)
           (play-game [[:heart 10]] [])))
    (is (= '(0 2)
           (play-game [] [[:heart 10]] [])))))

