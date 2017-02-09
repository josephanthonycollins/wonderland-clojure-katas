(ns card-game-war.game)

;; feel free to use these cards or use your own data structure
;;(def suits [:spade :club :diamond :heart])
;;(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])

(def suits [20 30 40 50])

(def ranks [2 3 4 5 6 7 8 9 10 11 12 13 14])

(def cards
  (for [suit suits
        rank ranks]
    [suit rank]))

(defn randomnumbergenerator [i]
  (take 1 (repeatedly #(rand-int (count i)))))

(defn whowinsround? [player1-card player2-card]
(cond
  (> (second player1-card) (second player2-card)) "Player1"
  (< (second player1-card) (second player2-card)) "Player2"
  (and (= (second player1-card) (second player2-card)) (> (first player1-card) (first player2-card))) "Player1"
  (and (= (second player1-card) (second player2-card)) (< (first player1-card) (first player2-card))) "Player2"
))


(defn shuffle [cards]
(loop [shuffledcards #{}]
  (let [c (into '() cards)
        x (first (randomnumbergenerator cards))
        element (nth c x)]
    (cond
      (= (count shuffledcards) (count cards)) shuffledcards
      :else (recur (conj shuffledcards element))
      )
    )))

(defn play-round [player1-cards player2-cards]
(let [p1 (first player1-cards)
      p2 (first player2-cards)
      winner (whowinsround? p1 p2)]
  (if (= "Player1" winner) [ (conj (into [] (disj (set player1-cards) p1)) p1 p2) (into [] (disj (set player2-cards) p2))]  [(into [] (disj (set player1-cards) p1)) (conj (into [] (disj (set player2-cards) p2)) p1 p2)])))

(defn splitdeck [cards]
  (let [p1cards (take (/ (count cards) 2) cards)
        p2cards (drop (/ (count cards) 2) cards)]
    [(into [] p1cards) (into [] p2cards)]))

(defn preparegame [cards]
  (-> cards (shuffle) (splitdeck)
  ))

(defn play-game [cards]
  (let [playercards (preparegame cards)
        p1cards (first playercards)
        p2cards (last playercards)]
    (loop [p1 p1cards p2 p2cards]
      (cond
         (empty? p1) "Game over, player 2 wins!"
         (empty? p2) "Game over, player 1 wins!"
        :else (recur (first (play-round p1 p2)) (last (play-round p1 p2)))
      )
)))

;;modify the play-round so that the winners hand also gets added to the bottom of the deck.
;;write the tests.
(play-game cards)
