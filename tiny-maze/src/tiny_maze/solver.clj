(ns tiny-maze.solver)

(defn solve-maze [maze])

(defn rowcount [maze]
(count maze)
  )

(defn columncount [maze]
  (count (first maze))
)


(defn dimension [maze]
  (vector (rowcount maze) (columncount maze))
)

(defn equivalentmazes? [maze1 maze2]
  (= maze1 maze2)
  )

(defn checkupperleft [maze rows columns]
(cond
  (= (get-in maze [0 0]) 1) maze
  (= (+ (get-in maze [1 0]) (get-in maze [0 1])) 2) (assoc-in maze [0 0] 1)
  :else maze
  )
)

(defn checkupperright [maze rows columns]
(cond
  (= (get-in maze (vector 0 (- columns 1))) 1) maze
  (= (+ (get-in maze (vector 1 (- columns 1))) (get-in maze (vector 0 (- columns 2)))) 2) (assoc-in maze (vector 0 (- columns 1)) 1)
  :else maze
  ))

(defn checkbottomright [maze rows columns]
(cond
  (= (get-in maze (vector (- rows 1) (- columns 1))) 1) maze
  (= (+ (get-in maze (vector (- rows 2) (- columns 1))) (get-in maze (vector (- rows 1) (- columns 2)))) 2) (assoc-in maze (vector (- rows 1) (- columns 1)) 1)
  :else maze
  ))

(defn checkbottomleft [maze rows columns]
(cond
  (= (get-in maze (vector (- rows 1) 0)) 1) maze
  (= (+ (get-in maze (vector (- rows 2) 0)) (get-in maze (vector (- rows 1) 1))) 2) (assoc-in maze (vector (- rows 1) 0) 1)
  :else maze
  ))

(defn checkcorners [maze rows columns]
(reduce (fn [newmaze check-fn]
              (check-fn newmaze rows columns))
            maze
            [checkupperleft checkupperright checkbottomright checkbottomleft]))

(defn checkleftposition [maze rposition cposition]
  (cond
      (= (get-in maze (vector rposition cposition)) 1) maze
    (>= (+ (get-in maze (vector (- rposition 1) cposition)) (get-in maze (vector rposition (+ cposition 1))) (get-in maze (vector (+ rposition 1) cposition))) 2) (assoc-in maze (vector rposition cposition) 1)
    :else maze))

(defn checkleftedge [maze rows columns]
(reduce (fn [newmaze pos]
              (checkleftposition newmaze (first pos) 0))
            maze
        (for [x (range 1 (- rows 1))]
    [x 0])
        ))

(defn checkrightposition [maze rposition cposition]
  (cond
      (= (get-in maze (vector rposition cposition)) 1) maze
    (>= (+ (get-in maze (vector (- rposition 1) cposition)) (get-in maze (vector rposition (- cposition 1))) (get-in maze (vector (+ rposition 1) cposition))) 2) (assoc-in maze (vector rposition cposition) 1)
    :else maze))

(defn checkrightedge [maze rows columns]
(reduce (fn [newmaze pos]
              (checkrightposition newmaze (first pos) (- columns 1)))
            maze
        (for [x (range 1 (- rows 1))]
    [x (- columns 1)])
        ))

(defn checkbottomposition [maze rposition cposition]
  (cond
      (= (get-in maze (vector rposition cposition)) 1) maze
    (>= (+ (get-in maze (vector (- rposition 1) cposition)) (get-in maze (vector rposition (- cposition 1))) (get-in maze (vector rposition (+ cposition 1)))) 2) (assoc-in maze (vector rposition cposition) 1)
    :else maze))

(defn checkbottomedge [maze rows columns]
(reduce (fn [newmaze pos]
              (checkbottomposition newmaze (- rows 1) (second pos)))
            maze
        (for [x (range 1 (- columns 1))]
    [(- rows 1) x])
        ))

(defn checktopposition [maze rposition cposition]
  (cond
      (= (get-in maze (vector rposition cposition)) 1) maze
    (>= (+ (get-in maze (vector (+ rposition 1) cposition)) (get-in maze (vector rposition (- cposition 1))) (get-in maze (vector rposition (+ cposition 1)))) 2) (assoc-in maze (vector rposition cposition) 1)
    :else maze))

(defn checktopedge [maze rows columns]
(reduce (fn [newmaze pos]
              (checktopposition newmaze 0 (second pos)))
            maze
        (for [x (range 1 (- columns 1))]
    [0 x])
        ))

(defn checkedges [maze rows columns]
(reduce (fn [newmaze check-fn]
              (check-fn newmaze rows columns))
            maze
            [checkleftedge checkrightedge checkbottomedge checktopedge]))

(defn checkmiddleposition [maze rposition cposition]
  (cond
      (= (get-in maze (vector rposition cposition)) 1) maze
    (>= (+ (get-in maze (vector (+ rposition 1) cposition)) (get-in maze (vector (- rposition 1) cposition)) (get-in maze (vector rposition (- cposition 1))) (get-in maze (vector rposition (+ cposition 1)))) 3) (assoc-in maze (vector rposition cposition) 1)
    :else maze))

(defn checkmiddle [maze rows columns]
(reduce (fn [newmaze pos]
              (checkmiddleposition newmaze (first pos) (second pos)))
            maze
        (for [x (range 1 (- rows 1))
              y (range 1 (- columns 1))]
    [x y])
        ))


(defn check [maze rows columns]
(reduce (fn [newmaze check-fn]
              (check-fn newmaze rows columns))
            maze
            [checkcorners checkedges checkmiddle]))

(defn replaceEandSwith0 [maze]
  (into [] (map #(replace {:E 0 :S 0} %) maze)
  ))

(defn replace0withx [maze]
  (into [] (map #(replace {0 :x} %) maze)
  ))

(defn solve-maze [maze]
(let [r (rowcount maze)
      c (columncount maze)
      m (replaceEandSwith0 maze)]
  (loop [prevmaze [] currentmaze m]
    (cond
      (equivalentmazes? prevmaze currentmaze) (replace0withx currentmaze)
      :else (recur currentmaze (check currentmaze r c))
    ))))

