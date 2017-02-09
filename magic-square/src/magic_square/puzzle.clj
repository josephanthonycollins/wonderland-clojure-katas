(ns magic-square.puzzle)


(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])


(defn permutations [colls]
  (if (= 1 (count colls))
    (list colls)
    (for [head colls
          tail (permutations (disj (set colls) head))]
      (cons head tail))))

(defn permutationinto3by3vector [colls]
(vec (map #(into [] %) (partition 3 colls))))


(defn sumrows [m]
  (map #(reduce + %) m))


(defn sumcols [m]
  [(reduce + (map first m))
   (reduce + (map second m))
   (reduce + (map last m))])


(defn sumdiagonals [m]
  [(+ (get-in m [0 0]) (get-in m [1 1]) (get-in m [2 2]))
   (+ (get-in m [2 0]) (get-in m [1 1]) (get-in m [0 2]))])


(defn ismagicsquare? [m]
(if (and (= (set (sumrows m)) (set (sumcols m)) (set (sumdiagonals m)))
      (= 1  (count (set (sumrows m))))) true false))

(defn magic-square [coll]
  (let [p (permutations coll)]
  (permutationinto3by3vector (first (filter (fn [x]  (ismagicsquare? (permutationinto3by3vector x))) p))
  )))

