(ns alphabet-cipher.coder)

(def alphabet (map char (range (int \a) (+ (int \z) 1))))

(defn rotate [n s]
 (loop [counter n output s]
   (cond
     (= 0 counter) output
     (> 0 counter) (recur (inc counter) (concat (vector (last output)) (take (- (count output) 1) output) ))
     (< 0 counter) (recur (dec counter) (concat (rest output) (vector (first output))))
     )
  )
)

(defn keywordstring [n val]
  (take n (cycle val)))

(defn keywordintegers [n val]
  (map #(- % 97) (map int (keywordstring n val))))

(defn messageintegers [val]
(map #(- % 97) (map int val)))

(defn encode [keyword message]
(let [vecoffsets (keywordintegers (count message) keyword)
      vecrotations (messageintegers message)]
  (apply str (map #(nth %1 %2) (map #(rotate % alphabet) vecrotations) vecoffsets))
)
)

(defn getindex [v s]
(loop [vs v ss s output []]
  (cond
    (or (empty? vs) (empty? ss)) output
    :else (recur (rest vs) (rest ss) (conj output (.indexOf (first vs) (first ss))))
    )
  )
)

(defn decode [keyword message]
(let [vecoffsets (keywordintegers (count message) keyword) vecstrings (map #(rotate % alphabet) vecoffsets)]
  (apply str (map #(nth alphabet %)
(getindex vecstrings message)))))

(decode "scones" "egsgqwtahuiljgs")


(defn decipher [cipher message]
  "decypherme")
