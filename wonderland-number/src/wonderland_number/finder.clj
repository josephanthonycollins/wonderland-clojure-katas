(ns wonderland-number.finder)

(defn AllTheSameDigits? [n1 n2]
  (let [s1 (set (str n1))
        s2 (set (str n2))]
    (= s1 s2)))

(defn wonderlandnumber? [num]
      (if (and (AllTheSameDigits? num (* 2 num))
               (AllTheSameDigits? num (* 3 num))
               (AllTheSameDigits? num (* 4 num))
               (AllTheSameDigits? num (* 5 num))
               (AllTheSameDigits? num (* 6 num))) true false))

(filter (fn [x] (wonderlandnumber? x)) (range 100000 900000))

(def wonderland-number
(first (filter (fn [x] (wonderlandnumber? x)) (range 100000 800000))))

(defn exp [x n]
  (reduce * (repeat n x)))

(defn cubedigits [num]
(reduce (fn [x y] (+ x (exp y 3))) 0 (map #(Character/digit % 10) (seq (str num)))))

(defn cubes []
(filter (fn [x] (= x (cubedigits x))) (range 1 1000)))

