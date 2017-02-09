(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))


(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))



(defn wordsofthesamelength [n dictionary]
 (filter (fn[x] (= n (count x))) dictionary))


(defn wordsimilaritymeasure [word1 word2]
 (if (not= (count (seq word1)) (count (seq word2))) 0
   (loop [w1 (seq word1) w2 (seq word2) n 0]
     (cond
       (or (empty? w1) (empty? w2)) n
       (= (first w1) (first w2)) (recur (rest w1) (rest w2) (inc n))
       :else  (recur (rest w1) (rest w2) n)))))


(defn wordanddistance [currentposition word start end]
(list word
      (wordsimilaritymeasure currentposition word)
      (wordsimilaritymeasure word start)
      (wordsimilaritymeasure word end)))


(defn choices [currentposition start end]
  (map #(wordanddistance currentposition % start end) (wordsofthesamelength (count currentposition) words)))


(defn nextword [currentposition start end sdistance edistance]
(let [v (choices currentposition start end)
      output '()]
(loop [inputlist v
       word (nth (first inputlist) 0)
       sd (nth (first inputlist) 2)
       ed (nth (first inputlist) 3)]
  (cond
    (= word end) word
    (empty? inputlist) output
    (and (= (wordsimilaritymeasure currentposition word) (- (count currentposition) 1)) (or (< sd sdistance) (> ed edistance))) word
    :else (recur
            (rest inputlist)
            (nth (first (rest inputlist)) 0)
            (nth (first (rest inputlist)) 2)
            (nth (first (rest inputlist)) 3))))))


(defn doublets [word1 word2]
 (let [size (count word1)
       dict (wordsofthesamelength size words)
       start word1
       end word2]
   (loop [currentword start
          sdistance (wordsimilaritymeasure currentword start)
          edistance (wordsimilaritymeasure currentword end)
          counter 0
          output []]
     (let [newword (nextword currentword start end sdistance edistance)]
       (cond
       (or (= newword end) (= currentword end)) (conj output currentword newword)
       (> counter (* 3 size)) []
       :else (recur
               newword
               (wordsimilaritymeasure newword start)
               (wordsimilaritymeasure newword end)
               (inc counter)
               (conj output currentword)))))))
