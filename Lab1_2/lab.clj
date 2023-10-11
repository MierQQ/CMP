(defn rearrangement
  "Rearrangment of alphabet letters in n length word without sequentially repeating letters"
  ([x n]
   (rearrangement x n x))
  ([x n acc]
   (if (= n 1)
     (apply list acc)
     (recur x (- n 1) (reduce
                               (fn [a1 v1]
                                 (concat a1
                                         (reduce
                                          (fn [a2 v2]
                                            (if (= (str (last v1)) (str (first v2)))
                                              a2
                                              (conj a2 (str v1 v2)))) [] acc))) [] x)))))

(println (rearrangement ["a" "b" "c"] 2))
(println (rearrangement ["a" "b" "c"] 3))
(println (rearrangement ["a" "b"] 1))
(println (rearrangement ["a" "b"] 2))
(println (rearrangement ["a" "b"] 100))
(println (rearrangement ["ab" "bc"] 3))
(println (rearrangement ["a"] 2))