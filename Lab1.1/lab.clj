(defn rearrangement
  "Rearrangment of alphabet letters in n length word without sequentially repeating letters"
  [x n]
  (if (= n 1)
    x
    (let [rv (rearrangement x (- n 1))]
      (reduce
       (fn [a1 v1]
         (concat a1
                 (reduce
                  (fn [a2 v2]
                    (if (= (str (last v1)) (str (first v2)))
                      a2
                      (conj
                       a2
                       (str v1 v2))))
                  [] rv)))
       [] x))))

(println (rearrangement ["a" "b" "c"] 2))