(ns lab3-2.core)

(defn split [nblock col]
  (if (<= nblock 0)
    nil
    (reverse (reduce #(cons (take nblock (drop %2 col)) %1) '() (range 0 (count col) nblock)))))

(defn my-subfilter [nblock pred col]
  (->> col
       (split nblock)
       (map #(future (doall (filter pred %))))
       (doall)
       (map deref)
       (doall)
       (reduce #(concat %1 %2) '())
       (doall)))

(defn my-filter [n-per pred col]
  (if (= 0 (count col))
    '()
    (lazy-cat
     (my-subfilter 1 pred (take n-per col))
     (my-filter n-per pred (drop n-per col)))))

(def r '(0 1 2 3 4 5 6 7 8 9 10 11))
(defn p [x]
  (Thread/sleep 1000)
  (= 0 (mod x 2)))

;;(time (doall (filter p r)))
;;(time (doall (my-filter 5 p r)))
;;(time (doall (take 10 (filter p (range)))))


;;(time (doall (take 10 (my-filter 5 p (range)))))


