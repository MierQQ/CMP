(ns lab3-1.core)

(defn split [nblock col]
  (if (<= nblock 0)
    nil
    (reverse (reduce #(cons (take nblock (drop %2 col)) %1) '() (range 0 (count col) nblock)))))

(defn my-filter [nblock pred col]
  (->> col
       (split nblock)
       (map #(future (doall (filter pred %))))
       (doall)
       (map deref)
       (doall)
       (reduce #(concat %1 %2) '())
       (doall)))

(def r '(0 1 2 3 4 5 6 7 8 9 10 11))
(defn p [x]
  (Thread/sleep 1000)
  (= 0 (mod x 2)))

(time (doall (filter p r)))
(time (my-filter 1 p r))
(time (my-filter 2 p r))
(time (my-filter 3 p r))
(time (my-filter 4 p r))
(time (my-filter 5 p r))
(time (my-filter 6 p r))
