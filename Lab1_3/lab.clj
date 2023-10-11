(defn myMap [f collection]
  (reverse (reduce 
   (fn [acc val] 
     (conj acc (f val)))
   '() collection)))

(defn myFilter [pred collection]
  (reverse (reduce 
   (fn [acc val] 
     (if (pred val) 
       (conj acc val) 
       acc)) 
   '() collection)))

(println '(-2 -1 0 1 2 3 4))
(println (myFilter (fn [x] (if (> x 0) true false)) '(-2 -1 0 1 2 3 4)))
(println (myMap (fn [x] (+ x 1)) '(-2 -1 0 1 2 3 4)))