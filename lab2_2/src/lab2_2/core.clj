(ns lab2-2.core)

(def delta 1/100) ;; delta

(defn count-square [f start end] ;; trap square
  (* (- end start) (/ (+ (f start) (f end)) 2)))

(def lf (memoize (fn
  ([f sign] (lf f sign (* sign delta)))
  ([f sign x] 
   (lazy-seq (cons (count-square f (- x (* sign delta)) x) (lf f sign (+ x (* sign delta)))))))))

(defn integral [f]
  (fn [x]
    (let [sign (if (> x 0) 1 -1)]
      (reduce + 0 (take (int (/ (* sign x) delta)) (lf f sign))))))


(def fx (fn [x] (+
                 (Math/sin x)
                 (Math/sin x)
                 (Math/sin x)
                 (Math/sin x)
                 (Math/sin x)
                 (Math/sin x)
                 (Math/sin x)
                 (Math/sin x)
                 (Math/sin x)
                 (Math/sin x)
                 (Math/sin x)
                 (Math/sin x)
                 (Math/sin x))))

(def ffx (fn [x] (+
                  (fx x)
                  (fx x)
                  (fx x)
                  (fx x)
                  (fx x)
                  (fx x)
                  (fx x)
                  (fx x)
                  (fx x)
                  (fx x)
                  (fx x))))

(def integral-ffx (integral ffx))

(time (integral-ffx 5000))
(time (integral-ffx 5000))
(time (integral-ffx 5001))
(time (integral-ffx 5002))
(time (integral-ffx 4999))
(time (integral-ffx 4998))