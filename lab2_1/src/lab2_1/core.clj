(ns lab2-1.core)

(def delta 1/100) ;; delta

(defn count-square [f start end] ;; trap square
  (* (- end start) (/ (+ (f start) (f end)) 2)))

(def count-square-memo (memoize count-square)) ;; square memo

(def f-memo (memoize (fn [f] (memoize f)))) ;; memoize points

(defn integral [f]
  (memoize (fn [x]
             (let [sign (if (> x 0) 1 -1)
                   sign-delta (* delta sign)
                   xs (map #(+ sign-delta (* sign-delta %1))
                           (take (int (/ (* sign x) delta)) (range)))]
               (reduce (fn [acc val]
                         (+ acc (count-square-memo (f-memo f) (- val (* sign delta)) val))) 0 xs)))))

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