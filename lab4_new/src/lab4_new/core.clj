(ns lab4-new.core 
  (:require [dnf.dnf :refer :all]))

(println "-----------------------")

(let [d (dnf_neg (dnf_conj
                  (dnf_neg
                   (dnf_impl
                    (dnf_neg (dnf_var :z))
                    (dnf_disj (dnf_var :x) (dnf_var :y))))
                  (dnf_disj (dnf_var :z) (dnf_var :v))))
      dnf_d (dnf d)]
  (dnf_print d)
  (dnf_print dnf_d))

(println "-----------------------")

(let [d (dnf_impl (dnf_var :x)
                  (dnf_impl (dnf_var :y)
                            (dnf_impl (dnf_var :z)
                                      (dnf_var :v))))
      dnf_d (dnf d)]
  (dnf_print d)
  (dnf_print dnf_d))

(println "-----------------------")

(let [d (dnf_conj
         (dnf_neg (dnf_conj (dnf_var :x) (dnf_var :y)))
         (dnf_conj (dnf_var :z) (dnf_var :v)))
      dnf_d (dnf d)]
  (dnf_print d)
  (dnf_print dnf_d))

(println "-----------------------")

(let [d (dnf_conj
         (dnf_neg (dnf_var :x))
         (dnf_var :x))
      dnf_d (dnf d)]
  (dnf_print d)
  (dnf_print dnf_d))



(println "-----------------------")

(let [d (dnf_disj
         (dnf_neg (dnf_var :x))
         (dnf_var :x))
      dnf_d (dnf d)]
  (dnf_print d)
  (dnf_print dnf_d))

(println "-----------------------")

(let [d (dnf_disj
         (dnf_const 1)
         (dnf_var :x))
      dnf_d (dnf d)]
  (dnf_print d)
  (dnf_print dnf_d))

(println "-----------------------")

(let [d (dnf_conj
         (dnf_const 0)
         (dnf_var :x))
      dnf_d (dnf d)]
  (dnf_print d)
  (dnf_print dnf_d))