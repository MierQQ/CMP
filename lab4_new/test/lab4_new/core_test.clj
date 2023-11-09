(ns lab4-new.core-test
  (:require [clojure.test :refer :all]
            [lab4-new.core :refer :all]
            [dnf.dnf :refer :all]))

(deftest api_test
  (testing "dnf_type"
    (is (= :var (dnf_type (dnf_var :x))))
    (is (= :const (dnf_type (dnf_const 0))))
    (is (= :neg (dnf_type (dnf_neg (dnf_const 0)))))
    (is (= :disj (dnf_type (dnf_disj (dnf_var :x) (dnf_const 0)))))
    (is (= :conj (dnf_type (dnf_conj (dnf_var :x) (dnf_const 0)))))
    (is (= :impl (dnf_type (dnf_impl (dnf_var :x) (dnf_const 0))))))
  (let [x (dnf_var :x)
        y (dnf_var :y)
        zero (dnf_const 0)
        one (dnf_const 1)]
    (testing "dnf_args"
      (is (= (dnf_args (dnf_disj x y))
             (list x y)))
      (is (= (dnf_args (dnf_conj x y))
             (list x y)))
      (is (= (dnf_args (dnf_impl x y))
             (list x y)))
      (is (= (dnf_args (dnf_neg x))
             (list x))))
    (testing "dnf_arg_1"
      (is (= (dnf_arg_1 (dnf_disj x y))
             x))
      (is (= (dnf_arg_1 (dnf_conj x y))
             x))
      (is (= (dnf_arg_1 (dnf_impl x y))
             x))
      (is (= (dnf_arg_1 (dnf_neg x))
             x)))
    (testing "dnf_arg_2"
      (is (= (dnf_arg_2 (dnf_disj x y))
             y))
      (is (= (dnf_arg_2 (dnf_conj x y))
             y))
      (is (= (dnf_arg_2 (dnf_impl x y))
             y))
      (is (= (dnf_arg_2 (dnf_neg x))
             nil)))
    (testing "dnf_value"
      (is (= (dnf_value zero)
             false))
      (is (= (dnf_value one)
             true))

      (is (= (dnf_value x {:x true})
             true))
      (is (= (dnf_value x {:x false})
             false))

      (is (= (dnf_value (dnf_neg x) {:x true})
             false))
      (is (= (dnf_value (dnf_neg x) {:x false})
             true))

      (is (= (dnf_value (dnf_disj x y) {:x false :y false})
             false))
      (is (= (dnf_value (dnf_disj x y) {:x true :y false})
             true))
      (is (= (dnf_value (dnf_disj x y) {:x false :y true})
             true))
      (is (= (dnf_value (dnf_disj x y) {:x true :y true})
             true))

      (is (= (dnf_value (dnf_conj x y) {:x false :y false})
             false))
      (is (= (dnf_value (dnf_conj x y) {:x true :y false})
             false))
      (is (= (dnf_value (dnf_conj x y) {:x false :y true})
             false))
      (is (= (dnf_value (dnf_conj x y) {:x true :y true})
             true))

      (is (= (dnf_value (dnf_impl x y) {:x false :y false})
             true))
      (is (= (dnf_value (dnf_impl x y) {:x true :y false})
             false))
      (is (= (dnf_value (dnf_impl x y) {:x false :y true})
             true))
      (is (= (dnf_value (dnf_impl x y) {:x true :y true})
             true)))
    (testing "str value"
      (is (= (str :x) (dnf_str_form x)))
      (is (= (str :y) (dnf_str_form y)))
      (is (= (str 0) (dnf_str_form zero)))
      (is (= (str 1) (dnf_str_form one)))
      (is (= (str "~(:x)") (dnf_str_form (dnf_neg x))))
      (is (= (str "(:x+:y)") (dnf_str_form (dnf_disj x y))))
      (is (= (str "(:x*:y)") (dnf_str_form (dnf_conj x y))))
      (is (= (str "(:x->:y)") (dnf_str_form (dnf_impl x y)))))))
  
(deftest dnf_test
  (testing "dnf_1"
    (is (= (doall (for [x [true false]
                        y [true false]
                        z [true false]
                        v [true false]]
                    (let [d (dnf_neg (dnf_conj
                                      (dnf_neg
                                       (dnf_impl
                                        (dnf_neg (dnf_var :z))
                                        (dnf_disj (dnf_var :x) (dnf_var :y))))
                                      (dnf_disj (dnf_var :z) (dnf_var :v))))
                          dnf_d (dnf d)]
                      (= (dnf_value
                                   d
                                   {:x x :y y :z z :v v})
                                  (dnf_value
                                   dnf_d
                                   {:x x :y y :z z :v v})))))
         (list true true true true true true true true true true true true true true true true))))
  (testing "dnf_2"
    (is (= (doall (for [x [true false]
                        y [true false]
                        z [true false]
                        v [true false]]
                    (let [d (dnf_impl (dnf_var :x) 
                                      (dnf_impl (dnf_var :y)
                                                (dnf_impl (dnf_var :z) 
                                                          (dnf_var :v))))
                          dnf_d (dnf d)]
                      (= (dnf_value
                          d
                          {:x x :y y :z z :v v})
                         (dnf_value
                          dnf_d
                          {:x x :y y :z z :v v})))))
           (list true true true true true true true true true true true true true true true true))))
  (testing "dnf_3"
    (is (= (doall (for [x [true false]
                        y [true false]
                        z [true false]
                        v [true false]]
                    (let [d (dnf_conj 
                             (dnf_neg (dnf_conj (dnf_var :x) (dnf_var :y))) 
                             (dnf_conj (dnf_var :z) (dnf_var :v)))
                          dnf_d (dnf d)]
                      (= (dnf_value
                          d
                          {:x x :y y :z z :v v})
                         (dnf_value
                          dnf_d
                          {:x x :y y :z z :v v})))))
           (list true true true true true true true true true true true true true true true true))))
  (testing "dnf_4"
    (is (= (doall (for [x [true false]
                        y [true false]
                        z [true false]
                        v [true false]]
                    (let [d (dnf_conj 
                             (dnf_neg (dnf_var :x)) 
                             (dnf_var :x))
                          dnf_d (dnf d)]
                      (= (dnf_value
                          d
                          {:x x :y y :z z :v v})
                         (dnf_value
                          dnf_d
                          {:x x :y y :z z :v v})))))
           (list true true true true true true true true true true true true true true true true))))
  (testing "dnf_5"
    (is (= (doall (for [x [true false]
                        y [true false]
                        z [true false]
                        v [true false]]
                    (let [d (dnf_disj 
                             (dnf_neg (dnf_var :x)) 
                             (dnf_var :x))
                          dnf_d (dnf d)]
                      (= (dnf_value
                          d
                          {:x x :y y :z z :v v})
                         (dnf_value
                          dnf_d
                          {:x x :y y :z z :v v})))))
           (list true true true true true true true true true true true true true true true true)))))
