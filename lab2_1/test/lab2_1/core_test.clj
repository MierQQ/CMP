(ns lab2-1.core-test
  (:require [clojure.test :refer :all]
            [lab2-1.core :refer :all]))

(def test-fx1 (fn [x] x))

(deftest count-square-test
  (testing "count-square from 0 to 1"
    (is (= 1/2 (count-square test-fx1 0 1))))
  (testing "count-square from 1 to 0"
    (is (= -1/2 (count-square test-fx1 1 0))))
  (testing "count-square from 0 to -1"
    (is (= 1/2 (count-square test-fx1 0 -1))))
  (testing "count-square from -1 to 0"
    (is (= -1/2 (count-square test-fx1 -1 0))))
  (testing "count-square from 0 to 100"
    (is (= 5000 (count-square test-fx1 0 100)))))

(def test-fx2 (fn [x] (+ x 5)))

(def test-integral-fx (integral test-fx2))

(deftest integral-test-1
  (testing "integral x = 1"
    (is (= 11/2 (test-integral-fx 1))))
  (testing "integral x = 10"
    (is (= 100 (test-integral-fx 10))))
  (testing "integral x = 20"
    (is (= 300 (test-integral-fx 20))))
  (testing "integral x = 0"
    (is (= 0 (test-integral-fx 0))))
  (testing "integral x = -1"
    (is (= -9/2 (test-integral-fx -1))))
  (testing "integral x = -10"
    (is (= 0 (test-integral-fx -10)))))



