(ns lab3-1.core-test
  (:require [clojure.test :refer :all]
            [lab3-1.core :refer :all]))

(def col '(0 1 2 3 4 5 6 7 8 9 10 11))

(deftest split-test
  (testing "split test 1"
    (is (= nil (split 0 col))))
  (testing "split test 2"
    (is (= '((0) (1) (2) (3) (4) (5) (6) (7) (8) (9) (10) (11)) (split 1 col))))
  (testing "split test 3"
    (is (= '((0 1) (2 3) (4 5) (6 7) (8 9) (10 11)) (split 2 col))))
  (testing "split test 4"
    (is (= '((0 1 2) (3 4 5) (6 7 8) (9 10 11)) (split 3 col))))
  (testing "split test 5"
    (is (= '((0 1 2 3) (4 5 6 7) (8 9 10 11)) (split 4 col))))
  (testing "split test 6"
    (is (= '((0 1 2 3 4) (5 6 7 8 9) (10 11)) (split 5 col))))
  (testing "split test 7"
    (is (= '((0 1 2 3 4 5 6 7 8 9 10 11)) (split 12 col))))
  (testing "split test 7"
    (is (= '((0 1 2 3 4 5 6 7 8 9 10 11)) (split 15 col)))))

(def pred #(= 0 (mod % 2)))

(deftest filter-test
  (testing "filter test 1"
    (is (= (filter pred col) (my-filter 1 pred col))))
  (testing "filter test 2"
    (is (= (filter pred col) (my-filter 2 pred col))))
  (testing "filter test 3"
    (is (= (filter pred col) (my-filter 3 pred col))))
  (testing "filter test 4"
    (is (= (filter pred col) (my-filter 4 pred col)))))


