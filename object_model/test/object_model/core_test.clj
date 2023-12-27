(ns object-model.core-test
  (:require [clojure.test :refer :all]
            [object-model.core :refer :all]))

(deftest object-creation
  (testing "object creating and slot access"
    (let [x (defclass 'class (list) (list
                                     (slot 'a :public)
                                     (slot 'b :private)
                                     (slot 'c :public "default c")
                                     (slot 'd :private "defailt d")
                                     (slot 'undefined :public)
                                     (slot 'setvalue :public "no")))
          obj (make-instance 'class {'a "defined a"
                                     'b "defined b"})
          x (set-value obj 'setvalue "yes")]
      (is (= "defined a" (get-value obj 'a)))
      (is (thrown? NullPointerException (get-value obj 'b)))
      (is (= "default c" (get-value obj 'c))) 
      (is (thrown? NullPointerException (get-value obj 'd)))
      (is (thrown? NullPointerException (set-value obj 'd "unaccesable")))
      (is (= nil (get-value obj 'undefined))) 
      (is (= "yes" (get-value obj 'setvalue))))
    (let [x (defclass 'class (list) (list
                                     (slot 'a :public)
                                     (slot 'b :private)
                                     (slot 'c :public "default c")
                                     (slot 'd :private "default d")
                                     (slot 'undefined :public)))
          obj (make-instance 'class {'a "defined a"
                                     'b "defined b"})
          obj (assoc obj :private true)]
      (is (= "defined a" (get-value obj 'a)))
      (is (= "defined b" (get-value obj 'b)))
      (is (= "default c" (get-value obj 'c)))
      (is (= "default d" (get-value obj 'd)))
      (is (= nil (get-value obj 'undefined)))))
  (testing "multi inheritance slot access"
    (let [x (defclass 'base1 (list) (list
                                     (slot 'a :public "base1 a")
                                     (slot 'b :private "base1 b")))
          x (defclass 'base2 (list 'base1) (list
                                     (slot 'a :public "base2 a")
                                     (slot 'b :private "base2 b")))
          x (defclass 'base3 (list 'base2) (list
                                     (slot 'a :public "base3 a")
                                     (slot 'b :private "base3 b")))
          x (defclass 'class (list 'base2 'base3) (list
                                     (slot 'a :public "class a")
                                     (slot 'b :private "class b")))
          obj (make-instance 'class {})]
      (is (= "class a" (get-value obj 'a)))
      (is (= "base3 a" (get-value obj 'a 'base3)))
      (is (= "base2 a" (get-value obj 'a 'base2)))
      (is (= "base1 a" (get-value obj 'a 'base1)))
      (is (thrown? NullPointerException (get-value obj 'b)))
      (is (thrown? NullPointerException (get-value obj 'b 'base3)))
      (is (thrown? NullPointerException (get-value obj 'b 'base2)))
      (is (thrown? NullPointerException (get-value obj 'b 'base1))))
      (let [x (defclass 'base1 (list) (list
                                       (slot 'a :public "base1 a")
                                       (slot 'b :private "base1 b")))
            x (defclass 'base2 (list 'base1) (list
                                              (slot 'a :public "base2 a")
                                              (slot 'b :private "base2 b")))
            x (defclass 'base3 (list 'base2) (list
                                              (slot 'a :public "base3 a")
                                              (slot 'b :private "base3 b")))
            x (defclass 'class (list 'base2 'base3) (list
                                                     (slot 'a :public "class a")
                                                     (slot 'b :private "class b")))
            obj (make-instance 'class {})
            obj (assoc obj :private true)]
        (is (= "class a" (get-value obj 'a)))
        (is (= "base3 a" (get-value obj 'a 'base3)))
        (is (= "base2 a" (get-value obj 'a 'base2)))
        (is (= "base1 a" (get-value obj 'a 'base1)))
        (is (= "class b" (get-value obj 'b)))
        (is (= "base3 b" (get-value obj 'b 'base3)))
        (is (= "base2 b" (get-value obj 'b 'base2)))
        (is (= "base1 b" (get-value obj 'b 'base1))))))

(deftest method-call
  (testing "method calls"
    (let [x (defclass 'base1 (list) (list
                                     (slot 'a :public "base1 a")
                                     (slot 'b :private "base1 b")))
          x (defclass 'base2 (list 'base1) (list
                                            (slot 'a :public "base2 a")
                                            (slot 'b :private "base2 b")))
          x (defclass 'base3 (list 'base2) (list
                                            (slot 'a :public "base3 a")
                                            (slot 'b :private "base3 b")))
          x (defclass 'class (list 'base2 'base3) (list
                                                   (slot 'a :public "class a")
                                                   (slot 'b :private "class b")))
          obj-class (make-instance 'class {})
          obj-base1 (make-instance 'base1 {})
          obj-base2 (make-instance 'base2 {})
          obj-base3 (make-instance 'base3 {})
          x (defclass 'gen (list) (list))
          g (make-instance 'gen {})
          x (register-method 'call1 :query :main ['generic-type] (fn [x] "generic"))
          x (register-method 'call1 :query :main ['base1] (fn [x] (str "base 1 " (get-value x 'a) " " (get-value x 'b))))
          x (register-method 'call1 :query :main ['base2] (fn [x] (str "base 2 " (get-value x 'a) " " (get-value x 'b))))
          x (register-method 'call1 :query :main ['base3] (fn [x] (str "base 3 " (get-value x 'a) " " (get-value x 'b))))
          x (register-method 'call1 :query :main ['class] (fn [x] (str "class " (get-value x 'a) " " (get-value x 'b))))
          x (register-method 'call2 :query :main ['base1] (fn [x] (str "base 1 " (get-value x 'a) " " (get-value x 'b))))
          x (register-method 'call3 :query :main ['class] (fn [x]
                                                              (do 
                                                                (set-value x 'a "class changed")
                                                                (get-value x 'a))))
          x (register-method 'call4 :command :main ['class] (fn [x]
                                                            (do
                                                              (set-value x 'a "class changed")
                                                              (get-value x 'a))))
          x (register-method 'call5 :query :main ['class] (fn [x]
                                                              (let [ctx1 (call-next-method 'call5 [x])
                                                                    ctx2 (call-next-method ctx1)
                                                                    ctx3 (call-next-method ctx2)]
                                                                (list
                                                                 "class"
                                                                 (call-next-method-result ctx1)
                                                                 (call-next-method-result ctx2)
                                                                 (call-next-method-result ctx3))))) 
          x (register-method 'call5 :query :main ['base1] (fn [x] "base1"))
          x (register-method 'call5 :query :main ['base2] (fn [x] "base2"))
          x (register-method 'call5 :query :main ['base3] (fn [x] "base3"))]
     (is (= "generic" (dispatch-method 'call1 [g])))
     (is (= "class class a class b" (dispatch-method 'call1 [obj-class])))
     (is (= "base 1 base1 a base1 b" (dispatch-method 'call1 [obj-base1])))
     (is (= "base 2 base2 a base2 b" (dispatch-method 'call1 [obj-base2])))
     (is (= "base 3 base3 a base3 b" (dispatch-method 'call1 [obj-base3])))
     (is (= "base 1 class a class b" (dispatch-method 'call2 [obj-class])))
     (is (= "class changed" (dispatch-method 'call3 [obj-class])))
     (is (= "class a" (get-value obj-class 'a)))
     (is (= "class changed" (dispatch-method 'call4 [obj-class])))
     (is (= "class changed" (get-value obj-class 'a)))
     (is (= (list "class" "base2" "base3" "base1") (dispatch-method 'call5 [obj-class])))
     ))) 
