(ns dnf.dnf)

(defn dnf_str_form [expr]
  (:strform expr))

(defn dnf_print [expr]
  (println (dnf_str_form expr)))

(defn dnf_type [expr]
  (:type expr))

(defn dnf_args [expr]
  (:args expr))

(defn dnf_arg_1 [expr]
  (first (dnf_args expr)))

(defn dnf_arg_2 [expr]
  (second (dnf_args expr)))

(defn dnf_value
  ([expr args]
   ((:value expr) args))
  ([expr]
   ((:value expr) {})))

(defn dnf_apply [expr f]
  ((:apply expr) f))

;;-----------------------------

(defn dnf_var [name]
  {:pre [(keyword? name)]}
  {:type :var
   :args (list name)
   :strform (str name)
   :value #(if (name %1) true false)
   :apply (fn [f]
            (dnf_var name))})

(defn dnf_const [num]
  {:pre [(or (= 1 num) (= 0 num))]}
  {:type :const
   :args (list num)
   :strform (str num)
   :value (fn [x]
            (if (= num 1) true false))
   :apply (fn [f]
            (dnf_const (f num)))})

(defn dnf_conj [arg1 arg2]
  {:type :conj
   :args (list arg1 arg2)
   :strform (str "(" (dnf_str_form arg1) "*" (dnf_str_form arg2) ")") 
   :value #(and (dnf_value arg1 %1) (dnf_value arg2 %1))
   :apply #(dnf_conj (%1 arg1) (%1 arg2))})

(defn dnf_disj [arg1 arg2]
  {:type :disj
   :args (list arg1 arg2)
   :strform (str "(" (dnf_str_form arg1) "+" (dnf_str_form arg2) ")") 
   :value #(or (dnf_value arg1 %1) (dnf_value arg2 %1))
   :apply #(dnf_disj (%1 arg1) (%1 arg2))})

(defn dnf_neg [arg]
  {:type :neg
   :args (list arg)
   :strform (str "~(" (dnf_str_form arg) ")") 
   :value #(not (dnf_value arg %1))
   :apply #(dnf_neg (%1 arg))})

;;-----------------------------

;; a1a2: 00 01 10 11
(defn dnf_simple [expr]
  (let [table (:table expr)
        a1 (dnf_arg_1 expr)
        a2 (dnf_arg_2 expr)
        na1 (dnf_neg (dnf_arg_1 expr))
        na2 (dnf_neg (dnf_arg_2 expr))]
    (if (not table) nil
        (case table
          [0 0 0 0] (dnf_const 0)

          [0 0 0 1] (dnf_conj a1 a2)   ;;11
          [0 0 1 0] (dnf_conj a1 na2)  ;;10
          [0 1 0 0] (dnf_conj na1 a2)  ;;01
          [1 0 0 0] (dnf_conj na1 na2) ;;00

          [0 0 1 1] (dnf_disj (dnf_conj a1 a2)   ;;11
                              (dnf_conj a1 na2)) ;;10
          [0 1 0 1] (dnf_disj (dnf_conj a1 a2)   ;;11
                              (dnf_conj na1 a2)) ;;01
          [0 1 1 0] (dnf_disj (dnf_conj a1 na2)  ;;10
                              (dnf_conj na1 a2)) ;;01
          [1 0 0 1] (dnf_disj (dnf_conj a1 a2)   ;;11
                              (dnf_conj na1 na2));;00
          [1 0 1 0] (dnf_disj (dnf_conj a1 na2)  ;;10
                              (dnf_conj na1 na2));;00
          [1 1 0 0] (dnf_disj (dnf_conj na1 a2)  ;;01
                              (dnf_conj na1 na2));;00

          [0 1 1 1] (dnf_disj a1 a2)
          [1 0 1 1] (dnf_disj a1 na2)
          [1 1 0 1] (dnf_disj na1 a2)
          [1 1 1 0] (dnf_disj na1 na2)

          [1 1 1 1] (dnf_const 1)
          expr))))

(defn dnf_impl [arg1 arg2]
  {:type :impl
   :args (list arg1 arg2)
   :strform (str "(" (dnf_str_form arg1) "->" (dnf_str_form arg2) ")") 
   :value #(dnf_value (dnf_simple (dnf_impl arg1 arg2)) %1)
   :table '[1 1 0 1]})

;;---------------------------------

(defn dnf_first_step [expr]
  (case (dnf_type expr)
    :var expr
    :const expr
    :conj (dnf_apply expr dnf_first_step)
    :disj (dnf_apply expr dnf_first_step)
    :neg (dnf_apply expr dnf_first_step)
    (dnf_apply (dnf_simple expr) dnf_first_step)))

(defn dnf_second_step [expr]
  ;;(println expr (dnf_type (dnf_arg_1 (dnf_arg_1 expr))) (dnf_type expr) (dnf_str_form expr)  (dnf_type (dnf_arg_1 expr)))
  (if (or (= 1 expr) (= 0 expr))
    expr
    (case (dnf_type expr)
      :var expr
      :const expr
      :conj (dnf_apply expr dnf_second_step)
      :disj (dnf_apply expr dnf_second_step)
      :neg (case (dnf_type (dnf_arg_1 expr))
             :conj (dnf_apply
                    (dnf_disj (dnf_neg (dnf_arg_1 (dnf_arg_1 expr)))
                              (dnf_neg (dnf_arg_2 (dnf_arg_1 expr))))
                    dnf_second_step)
             :disj (dnf_apply
                    (dnf_conj (dnf_neg (dnf_arg_1 (dnf_arg_1 expr)))
                              (dnf_neg (dnf_arg_2 (dnf_arg_1 expr))))
                    dnf_second_step)
             :neg (dnf_apply (dnf_arg_1 (dnf_arg_1 expr)) dnf_second_step)
             :const (dnf_apply (dnf_arg_1 expr) #(if (= 0 %1) 1 0))
             expr))))

(defn dnf_third_step [expr]
  ;;(dnf_print expr)
  ;;(println (dnf_type expr) (dnf_type (dnf_arg_1 expr)) (dnf_type (dnf_arg_2 expr)))
  (case (dnf_type expr)
    :var expr
    :const expr
    :conj (let [a1 (dnf_arg_1 expr)
                a2 (dnf_arg_2 expr)]
            (if (= :disj (dnf_type a1))
              (dnf_apply
               (dnf_disj
                (dnf_conj (dnf_arg_1 a1) a2)
                (dnf_conj (dnf_arg_2 a1) a2))
               dnf_third_step)
              (if (= :disj (dnf_type a2))
                (dnf_apply
                 (dnf_disj
                  (dnf_conj (dnf_arg_1 a2) a1)
                  (dnf_conj (dnf_arg_2 a2) a1))
                 dnf_third_step)
                (dnf_apply expr dnf_third_step))))
    :disj (dnf_apply expr dnf_third_step)
    :neg expr))

;;---------------------------------

(defn dnf_elem_conj 
  ([pos neg consts]
   (let [const_value (reduce #(if (= 1 %1 %2) 1 0) 1 consts)
         pos_set (into #{} (map dnf_arg_1 pos))
         neg_set (into #{} (map dnf_arg_1 neg))
         var_value (if (empty? (concat pos_set neg_set))
                     (list (dnf_const const_value))
                     (if (and (= 1 const_value)
                              (= (+ (count pos_set) (count neg_set))
                                 (count (into #{} (concat pos_set neg_set)))))
                       (reduce #(conj %1 (dnf_neg (dnf_var %2))) (map dnf_var pos_set) neg_set)
                       (list (dnf_const 0))))]
     {:type :elem_conj
      :args var_value
      :strform (str (dnf_str_form (first var_value))
                    (reduce #(str %1 "*" (dnf_str_form %2)) "" (rest var_value))) 
      :value #(reduce (fn [acc val]
                        (and acc (dnf_value val %1))) true var_value)
      :apply (fn [f]
               (dnf_elem_conj (map f var_value)))}))
  ([args]
   {:type :elem_conj
    :args args
    :strform (str (dnf_str_form (first args))
                  (reduce #(str %1 "*" (dnf_str_form %2)) "" (rest args))) 
    :value #(reduce (fn [acc val]
                      (and acc (dnf_value val %1))) true args)
    :apply (fn [f]
             (dnf_elem_conj (map f args)))}))

(defn dnf_get_pos_neg_consts [expr]
  (case (dnf_type expr)
    :var {:pos (list expr)
          :negative ()
          :consts ()}
    :const {:pos ()
            :negative ()
            :consts (list (if (dnf_value expr) 1 0))}
    :neg {:pos ()
          :negative (list (dnf_arg_1 expr))
          :consts ()}
    :conj {:pos (concat
                 (:pos (dnf_get_pos_neg_consts (dnf_arg_1 expr)))
                 (:pos (dnf_get_pos_neg_consts (dnf_arg_2 expr))))
           :negative (concat
                      (:negative (dnf_get_pos_neg_consts (dnf_arg_1 expr)))
                      (:negative (dnf_get_pos_neg_consts (dnf_arg_2 expr))))
           :consts (concat
                    (:consts (dnf_get_pos_neg_consts (dnf_arg_1 expr)))
                    (:consts (dnf_get_pos_neg_consts (dnf_arg_2 expr))))}))

(defn dnf_get_elem_conj [expr]
  (let [pnc (dnf_get_pos_neg_consts expr)
        pos (:pos pnc)
        neg (:negative pnc)
        consts (:consts pnc)]
    (dnf_elem_conj pos neg consts)))

(defn dnf_get_elem_conjs [expr]
  (case (dnf_type expr)
    :disj (concat
           (dnf_get_elem_conjs (dnf_arg_1 expr))
           (dnf_get_elem_conjs (dnf_arg_2 expr)))
    (list (dnf_get_elem_conj expr))))

(defn dnf_norm [args_raw]
  (let [args (->>
              args_raw
              (filter #(if (and (= :const (dnf_type (dnf_arg_1 %1)))
                               (= false (dnf_value (dnf_arg_1 %1))))
                        false
                        true))
              (#(if (empty? %1) (list (dnf_elem_conj (list (dnf_const 0)))) %1))
              (#(reduce (fn [acc val]
                          (if (and (= :const (dnf_type (dnf_arg_1 val)))
                                   (= true (dnf_value (dnf_arg_1 val))))
                            (list (dnf_elem_conj (list (dnf_const 1))))
                            acc)) %1 %1)))]
    {:type :dnf
     :args args
     :strform (str (dnf_str_form (first args))
                   (reduce #(str %1 "+" (dnf_str_form %2)) "" (rest args)))
     :value #(reduce (fn [acc val]
                       (or acc val))
                     false
                     (map
                      (fn [x]
                        (dnf_value x %1))
                      args))
     :apply (fn [f]
              (dnf_norm (map f args)))}))

(defn dnf_to_norm [expr]
  (dnf_norm (dnf_get_elem_conjs expr)))

;;------------------------------------

(defn dnf [expr]
  (-> expr
      dnf_first_step
      dnf_second_step
      dnf_third_step
      dnf_to_norm))
