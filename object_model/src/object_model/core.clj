(ns object-model.core)

(def obj-model-classes (ref {}));; variable with declared classes
(def obj-model-methods (ref {}));; variable with declared methods

(defn register-class
  "Registers class"
  [class-obj]
  (dosync
   (alter obj-model-classes assoc (:name class-obj) class-obj)))

(defn class-registed?
  "Return true if class registred"
  [class]
  {:pre [(symbol? class)]}
  (some? (find (deref obj-model-classes) class)))

(defn is-slot?
  "If it's slot, returns true"
  [slot]
  (= :slot (:type slot)))

(defn is-slot-public?
  "returns true if slot is public"
  [slot]
  (= :public (:access slot)))

(defn get-slot-name
  "returns slot name"
  [slot]
  (:name slot))

(defn get-slot-default
  "return default value"
  [slot]
  (:default slot))


(defn slot-list-to-map
  "convert slot list to map with keys :public-slots and :slots,
   public slot name list and slot map with name as key respectevly"
  [slot-list]
  (let [public-slots-list (reduce #(if (is-slot-public? %2) (conj %1 (get-slot-name %2)) %1)
                                  (list) slot-list)
        hash-map-slot (reduce #(assoc %1 (get-slot-name %2) %2) {} slot-list)]
    {:public-slots public-slots-list
     :slots hash-map-slot}))

(defn slot
  "return slot, args are name, acess-type and default-value, default default-value is nil"
  ([slot-name access-type]
   {:pre [(symbol? slot-name)
          (or (= :private access-type)
              (= :public access-type))]}
   (slot slot-name access-type nil))
  ([slot-name access-type default-value]
   {:pre [(symbol? slot-name)
          (or (= :private access-type)
              (= :public access-type))]}
   {:type :slot
    :name slot-name
    :access access-type
    :default default-value}))

(defn get-class-parents
  "return parents of this class"
  [class-name]
  {:pre [(symbol? class-name)
         (class-registed? class-name)]}
  (:parents (class-name (deref obj-model-classes))))

(defn get-class-order
  "returns class order in bfs order"
  [class-name]
  {:pre [(symbol? class-name)
         (class-registed? class-name)]}
  (loop [queue (list class-name)
         ctx (list class-name)]
    (cond
      (empty? queue) ctx
      :else (let [current (first queue)
                  cleared-parents (reduce #(if (.contains ctx %2) %1 (concat %1 (list %2))) '() (get-class-parents current))
                  next-queue (concat (rest queue) cleared-parents)
                  next-ctx (concat ctx cleared-parents)]
              (recur next-queue next-ctx)))))

(defn get-class-slots
  "returns slots only from this class"
  [class-name]
  {:pre [(symbol? class-name)
         (class-registed? class-name)]}
  (:slots ((deref obj-model-classes) class-name)))

(defn get-class-slot
  "returns slot form class-name and slot-name"
  [class-name slot-name]
  {:pre [(symbol? class-name)
         (class-registed? class-name)
         (symbol? slot-name)]}
  (slot-name (get-class-slots class-name)))

(defn defclass
  "define class, gets class name, parent list and list of slots, 
   class name and parent list elements are keyword"
  [class-name parent-list-names slot-list]
  {:pre [(symbol? class-name)
         (list? parent-list-names)
         (every? symbol? parent-list-names)
         (every? class-registed? parent-list-names)
         (every? #(not (.contains %1 class-name)) (map get-class-order parent-list-names))
         (list? slot-list)
         (every? is-slot? slot-list)]}
  (let [info {:type :class
              :name class-name
              :parents parent-list-names}
        slot-map (slot-list-to-map slot-list)]
    (register-class (merge info slot-map))))

(defn make-slots-instance
  "makes instances "
  [class-name args class-args]
  (let [class-order (get-class-order class-name)
        result (reduce
                (fn [acc class]
                  (assoc
                   acc
                   class
                   (reduce
                    #(assoc
                      %1
                      (first %2)
                      (ref (cond
                             (and (some? (class-args class))
                                  (some? ((class-args class) (first %2)))) ((class-args class) (first %2))
                             (some? (args (first %2))) (args (first %2))
                             :else (get-slot-default (second %2)))))
                    {} (get-class-slots class))))
                {}
                class-order)]
    result))

(defn make-instance
  "provide initvalues for slots and for parents or default value will be assigned"
  ([class-name args]
   {:pre [(symbol? class-name)
          (class-registed? class-name)
          (map? args)]}
   (make-instance class-name args {}))
  ([class-name args class-args]
   {:pre [(symbol? class-name)
          (class-registed? class-name)
          (map? args)]}
   (make-instance class-name args class-args (make-slots-instance class-name args class-args)))
  ([class-name args class-args ctx]
   {:pre [(symbol? class-name)
          (class-registed? class-name)
          (map? args)]}
   {:type :obj
    :class-name class-name
    :super (reduce #(assoc %1 %2 (make-instance %2 args class-args ctx)) {} (get-class-parents class-name))
    :slots (ctx class-name)}))

(defn is-object?
  "return true if objet passed"
  [obj]
  (= :obj (:type obj)))

(defn get-object-class
  "returns object class"
  [obj]
  {:pre [(is-object? obj)]}
  (:class-name obj))

(defn get-object-parents-objects
  "returns parent objects of this object"
  [obj]
  {:pre [(is-object? obj)]}
  (reduce #(conj %1 (second %2)) (list) (:super obj)))

(defn get-object-slot
  "returns map of privacy and slots with class-names as a key"
  [obj slot-name]
  {:pre [(is-object? obj)]}
  (reduce #(merge %2 %1)
          {(get-object-class obj) [(is-slot-public? (get-class-slot (get-object-class obj) slot-name)) ((:slots obj) slot-name)]}
          (map #(get-object-slot %1 slot-name) (get-object-parents-objects obj))))

(defn get-object-class-order
  "returns class order for the object"
  [obj]
  {:pre [(is-object? obj)]}
  (get-class-order (get-object-class obj)))

(defn get-value
  "access to object slot, if slot is private and didn't called from method, throws nullpointer exception"
  ([obj slot-name]
   {:pre [(is-object? obj)
          (symbol? slot-name)]}
   (let [slots (get-object-slot obj slot-name)
         order (get-object-class-order obj)
         slot-with-access (reduce #(if (and (not (some? %1)) (some? (second (slots %2)))) (slots %2) %1) nil order)
         private-level-of-access (some? (:private obj))
         can-access (and (some? slot-with-access)
                         (or private-level-of-access
                             (first slot-with-access)))
         slot (if can-access (second slot-with-access) nil)]
     (deref slot)))
  ([obj slot-name class-name]
   {:pre [(is-object? obj)
          (symbol? slot-name)
          (symbol? class-name)]}
   (let [slots (get-object-slot obj slot-name)
         slot-with-access (slots class-name)
         private-level-of-access (some? (:private obj))
         can-access (and (some? slot-with-access)
                         (or private-level-of-access
                             (first slot-with-access)))
         slot (if can-access (second slot-with-access) nil)]
     (deref slot))))

(defn set-value
  "sets slot value, if slot is private and didn't called from method, throws nullpointer exception"
  ([obj slot-name value]
   {:pre [(is-object? obj)
          (symbol? slot-name)]}
   (let [slots (get-object-slot obj slot-name)
         order (get-object-class-order obj)
         slot-with-access (reduce #(if (and (not (some? %1)) (some? (second (slots %2)))) (slots %2) %1) nil order) 
         private-level-of-access (some? (:private obj))

         can-access (and (some? slot-with-access)
                         (or private-level-of-access
                             (first slot-with-access)))
         slot (if can-access (second slot-with-access) nil)]
     (dosync
      (alter slot (fn [slot value] value) value)
      obj)))
  ([obj slot-name value class-name]
   {:pre [(is-object? obj)
          (symbol? slot-name)
          (symbol? class-name)]}
   (let [slots (get-object-slot obj slot-name)
         slot-with-access (slots class-name)
         private-level-of-access (some? (:private obj))
         can-access (and (some? slot-with-access)
                         (or private-level-of-access
                             (first slot-with-access)))
         slot (if can-access (second slot-with-access) nil)]
     (dosync
      (alter slot (fn [slot value] value) value)
      obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;---methodname -> types (generic < types) -> before main and after

(defn is-method-name-exist?
  "return true if there is a method woth such name"
  [method-name]
  {:pre [(symbol? method-name)]}
  (if (some? ((deref obj-model-methods) method-name))
    true
    false))

(defn get-method-name-branch-ref
  "returns ref to a method branch"
  [method-name]
  {:pre [(symbol? method-name)]}
  ((deref obj-model-methods) method-name))

(defn is-allowed-type?
  "check of type 
   allowed types: 
   logic 'bool-type
   num 'number-type
   str 'str-type
   symbols 'symbol-type
   keywords 'keyword-type
   collection 'collection
   generic type 'generic-type
   classes that is registred"
  [type]
  (if (or (= type 'bool-type)
          (= type 'number-type)
          (= type 'str-type)
          (= type 'symbol-type)
          (= type 'keyword-type)
          (= type 'collection)
          (= type 'generic-type)
          (class-registed? type))
    true
    false))

(defn register-method
  "register method. Args are method name, type of access,
  [specificator (:before,:main,:after)], arg types vector, function"
  ([method-name command-query args-types function]
   {:pre [(symbol? method-name)
          (vector? args-types)
          (every? is-allowed-type? args-types)
          (or (= :command command-query)
              (= :query command-query))
          (fn? function)]}
   (register-method method-name command-query :main args-types function))
  ([method-name command-query specificator args-types function]
   {:pre [(symbol? method-name)
          (vector? args-types)
          (every? is-allowed-type? args-types)
          (or (= :main specificator)
              (= :before specificator)
              (= :after specificator))
          (or (= :command command-query)
              (= :query command-query))
          (fn? function)]}
   (if (is-method-name-exist? method-name)
     (loop [args-types-left args-types
            branch-ref (get-method-name-branch-ref method-name)]
       (cond
         (empty? args-types-left) (dosync
                                   (alter branch-ref assoc specificator {:command-query command-query
                                                                         :function function}))
         (some? ((deref branch-ref) (first args-types-left))) (recur (rest args-types-left)
                                                                     ((deref branch-ref) (first args-types-left)))

         :else (do
                 (dosync
                  (alter branch-ref assoc (first args-types-left) (ref {})))
                 (recur args-types-left branch-ref))))
     (do
       (dosync
        (alter obj-model-methods assoc method-name (ref {})))
       (register-method method-name command-query specificator args-types function)))))

(defn map-type
  "maps value to type"
  [value]
  (cond
    (is-object? value) (get-object-class value)
    (boolean? value) 'bool-type
    (number? value) 'number-type
    (string? value) 'string-type
    (keyword? value) 'keyword-type
    (symbol? value) 'symbol-type
    (coll? value) 'value
    :else 'generic-type))

(defn made-private-acceess
  "add to object priveledge"
  [object]
  {:pre [(is-object? object)]}
  (assoc object :private true))

(defn copy-object
  "copy object"
  [object]
  {:pre [(is-object? object)]}
  (reduce #(assoc %1 %2 (cond
                          (= %2 :slots) (reduce (fn [acc val]
                                                  (assoc acc val (ref (deref ((object %2) val)))))
                                                {}
                                                (keys (object %2)))
                          (= %2 :super) (reduce (fn [acc val]
                                                  (assoc acc val (copy-object ((object %2) val))))
                                                {}
                                                (keys (object %2)))
                          :else (object %2))) {} (keys object)))

(defn create-function
  "create 0 arg function, that will execute as function, that called with args"
  [function args specificator]
  (cond
    (empty? args) function
    :else (recur (partial function (if (and (is-object? (first args))
                                            (= :query specificator))
                                     (copy-object (first args))
                                     (first args))) (rest args) specificator)))

(defn get-most-specific-functions
  "returns function with given specificator in order:
   firsts args are more specific
   in arg in bfs order
   if in arg no specified method, generic arg is searched"
  [private-access-args types branch-ref specificator]
  (cond
    (empty? types) (if (some? ((deref branch-ref) specificator)) (let [function-meta ((deref branch-ref) specificator)
                                                                       function (function-meta :function)
                                                                       command-query (function-meta :command-query)]
                                                                   [(create-function function private-access-args command-query)])
                       [])

    :else (let [is-obj (class-registed? (first types))
                order (if is-obj
                        (get-class-order (first types))
                        (list (first types)))
                spec (reduce #(if (some? ((deref branch-ref) %2))
                                (concat %1 (get-most-specific-functions private-access-args (rest types) ((deref branch-ref) %2) specificator))
                                %1) (list) order)
                is_empty_spec (empty? spec)
                result (if is_empty_spec
                         (reduce #(if (some? ((deref branch-ref) %2))
                                    (concat %1 (get-most-specific-functions private-access-args (rest types) ((deref branch-ref) %2) specificator))
                                    %1) (list) (list 'generic-type))
                         spec)]
            result)))

(defn dispatch-method
  "calls :before methods in most specific order,
   :main the most specific
   :after in least specific order"
  [method-name arg-vector]
  {:pre [(is-method-name-exist? method-name)
         (vector? arg-vector)]}
  (let [types (map map-type arg-vector)
        private-access-args (map #(if (is-object? %1)
                                    (made-private-acceess %1)
                                    %1) arg-vector)
        before-functions (get-most-specific-functions private-access-args types (get-method-name-branch-ref method-name) :before)
        main-function (first (get-most-specific-functions private-access-args types (get-method-name-branch-ref method-name) :main))
        after-functions (reverse (get-most-specific-functions private-access-args types (get-method-name-branch-ref method-name) :after))
        functions (concat before-functions [main-function] after-functions)]
    (if (seq functions)
      (let [results-b (doall (for [f before-functions]
                        (f)))
            result-m (main-function)
            results-a (doall (for [f after-functions]
                        (f)))]
        result-m)
      (assert false "no fittable functions"))))

(defn call-next-method-result
  "returns method result from call-next-method-result result"
  [context]
  (context :result))

(defn call-next-method
  "calls next :main method in most specific order"
  ([method-name arg-vector]
   {:pre [(is-method-name-exist? method-name)
          (vector? arg-vector)]}
   (let [types (map map-type arg-vector)
         rest-main-functions (rest (get-most-specific-functions arg-vector types (get-method-name-branch-ref method-name) :main))]
     (call-next-method {:context rest-main-functions})))
  ([context]
   {:result ((first (context :context)))
    :context (rest (context :context))}))

;;;;;;;;;; example 1

(comment (defclass 'person (list) (list
                          (slot 'name :public)
                          (slot 'private-defolt-slot :private "private")
                          (slot 'height :private)
                          (slot 'weight :private)))

(defclass 'not-a-person (list) (list
                          (slot 'name :public)
                          (slot 'private-defolt-slot :private "private")
                          (slot 'height :private)
                          (slot 'weight :private)))

obj-model-classes

(def boba (make-instance 'person {'name "boba"
                                  'height 180
                                  'weight 50}))

boba

(get-value boba 'name)
;(get-value boba 'private-defolt-slot)
;(get-value boba 'height)

(register-method 'print-name
                 :query
                 :before
                 ['person]
                 #(println (get-value %1 'height)))

(register-method 'print-name
                 :query
                 :main
                 ['person]
                 #(println (get-value %1 'name)))

(register-method 'print-name
                 :query
                 :after
                 ['person]
                 #(println (get-value %1 'weight)))

(register-method 'print-name
                 :query
                 :main
                 ['not-a-person]
                 #(println "not wanted method"))

obj-model-methods

(dispatch-method 'print-name [boba]))

;;;; example 2

(comment (defclass 'base (list) (list
                        (slot 'defining :public)
                        (slot 'base-exclude :public "base-exclude")
                        (slot 'base-override-1 :public "base-override-1")
                        (slot 'base-override-2 :public "base-override-2")
                        (slot 'base-override-3 :public "base-override-3")))

(defclass 'child-1 (list 'base) (list
                                 (slot 'defining :public)
                                 (slot 'base-override-1 :public "child-override-1")
                                 (slot 'child-inclusion :public "child-inclusion-1-child")))

(defclass 'child-2 (list 'base) (list
                                 (slot 'defining :public)
                                 (slot 'base-override-2 :public "child-override-2")
                                 (slot 'child-inclusion :public "child-inclusion-2-child")
                                 (slot 'child-2-exclude :public "child-2-exclude")))

(defclass 'child-child (list 'child-1 'child-2) (list
                                                 (slot 'defining :public) 
                                                 (slot 'base-override-1 :public "child-child-override-1")
                                                 (slot 'base-override-3 :public "child-child-override-3")
                                                 (slot 'child-child :public "child-child")))

(def cc (make-instance 'child-child 
                       {'defining "defining"}
                       {'base {'defining "base defining"}
                        'child-2 {'defining "child-2 defining"}}))

(get-value cc 'defining)
(get-value cc 'defining 'base)
(get-value cc 'defining 'child-1)
(get-value cc 'defining 'child-2)



(set-value cc 'defining "defining child child")
(get-value cc 'defining)
(get-value cc 'defining 'base)
(get-value cc 'defining 'child-1)
(get-value cc 'defining 'child-2)

(set-value cc 'defining "child-1 defining" 'child-1)
(get-value cc 'defining)
(get-value cc 'defining 'base)
(get-value cc 'defining 'child-1)
(get-value cc 'defining 'child-2)


(get-value cc 'child-child)

(get-value cc 'base-exclude)

(get-value cc 'child-2-exclude)

(get-value cc 'child-inclusion)
(get-value cc 'child-inclusion 'child-2)

(get-value cc 'base-override-1)
(get-value cc 'base-override-2)
(get-value cc 'base-override-2 'base)
(get-value cc 'base-override-3)
(get-value cc 'base-override-3 'base))

;; example 3

(comment (defclass 'a (list)
  (list
   (slot 'a :private "a")))

(defclass 'b (list 'a)
  (list 
   (slot 'b :private "b")))

(defclass 'd (list)
  (list
   (slot 'd :private "d")))

(defclass 'c (list 'b 'd)
  (list
   (slot 'b :private "c's b")
   (slot 'c :private "c")))

(register-method 'method :query :before ['d]
                 #(println (get-value %1 'd)
                           "before d"))

(register-method 'method :query :main ['d]
                 #(println (get-value %1 'd)))

(register-method 'method :query :after ['d]
                 #(println (get-value %1 'd)
                           "after d"))

(register-method 'method :query :before ['generic-type]
                 #(println "before" %1))

(register-method 'method :query :main ['generic-type]
                 #(println %1))

(register-method 'method :query :before ['a]
                 #(println (get-value %1 'a)
                           "before a"))

(register-method 'method :query :main ['a] 
                 #(println (get-value %1 'a)))

(register-method 'method :query :after ['a]
                 #(println (get-value %1 'a)
                           "after a"))

(register-method 'method :query :before ['b]
                 #(println (get-value %1 'b)
                           (get-value %1 'a)
                           "before b"))

(register-method 'method :query :main ['b]
                 #(println (get-value %1 'b) 
                           (get-value %1 'a)))


(register-method 'method :query :after ['b]
                 #(println (get-value %1 'b)
                           (get-value %1 'a)
                           "after b"))

(register-method 'method :query :before ['c]
                 #(println (get-value %1 'c)
                           (get-value %1 'b)
                           (get-value %1 'b 'b)
                           (get-value %1 'a)
                           "before c"))

(register-method 'method :query :main ['c]
                 #(println (get-value %1 'c) 
                           (get-value %1 'b) 
                           (get-value %1 'b 'b)
                           (get-value %1 'a)))

(def a (make-instance 'a {}))
(def b (make-instance 'b {}))
(def c (make-instance 'c {}))
(def d (make-instance 'd {}))

(dispatch-method 'method [a])
(dispatch-method 'method [b])
(dispatch-method 'method [c])
(dispatch-method 'method [d])

(register-method 'method :query :main ['c]
                 #(let [p (println "will call next methods")
                        ctx (call-next-method 'method [%1])
                        ctx (call-next-method ctx)
                        ctx (call-next-method ctx)]))

(dispatch-method 'method [c])

(dispatch-method 'method ["hello world"])

(defclass 'kuk (list) (list))

(def kuk (make-instance 'kuk {}))

(dispatch-method 'method [kuk]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; example 3

(comment (defclass 'base1 (list) (list))
(defclass 'child1 (list 'base1) (list))

(defclass 'base2 (list) (list))
(defclass 'child2 (list 'base2) (list))

(register-method 'foo :query :main ['child1 'base2] 
                 (fn [o1 o2] (println "child1 base2")))

(register-method 'foo :query :main ['base1 'child2]
                 (fn [o1 o2] (println "base1 child2")))

(def obj1 (make-instance 'child1 {}))
(def obj2 (make-instance 'child2 {}))
(dispatch-method 'foo [obj1 obj2])

(register-method 'foo :query :main ['base1 'number-type]
                 (fn [o1 o2] (println "base1 and number" o2)))
(dispatch-method 'foo [obj1 1])

(register-method 'foo :query :main ['base1 'generic-type]
                 (fn [o1 o2] (println "base1 and" o2)))
(dispatch-method 'foo [obj1 [1 2 3]])

(register-method 'foo :query :main ['child1 'child2]
                 (fn [o1 o2]
                   (let [p (println "will print order")
                         ctx (call-next-method 'foo [o1 o2])
                         ctx (call-next-method ctx)])))

(dispatch-method 'foo [obj1 obj2]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; example 4

(comment (defclass 'class (list) 
  (list
   (slot 'a :public "default")))

(def class-object (make-instance 'class {}))

(register-method 'foo :query :main ['class]
                 #(do 
                    (set-value %1 'a "changed value")
                    (get-value %1 'a)))

(register-method 'boo :command :main ['class]
                 #(do
                    (set-value %1 'a "changed value")
                    (get-value %1 'a)))

(println (dispatch-method 'foo [class-object]))
(println (get-value class-object 'a))

(println (dispatch-method 'boo [class-object]))
(println (get-value class-object 'a)))