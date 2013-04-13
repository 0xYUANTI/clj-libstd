;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tagged unions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;_* Declarations =====================================================
(ns clj-libstd.union
  (:use clojure.test))

(declare gen-ctor gen-clause gen-bindings)
(declare predict-gensyms current-gensym-counter)

;;;_* Code =============================================================
;;;_ * defunion --------------------------------------------------------
(defmacro defunion
  "Define a tagged union."
  [name & ctors]
  `(do (defrecord ~name [~'tag ~'args])
       ~@(map #(gen-ctor % name) ctors)))

(defn- gen-ctor [ctor name]
  (let [name (symbol (str name "."))]
    (if (seq? ctor)
      `(defn ~(first ctor) ~(vec (rest ctor))
         (~name ~(keyword (first ctor)) ~(cons 'list (rest ctor))))
      `(def ~ctor
         (~name ~(keyword ctor) nil)))))

;;;_ * match -----------------------------------------------------------
(defmacro match
  "Case analysis."
  [expr & clauses]
  (let [val  (gensym)
        tag  (gensym)
        args (gensym)]
    `(let [~val  ~expr
           ~tag  (:tag  ~val)
           ~args (:args ~val)]
     (case ~tag
       ~@(mapcat #(gen-clause % args) (partition 2 clauses))
       :else (throw (Exception. "badmatch"))))))

(defn gen-clause [clause args]
  (if (seq? (first clause))
    `(~(keyword (first (first clause)))
      (let ~(gen-bindings (rest (first clause)) args)
        ~@(rest clause)))
    `(~(keyword (first clause)) ~@(rest clause))))

(defn gen-bindings [vars args]
  (vec (mapcat (fn [[var idx]] [var (list 'nth args idx)])
               (map #(vector %1 %2) vars (range (count vars))))))

;;;_* Tests ============================================================
;; Test helpers
(defn- predict-gensyms [n]
  (let [c1 (current-gensym-counter)
        c2 (current-gensym-counter)
        d  (- c2 c1)]
    (for [i (range 1 (inc n))]
      (symbol (str "G__" (+ c2 (* i d)))))))

(defn- current-gensym-counter []
  (let [string (str (gensym))]
    (Integer/parseInt (subs string (inc (.lastIndexOf string "_"))))))

;; Test union
(defunion Tree
  T
  (V x xs)
  (W x xs))

;; Test cases
(deftest defunion-syntax-test
  (is (= (macroexpand-1
          '(clj-libstd.union/defunion Tree
             T
             (V x xs)
             (W x xs)))
         '(do (clojure.core/defrecord Tree [tag args])
              (def T (Tree. :T nil))
              (clojure.core/defn V [x xs] (Tree. :V (list x xs)))
              (clojure.core/defn W [x xs] (Tree. :W (list x xs)))))))

(deftest defunion-semantics-test
  (is (= T            (Tree. :T nil)))
  (is (= (V 1 '(2 3)) (Tree. :V '(1 (2 3)))))
  (is (= (W 1 '(2 3)) (Tree. :W '(1 (2 3))))))

(deftest match-syntax-test
  (let [[s1 s2 s3] (predict-gensyms 3)]
    (is (= (macroexpand-1
            '(clj-libstd.union/match foo
               T nil
               (V x xs) (cons x xs)))
           (list 'clojure.core/let [s1 'foo
                                    s2 (list :tag  s1)
                                    s3 (list :args s1)]
              (list 'clojure.core/case s2
                :T    nil
                :V    (list 'clojure.core/let ['x  (list 'nth s3 0)
                                               'xs (list 'nth s3 1)]
                        (list 'cons 'x 'xs))
                :else (list 'throw (list 'java.lang.Exception. "badmatch"))))))))

(deftest match-semantics-test
  (is (= (match T
           T nil
           (V x xs) (cons x xs))
         nil))
  (is (= (match (V 1 '(2 3))
           T nil
           (V x xs) (cons x xs))
         '(1 2 3)))
  (is (thrown? Exception
               (match (W 1 '(2 3))
                 T nil
                 (V x xs) (cons x xs))))
  (is (thrown? Exception
               (match 42
                 T nil
                 (V x xs) (cons x xs)))))

;;;_* Emacs ============================================================
;;; Local Variables:
;;; allout-layout: t
;;; End:
