;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous utility functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;_* Declarations =====================================================
(ns clj-libstd.core
  (:use clojure.test))

;;;_* Code =============================================================
(defn member?
  "Returns true iff X is an element of XS."
  [x xs]
  (some #(= x %) xs))

(deftest member?-test
  (is (= true (member? 2 '(1 2 3))))
  (is (= nil  (member? 4 '(1 2 3)))))

;;;_* Emacs ============================================================
;;; Local Variables:
;;; allout-layout: t
;;; End:
