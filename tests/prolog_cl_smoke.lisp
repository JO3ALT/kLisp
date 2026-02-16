(load "prolog-cl.lisp")
(in-package #:cl-user)

(defun assert-true (cond msg)
  (unless cond
    (error "ASSERT FAILED: ~A" msg)))

(reset-kb!)
(fact! '(p a))
(fact! '(p b))
(fact! '(p c))
(rule! '(q (var X)) '((p (var X)) !))

(let* ((sols (query-solutions '((q (var X)))))
       (s1 (car sols)))
  (assert-true (= (length sols) 1) "cut should keep only first q/1 answer")
  (assert-true (equal (walk s1 '(var X)) 'a) "q(X) should bind X=a after cut"))

(reset-kb!)
(fact! '(p a))
(fact! '(p b))
(let* ((sols (query-solutions '((once (p (var X))))))
       (s1 (car sols)))
  (assert-true (= (length sols) 1) "once/1 should keep only first solution")
  (assert-true (equal (walk s1 '(var X)) 'a) "once(p(X)) should bind X=a"))

(reset-kb!)
(fact! '(p a))
(fact! '(r b))
(rule! '(m (var X)) '((or (p (var X)) (r (var X)))))
(let ((sols (query-solutions '((m (var X))))))
  (assert-true (= (length sols) 2) "or/2 should return both branches"))

(reset-kb!)
(rule! '(ok) '(true))
(rule! '(ng) '(fail))
(assert-true (= (length (query-solutions '((ok)))) 1) "true/0 should succeed")
(assert-true (= (length (query-solutions '((ng)))) 0) "fail/0 should fail")

(reset-kb!)
(fact! '(p a))
(fact! '(p b))
(fact! '(r c))
;; once + or: only first success should remain.
(let ((sols (query-solutions '((once (or (p (var X)) (r (var X))))))))
  (assert-true (= (length sols) 1) "once(or(...)) should keep one solution"))

(reset-kb!)
(fact! '(p a))
(fact! '(p b))
(fact! '(r c))
;; cut after disjunction should prune later alternatives.
(rule! '(k (var X)) '((or (p (var X)) (r (var X))) !))
(let ((sols (query-solutions '((k (var X))))))
  (assert-true (= (length sols) 1) "cut after or should prune remaining choices"))

(format t "PROLOG-CL-SMOKE: OK~%")
