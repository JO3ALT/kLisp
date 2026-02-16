(load "prolog-cl.lisp")
(in-package #:cl-user)

(defun v (name)
  (list 'var name))

(defun project-solution (subst vars)
  (mapcar (lambda (sym)
            (list sym (apply-subst subst (v sym))))
          vars))

(defun project-solutions (sols vars)
  (mapcar (lambda (s) (project-solution s vars)) sols))

(defun emit (name value)
  (format t "~A => ~S~%" name value))

(defun run-samples ()
  ;; 1. single fact, ground query
  (reset-kb!)
  (fact! '(parent tom bob))
  (emit "s1-ground-fact" (length (query-solutions '((parent tom bob)))))

  ;; 2. one variable lookup
  (reset-kb!)
  (fact! '(parent tom bob))
  (fact! '(parent tom liz))
  (emit "s2-one-var"
        (project-solutions
         (query-solutions '((parent tom (var X))))
         '(X)))

  ;; 3. recursive ancestor
  (reset-kb!)
  (fact! '(parent tom bob))
  (fact! '(parent bob ann))
  (fact! '(parent ann ken))
  (rule! '(ancestor (var X) (var Y))
         '((parent (var X) (var Y))))
  (rule! '(ancestor (var X) (var Y))
         '((parent (var X) (var Z))
           (ancestor (var Z) (var Y))))
  (emit "s3-ancestor"
        (project-solutions
         (query-solutions '((ancestor tom (var Y))))
         '(Y)))

  ;; 4. explicit or/2 in body
  (reset-kb!)
  (fact! '(p a))
  (fact! '(q b))
  (rule! '(good (var X))
         '((or (p (var X)) (q (var X)))))
  (emit "s4-or"
        (project-solutions
         (query-solutions '((good (var X))))
         '(X)))

  ;; 5. cut
  (reset-kb!)
  (fact! '(p a))
  (fact! '(p b))
  (fact! '(q a))
  (fact! '(q b))
  (rule! '(choose (var X))
         '((p (var X)) ! (q (var X))))
  (emit "s5-cut"
        (project-solutions
         (query-solutions '((choose (var X))))
         '(X)))

  ;; 6. once/1
  (reset-kb!)
  (fact! '(p a))
  (fact! '(p b))
  (emit "s6-once"
        (project-solutions
         (query-solutions '((once (p (var X)))))
         '(X)))

  ;; 7. member over cons/nil list
  (reset-kb!)
  (rule! '(member (var X) (cons (var X) (var _)))
         '(true))
  (rule! '(member (var X) (cons (var _) (var T)))
         '((member (var X) (var T))))
  (emit "s7-member"
        (project-solutions
         (query-solutions
          '((member (var X)
                    (cons a (cons b (cons c nil))))))
         '(X)))

  ;; 8. append relation
  (reset-kb!)
  (rule! '(append nil (var L) (var L))
         '(true))
  (rule! '(append (cons (var H) (var T)) (var L) (cons (var H) (var R)))
         '((append (var T) (var L) (var R))))
  (emit "s8-append"
        (project-solutions
         (query-solutions
          '((append (cons a (cons b nil))
                    (cons c nil)
                    (var Z))))
         '(Z)))

  ;; 9. true/fail behavior
  (reset-kb!)
  (rule! '(ok) '(true))
  (rule! '(ng) '(fail))
  (emit "s9-true-fail"
        (list (length (query-solutions '((ok))))
              (length (query-solutions '((ng))))))

  ;; 10. alias/unification constraint
  (reset-kb!)
  (fact! '(eqpair a a))
  (fact! '(eqpair a b))
  (emit "s10-alias"
        (project-solutions
         (query-solutions '((eqpair (var X) (var X))))
         '(X))))

(run-samples)
