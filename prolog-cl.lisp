;;;; prolog-cl.lisp  -- CPS Prolog meta-interpreter in Common Lisp (ECL compatible)
;;;; Features:
;;;;  - Horn clauses (facts/rules)
;;;;  - Unification (no occurs-check)
;;;;  - standardize-apart (variable renaming per rule use)
;;;;  - Full cut (!) via CPS (discard failure continuations up to predicate-call boundary)
;;;;  - Streaming query output: one solution per line (subst S-exp); T for success with no bindings; NIL if no solutions

(in-package #:cl-user)

;;;; ============================================================
;;;; KB
;;;; ============================================================

(defparameter *facts* nil)   ; list of fact heads (terms)
(defparameter *rules* nil)   ; list of rules: (cons head body-list)
(defparameter *gensym* 0)
(defparameter +fail+ :fail)

(defun reset-kb! ()
  (setf *facts* nil
        *rules* nil
        *gensym* 0)
  'ok)

(defun fact! (head)
  ;; Preserve source order (Prolog tries clauses in definition order).
  (setf *facts* (append *facts* (list head)))
  'ok)

(defun rule! (head body)
  ;; body is a list of goals (terms)
  ;; Preserve source order (Prolog tries clauses in definition order).
  (setf *rules* (append *rules* (list (cons head body))))
  'ok)

;;;; ============================================================
;;;; Term helpers
;;;; ============================================================

(defun var? (x)
  (and (consp x) (eq (car x) 'var)))

(defun var-base (v)
  ;; (var X ...) => X
  (cadr v))

(defun fresh-var (base)
  (incf *gensym*)
  (list 'var base *gensym*))

;;;; ============================================================
;;;; Substitution (alist: ((var . term) ...) represented as ((var term) ...))
;;;; We use (assoc ... :test #'equal) because vars are lists.
;;;; ============================================================

(defun lookup (subst v)
  (assoc v subst :test #'equal))

(defun extend (subst v term)
  (cons (list v term) subst))

(defun walk (subst term)
  (if (var? term)
      (let ((p (lookup subst term)))
        (if p (walk subst (cadr p)) term))
      term))

(defun apply-subst (subst term)
  (let ((t2 (walk subst term)))
    (cond
      ((var? t2) t2)
      ((atom t2) t2)
      (t (mapcar (lambda (x) (apply-subst subst x)) t2)))))

;;;; ============================================================
;;;; Unification (no occurs-check)
;;;;  - Equality check uses EQUAL for structural equality.
;;;;  - For symbols/numbers, EQUAL is fine (numbers compare as EQL).
;;;; ============================================================

(defun unify (x y subst)
  (let ((x1 (walk subst x))
        (y1 (walk subst y)))
    (cond
      ((equal x1 y1) subst)
      ((var? x1) (extend subst x1 y1))
      ((var? y1) (extend subst y1 x1))
      ((and (consp x1) (consp y1)) (unify-list x1 y1 subst))
      (t +fail+))))

(defun unify-list (xs ys subst)
  (cond
    ((null xs) (if (null ys) subst +fail+))
    ((null ys) +fail+)
    (t (let ((s1 (unify (car xs) (car ys) subst)))
         (if (eq s1 +fail+)
             +fail+
             (unify-list (cdr xs) (cdr ys) s1))))))

;;;; ============================================================
;;;; standardize-apart (rename variables per rule use)
;;;; ren: alist mapping old-var -> new-var  (as ((old new) ...))
;;;; ============================================================

(defun rename-term (term ren)
  (cond
    ((var? term)
     (let ((p (assoc term ren :test #'equal)))
       (if p
           (list (cadr p) ren)
           (let ((nv (fresh-var (var-base term))))
             (list nv (extend ren term nv))))))
    ((atom term) (list term ren))
    (t (rename-list term ren))))

(defun rename-list (xs ren)
  (if (null xs)
      (list nil ren)
      (let* ((r1 (rename-term (car xs) ren))
             (x2 (car r1))
             (ren2 (cadr r1))
             (r2 (rename-list (cdr xs) ren2))
             (xs2 (car r2))
             (ren3 (cadr r2)))
        (list (cons x2 xs2) ren3))))

(defun standardize-apart (rule)
  ;; rule = (cons head bodylist)
  (let ((head (car rule))
        (body (cdr rule)))
    (let* ((r1 (rename-term head nil))
           (h2 (car r1))
           (ren (cadr r1))
           (r2 (rename-list body ren))
           (b2 (car r2)))
      (cons h2 b2))))

;;;; ============================================================
;;;; CPS Solver with FULL cut (!)
;;;;
;;;; solve(goals, subst, cutk, sc, fc)
;;;;  goals : list of goals (terms)
;;;;  subst : substitution
;;;;  cutk  : "cut boundary" failure continuation
;;;;  sc    : success continuation  (lambda (subst fc-next) ...)
;;;;  fc    : failure continuation  (lambda () ...)
;;;;
;;;; Cut goal '(!):
;;;;   solve(rest, subst, cutk, sc, cutk)
;;;;
;;;; Predicate-call boundary:
;;;;  - When solving a goal G, the cut boundary for G's internal body is the
;;;;    failure continuation at "goal entry" (fc-goal).
;;;;  - After a clause body succeeds, we continue the parent's rest-goals with
;;;;    the parent's cutk (scope separation).
;;;; ============================================================

(defun query-solutions (goals)
  "Returns all solutions as a list of substitutions."
  (let ((acc nil))
    (labels
        ((sc (s fc-next)
           (push s acc)
           (funcall fc-next))
         (fc0 ()
           'done))
      (solve goals nil #'fc0 #'sc #'fc0)
      (nreverse acc))))

(defun query! (goals)
  "Streaming query: prints one line per solution.
Protocol:
  - each solution with bindings: prints SUBST as S-exp (~S)
  - success with no bindings: prints T
  - if no solutions at all: prints NIL (one line)
Returns OK."
  (let ((sols (query-solutions goals)))
    (if (null sols)
        (format t "~S~%" nil)
        (dolist (s sols)
          (if (null s)
              (format t "~S~%" t)
              (format t "~S~%" s))))
    'ok))

(defun solve (goals subst cutk sc fc)
  (if (null goals)
      (funcall sc subst fc)
      (let ((g (car goals))
            (rest (cdr goals)))
        (cond
          ((eq g '!)
           (solve rest subst cutk sc cutk))
          ((eq g 'true)
           (solve rest subst cutk sc fc))
          ((eq g 'fail)
           (funcall fc))
          ((and (consp g) (eq (car g) 'once) (consp (cdr g)) (null (cddr g)))
           (solve-once (cadr g) rest subst cutk sc fc))
          ((and (consp g) (eq (car g) 'or) (consp (cdr g)) (consp (cddr g)) (null (cdddr g)))
           (solve-or (cadr g) (caddr g) rest subst cutk sc fc))
          (t
           (solve-goal g rest subst cutk sc fc))))))

(defun solve-once (goal rest subst cutk sc fc)
  ;; once(G) = execute G and keep only the first success.
  (let ((used nil))
    (labels
        ((sc-once (s fc-from-goal)
           (declare (ignore fc-from-goal))
           (setf used t)
           (solve rest s cutk sc fc))
         (fc-once ()
           (if used
               (funcall fc)
               (funcall fc))))
      (solve (list goal) subst fc #'sc-once #'fc-once))))

(defun solve-or (left right rest subst cutk sc fc)
  ;; (or A B): try A branch first, then B on failure.
  (solve (cons left rest)
         subst
         cutk
         sc
         (lambda ()
           (solve (cons right rest) subst cutk sc fc))))

(defun solve-goal (g rest subst parent-cutk sc fc-goal)
  ;; facts first, then rules
  (let ((fc-after-facts
          (lambda ()
            (solve-rules g rest subst parent-cutk sc fc-goal))))
    (solve-facts g rest subst parent-cutk sc fc-after-facts)))

(defun solve-facts (g rest subst parent-cutk sc fc-goal)
  (solve-clauses-fact *facts* g rest subst parent-cutk sc fc-goal))

(defun solve-rules (g rest subst parent-cutk sc fc-goal)
  (solve-clauses-rule *rules* g rest subst parent-cutk sc fc-goal))

(defun solve-clauses-fact (clauses g rest subst parent-cutk sc fc-goal)
  (if (null clauses)
      (funcall fc-goal)
      (let* ((cl (car clauses))
             (more (cdr clauses))
             (fc-next-clause
               (lambda ()
                 (solve-clauses-fact more g rest subst parent-cutk sc fc-goal)))
             (s2 (unify g cl subst)))
        (if (eq s2 +fail+)
            (funcall fc-next-clause)
            ;; success: continue with parent's rest; failure tries next clause
            (solve rest s2 parent-cutk sc fc-next-clause)))))

(defun solve-clauses-rule (clauses g rest subst parent-cutk sc fc-goal)
  (if (null clauses)
      (funcall fc-goal)
      (let* ((cl0 (car clauses))
             (more (cdr clauses))
             (fc-next-clause
               (lambda ()
                 (solve-clauses-rule more g rest subst parent-cutk sc fc-goal)))
             (cl (standardize-apart cl0))
             (head (car cl))
             (body (cdr cl))
             (s2 (unify g head subst)))
        (if (eq s2 +fail+)
            (funcall fc-next-clause)
            ;; Solve body in callee predicate scope:
            ;;   cut boundary = fc-goal (goal-entry failure continuation)
            ;;   failure continuation for body backtracking into other clauses = fc-next-clause
            (let ((sc-body
                    (lambda (s-body fc-from-body)
                      ;; after body success, continue parent's rest with parent's cutk
                      (solve rest s-body parent-cutk sc fc-from-body))))
              (solve body s2 fc-goal sc-body fc-next-clause))))))

;;;; ============================================================
;;;; Optional convenience: compile-time-ish helpers for interactive use
;;;; ============================================================

(defun listing ()
  (format t "~&FACTS: ~S~%" *facts*)
  (format t "RULES: ~S~%" *rules*)
  'ok)
