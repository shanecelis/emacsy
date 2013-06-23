(define-module (float-equality)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1) ;; fold
  #:export (=?))

(define-method (=? (a <number>) (b <number>) . rest)
  (let-optional rest ((tolerance 0.001))
   (< (abs (- a b)) tolerance)))

(define-method (=? (a <list>) (b <list>) . rest)
  (fold (lambda (x y prev)
          (and prev (apply =? x y rest))) #t a b))

(define-method (=? (a <pair>) (b <pair>) . rest)
  (and (apply =? (car a) (car b) rest)
       (apply =? (cdr a) (cdr b) rest)))


(define-method (=? (a <vector>) (b <vector>) . rest)
  (apply =? (vector->list a) (vector->list b) rest))

(define-method (=? (a <uvec>) (b <uvec>) . rest)
  (apply =? (generalized-vector->list a) (generalized-vector->list b) rest))

(define-method (=? a b . rest)
  #f)

(define-method (=? (tolerance <number>))
  (lambda (a b)
    (=? a b tolerance)))

