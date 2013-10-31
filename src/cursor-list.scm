#| 
cursor-list.scm

This module creates a list with a cursor, that is, a position
within the list.  It's represented by two lists.  The "left" list is
held in reverse order which has the preceding contents.  The "right"
list is held in the conventional order.

|#

(define-module (cursor-list)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (make-cursor-list
            cursor-right?
            cursor-right!
            cursor-left?
            cursor-left!
            cursor-right-insert!
            cursor-left-insert!
            cursor-right-delete!
            cursor-left-delete!
            cursor-right-ref
            cursor-right-set!
            cursor-left-ref
            cursor-left-set!
            cursor-list?
            cursor-list->list))

(define-record-type <cursor-list>
  (%make-cursor-list left right)
  cursor-list?
  (left left set-left!)
  (right right set-right!))

(define* (make-cursor-list list #:optional (index 0))
  (%make-cursor-list (reverse (take list index)) (drop list index)))

(define (cursor-right-ref clist)
  (car (right clist)))

(define (cursor-left-ref clist)
  (car (left clist)))

(define (cursor-right-set! clist item)
  (set-car! (right clist) item)
  *unspecified*)

(define (cursor-left-set! clist item)
  (set-car! (left clist) item)
  *unspecified*)

(define* (cursor-right? clist #:optional (count 1))
  (>= (length (right clist)) count))

(define* (cursor-left? clist #:optional (count 1))
  (>= (length (left clist)) count))

(define (cursor-right! clist)
  (when (cursor-right? clist)
    (set-left!  clist (cons (cursor-right-ref clist) (left clist)))
    (set-right! clist (cdr (right clist))))
  *unspecified*)

(define (cursor-left! clist)
  (when (cursor-left? clist)
    (set-right! clist (cons (cursor-left-ref clist) (right clist)))
    (set-left!  clist (cdr (left clist))))
  *unspecified*)

(define (cursor-right-insert! clist item)
  (set-right! clist (cons item (right clist)))
  *unspecified*)

(define (cursor-left-insert! clist item)
  (set-left! clist (cons item (left clist)))
  *unspecified*)

(define (cursor-right-delete! clist)
  (set-right! clist (cdr (right clist)))
  *unspecified*)

(define (cursor-left-delete! clist)
  (set-left! clist (cdr (left clist)))
  *unspecified*)

(define (cursor-list->list clist)
  (append (reverse (left clist)) (right clist)))

(set-record-type-printer! <cursor-list>
  (lambda (clist port)
    (format port "#<cursor-list ~{~s ~}| ~{~s ~}>" 
            (reverse (left clist)) 
            (right clist))))
