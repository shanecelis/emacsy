;; https://github.com/davexunit/guile-2d/blob/master/2d/coroutine.scm
;;
;; I'm Shane. I'm having a problem trying to fix codefine*.  I wrote
;; the strip-optargs macro to try to fix it, but I can't get it right.
;; Help me, #guile!

;;; guile-2d
;;; Copyright (C) 2013 David Thompson <dthompson2@worcester.edu>
;;;
;;; Guile-2d is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; Guile-2d is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Cooperative multi-tasking.
;;
;;; Code:

(define-module (emacsy coroutine)
  #:export (coroutine
            colambda
            codefine
            codefine*
            coroutine->cid
            )
  #:replace (yield))

(define cid-next 0)

(define (coroutine->cid resume)
  "Given a coroutine resumption method. This will return that
coroutine's ID, or cid."
  (procedure-property resume 'cid))

;; could have a (make-coroutine thunk) => (cid . run-coroutine-thunk)
(define (make-coroutine thunk)
  "Creates a procedure that can yield a continuation.  (Does not execute thunk.)"
  (define cid cid-next)
  (define (handler cont callback . args)
    (define (resume . args)
      (format #t "resuming cid ~a~%" cid)
      ;; Call continuation that resumes the procedure.
      (call-with-prompt 'coroutine-prompt
                        (lambda () (apply cont args))
                        handler))
    (set-procedure-property! resume 'cid cid)
    (when (procedure? callback)
      (apply callback resume args)))
  (set! cid-next (1+ cid-next))

  ;; Call procedure.
  (let ((first-call (lambda () (call-with-prompt 'coroutine-prompt thunk handler))))
    (set-procedure-property! first-call 'cid cid)
    (values first-call cid)))

(define (coroutine thunk)
  "Calls a procedure that can yield a continuation."
  ((make-coroutine thunk)))

;; emacs: (put 'colambda 'scheme-indent-function 0)
(define-syntax-rule (colambda args body ...)
  "Syntacic sugar for a lambda that is run as a coroutine."
  (lambda args
    (coroutine
     (lambda () body ...))))

;; emacs: (put 'codefine 'scheme-indent-function 1)
(define-syntax-rule (codefine (name ...) . body)
  "Syntactic sugar for defining a procedure that is run as a
coroutine."
  (define (name ...)
    ;; Create an inner procedure with the same signature so that a
    ;; recursive procedure call does not create a new prompt.
    (define (name ...) . body)
    (coroutine
     (lambda () (name ...)))))

;; (strip-optargs #'(a #:optional (b 2) c)) =>~ (a b c)
;; TODO make it work with keyword arguments.
(define strip-optargs
  (lambda (x)
    (syntax-case x ()
      (()
       #'())
      ;; optional values
      (((v e) e2 ...)
       (identifier? #'v)
       #`(v . #,(strip-optargs #'(e2 ...))))
      ;; #:optional keyword
      ((#:optional e2 ...)
       (strip-optargs #'(e2 ...)))
      ;; identifiers
      ((e1 e2 ...)
       (identifier? #'e1)
       #`(e1 . #,(strip-optargs #'(e2 ...)))))))

;; emacs: (put 'codefine* 'scheme-indent-function 1)
  "Syntactic sugar for defining a procedure with optional and
keyword arguments that is run as a coroutine."
#;(define-syntax codefine*
  (lambda (x)
    (syntax-case x ()
      ((codefine* (name . args) . body) 
       (with-syntax ((callable-args (strip-optargs #'args))) 
         #'(define* (name . args)
             ;; Create an inner procedure with the same signature so that a
             ;; recursive procedure call does not create a new prompt.
             (define* (name . args) . body)
             (coroutine
              (lambda () (name . callable-args)))))))))

;; emacs: (put 'codefine* 'scheme-indent-function 1)
(define-syntax-rule (codefine* (name . formals) . body)
  "Syntactic sugar for defining a procedure that is run as a
coroutine."
  (define (name . args)
    ;; Create an inner procedure with the same signature so that a
    ;; recursive procedure call does not create a new prompt.
    (define* (name . formals) . body)
    (coroutine
     (lambda () (apply name args)))))

(define (yield callback)
  "Yield continuation to a CALLBACK procedure."
  (abort-to-prompt 'coroutine-prompt callback))
