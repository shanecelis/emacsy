#line 617 "(null)"
;;; <check/harness.scm>=
(define-module (check harness)
  #:use-module (check)
  #:export (run-tests
            run-tests-and-exit)
  #:export-syntax (define-test))
;;; Set up the variables.
;;; 

#line 627 "(null)"
;;; <check/harness.scm>=
(define unit-tests '())
(define test-errors '())
;;; We can register any procedure to a test name.  
;;; 

#line 634 "(null)"
;;; <check/harness.scm>=
(define (register-test name func)
  "Register a procedure to a test name."
  (set! unit-tests (acons name func unit-tests)))
;;; Typically, users will define and register their tests with this macro.
;;; 

#line 642 "(null)"
;;; <check/harness.scm>=
(define-syntax define-test
  (syntax-rules ()
    ((define-test (name args ...) expr ...)
     (begin (define* (name args ...)
        expr ...)
     (register-test 'name name)))))
#line 650 "(null)"
;;; We need to run the tests.
;;; 
;;; <check/harness.scm>=
(define (run-tests)
  (catch 'first-error
    (lambda () 
      
#line 660 "(null)"
;;; <handle each test>=
(for-each 
 (lambda (elt)
   (format #t "TEST: ~a\n" (car elt))
   ;;(pretty-print elt)
   (catch #t
     (lambda ()
       (with-throw-handler 
        #t
        (lambda ()
          (apply (cdr elt) '()))
        (lambda args
          (set! test-errors (cons (car elt) test-errors))
          (format #t "Error in test ~a: ~a" (car elt) args)
          (backtrace))))
     (lambda args
       (throw 'first-error)
       #f)))
 (reverse unit-tests))
#line 656 "(null)"
                          )
    (lambda args
      #f)))
#line 680 "(null)"
;;; <check/harness.scm>=
(define (run-tests-and-exit)
  (run-tests)
  (check-report)
  (if (> (length test-errors) 0)
      (format #t "~a ERROR in tests: ~a." (length test-errors) (reverse test-errors))
      (format #t "NO ERRORs in tests."))
  (exit (if (and (= (length test-errors) 0) (= 0 (length check:failed))) 0 1)))
