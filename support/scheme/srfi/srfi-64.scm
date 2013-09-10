;; Copyright (c) 2012 Sunjoong Lee
;; Copyright (c) 2005, 2006, 2007 Per Bothner
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; 2007 Per Bothner original.
;; 2012 Sunjoong Lee modified to fit Guile 2.0.5.

;; Date: 2012-05-07

;; Porting Tips:
;; 1. Check required SRFIs;
;;    - most languages support SRFI 0, cond-expand, and SRFI 23, error, I think.
;;    - SRFI 9, define-record-type, is needed but not mandatory.
;;    - SRFI 39, make-parameter, is needed but not mandatory.
;;    - SRFI 34, with-exception-handler, may be need. Read #2 of this tips.
;;    - SRFI 35, Conditions, may be need. Read #6 of this tips.
;; 2. Check the exception handling method;
;;    - %test-eval macro of this file need to know exception handling method.
;;    - %test-eval macro returns a pair of evaluation value and false value
;;      unless a exception occurs.
;;    - %test-eval macro returns a pair of false value and exception cause
;;      if a exception occurs.
;;    - there are pre-defined %test-eval macros for some languages and SRFI 34.
;; 3. Check the export method if you make a module;
;;    - this file export procedures and macros using %export macro.
;;    - for example, Guile 2.0.5 need not to export any additional procedure
;;      but Guile 1.8.8 need to export %test-eval for test-assert macro.
;; 4. Check whether pass the test suite for SRFI 64;
;;    - http://srfi.schemers.org/srfi-64/srfi-64-test.scm is a test suite for
;;      SRFI 64.
;;    - 51 expected passes and 2 expected failures are normal.
;;    - something's wrong if unexpected failure.
;; 5. Try to include source line numbers when reporting;
;;    - there are pre-defined %->form, %->file and %->line procedures,
;;      these procedures are used in %line procedure, for some languages.
;;    - there are pre-defined %quote macro, this macro used in %line procedure
;;      and some macros like test-assert, for some languages.
;;    - for example, test-assert macro use syntax-case for some languages
;;      like Guile and use %quote macro and %line procedure.
;; 6. Try to improve a exception trapping procedure;
;;    - there are pre-defined %exception procedure for some languages.

(cond-expand
 (guile
  ;; 1. Put these files to srfi and srfi/srfi-64 directories.
  ;;    $ mkdir -p srfi/srfi-64
  ;;    $ cp #(some location)#/srfi-64-port.scm srfi/srfi-64
  ;;    $ cp #(some location)#/srfi-64.scm srfi
  ;; 2. Use -L and --use-srfi option.
  ;;    $ guile -L `pwd` --use-srfi=64
  (define-module (srfi srfi-64))
  ;; %cond-expand-features of Guile 2.0.5 is
  ;; '(guile guile-2 r5rs
  ;;   srfi-0 srfi-4 srfi-6 srfi-13 srfi-14 srfi-23 srfi-39 srfi-55 srfi-61)
  (use-modules (srfi srfi-9)
               ((srfi srfi-35) :select (condition-type?
                                        condition?
                                        condition-has-type?))
               ((ice-9 regex)  :select (string-match))
               (rnrs io ports)
               )
  (cond-expand
   (guile-2)
   (else
    ;; Guile 1.8 fails the test suite for testing srfi-64 by Donovan Kolbly
    ;; because of nested block comments used in srfi-64-test.scm file.
    ;; Comments are comments. So, not problem.
    (use-modules (ice-9 syncase)
                 (rnrs io ports)
                 ))))
 (kawa
  (module-compile-options warn-undefined-variable: #t
                          warn-invoke-unknown-method: #t)
  (provide 'srfi-64))
 (sisc
  (require-extension (srfi 9 34 35 39)))
 (else
  ;; For Chicken 4.7:
  ;; 1. Put this file to src directory.
  ;;    $ mkdir src
  ;;    $ cp #(some location)#/srfi-64.scm src
  ;; 2. Make a new srfi-64.scm file.
  ;;    $ echo \ 
  ;;    '(module srfi-64 () (import chicken scheme) (include "src/srfi-64"))' \
  ;;    > srfi-64.scm
  ;; 3. Compile that srfi-64.scm file.
  ;;    $ csc -feature compiling-extension -setup-mode -k -s -O3 -d1 \
  ;;    srfi-64.scm -j srfi-64
  ;;    $ csc -feature compiling-extension -setup-mode -k -s -O3 -d0 \
  ;;    srfi-64.import.scm
  ;; 4. Use -R option.
  ;;    $ csi -R srfi-64
  ;;
  ;; Gambit 4.6.5 fails the test suite for testing srfi-64 by Donovan Kolbly.
  ;; I suspect dynamic-wind bug of Gambit is cause of it.
  ;; So, simple test-cases like test-assert, test-equal, test-eqv, test-eq
  ;; and test-error are available but complex ones like test-group-with-cleanup
  ;; are faithless on Gambit just now.
))

(cond-expand
 (kawa
  (define-syntax %export
    (syntax-rules ()
      ((_ test-begin name ...) (module-export %test-begin name ...)))))
 (guile
  (cond-expand
   (guile-2
    (define-syntax %export
      (syntax-rules () ((_ name ...) (export name ...)))))
   (else
    (define-syntax %export
      (syntax-rules ()
        ((_ name ...)
         (export %test-begin
                 %test-end
                 %test-assert
                 %test-comp
                 %test-error
                 %test-should-execute
                 %test-eval
                 name ...)))))))
 (chicken
  (define-syntax %export
    (syntax-rules ()
      ((_ name ...)
       (export %test-begin
               %test-end
               %test-assert
               %test-comp
               %test-error
               %test-should-execute
               name ...)))))
 (else
  (define-syntax %export
    (syntax-rules () ((_ name ...) (if #f #f))))))
(%export test-begin                     ; test-begin must be listed first,
         test-end                       ; since in Kawa (at least)
         test-group                     ; it is "magic".
         test-group-with-cleanup
         test-assert
         test-equal
         test-eqv
         test-eq
         test-approximate
         test-error
         test-read-eval-string
         test-with-runner
         test-apply
         test-match-name
         test-match-nth
         test-match-any
         test-match-all
         test-skip
         test-expect-fail
         test-runner-group-path
         test-result-kind
         test-passed?
         test-result-ref
         test-result-set!
         test-result-remove
         test-result-clear
         test-log-to-file               ; not a part of the specification
         ;; Misc test-runner functions
         test-runner?
         test-runner-current
         test-runner-get
         test-runner-simple
         test-runner-null
         test-runner-create
         test-runner-factory
         test-runner-reset
         test-runner-test-name
         ;; test-runner field setter and getter functions
         ;; - see test-runner record definition:
         test-runner-pass-count
         test-runner-pass-count!        ; not a part of the specification
         test-runner-fail-count
         test-runner-fail-count!        ; not a part of the specification
         test-runner-xpass-count
         test-runner-xpass-count!       ; not a part of the specification
         test-runner-xfail-count
         test-runner-xfail-count!       ; not a part of the specification
         test-runner-skip-count
         test-runner-skip-count!        ; not a part of the specification
         test-runner-group-stack
         test-runner-group-stack!       ; not a part of the specification
         test-runner-on-test-begin
         test-runner-on-test-begin!
         test-runner-on-test-end
         test-runner-on-test-end!
         test-runner-on-group-begin
         test-runner-on-group-begin!
         test-runner-on-group-end
         test-runner-on-group-end!
         test-runner-on-final
         test-runner-on-final!
         test-runner-on-bad-count
         test-runner-on-bad-count!
         test-runner-on-bad-end-name
         test-runner-on-bad-end-name!
         test-result-alist
         test-result-alist!             ; not a part of the specification
         test-runner-aux-value
         test-runner-aux-value!
         ;; default/simple call-back functions,
         ;; used in default test-runner,
         ;; but can be called to construct more complex ones.
         test-on-test-begin-simple
         test-on-test-end-simple
         test-on-group-begin-simple
         test-on-group-end-simple
         test-on-bad-count-simple
         test-on-bad-end-name-simple
         test-on-final-simple)

(cond-expand
 (srfi-9
  (define-syntax %record-define
    (syntax-rules ()
      ((_ alloc runner? (name index getter setter) ...)
       (define-record-type
         <test-runner> (alloc) runner? (name getter setter) ...)))))
 (else
  (define-syntax %record-define
    (syntax-rules ()
      ((_ alloc runner? (name index getter setter) ...)
       (begin
         (define %runner-cookie '<test-runner>)
         (define (runner? obj)
           (and (vector? obj)
                (< 1 (vector-length obj))
                (eq? (vector-ref obj 0) %runner-cookie)))
         (define (alloc)
           (let ((runner (make-vector 23)))
             (vector-set! runner 0 %runner-cookie)
             runner))
         (begin
           (define (getter runner)
             (vector-ref runner index)) ...)
         (begin
           (define (setter runner value)
             (vector-set! runner index value)) ...)))))))
(%record-define
 %test-runner-alloc test-runner?
 ;; Cumulate count of all tests that have passed and were expected to.
 (pass-count       1 test-runner-pass-count      test-runner-pass-count!)
 (fail-count       2 test-runner-fail-count      test-runner-fail-count!)
 (xpass-count      3 test-runner-xpass-count     test-runner-xpass-count!)
 (xfail-count      4 test-runner-xfail-count     test-runner-xfail-count!)
 (skip-count       5 test-runner-skip-count      test-runner-skip-count!)
 (skip-list        6 %test-runner-skip-list      %test-runner-skip-list!)
 (fail-list        7 %test-runner-fail-list      %test-runner-fail-list!)
 ;; Normally #t, except when in a test-apply.
 (run-list         8 %test-runner-run-list       %test-runner-run-list!)
 (skip-save        9 %test-runner-skip-save      %test-runner-skip-save!)
 (fail-save       10 %test-runner-fail-save      %test-runner-fail-save!)
 (group-stack     11 test-runner-group-stack     test-runner-group-stack!)
 (on-test-begin   12 test-runner-on-test-begin   test-runner-on-test-begin!)
 (on-test-end     13 test-runner-on-test-end     test-runner-on-test-end!)
 ;; Call-back when entering a group. Takes (runner suite-name count).
 (on-group-begin  14 test-runner-on-group-begin  test-runner-on-group-begin!)
 ;; Call-back when leaving a group.
 (on-group-end    15 test-runner-on-group-end    test-runner-on-group-end!)
 ;; Call-back when leaving the outermost group.
 (on-final        16 test-runner-on-final        test-runner-on-final!)
 ;; Call-back when expected number of tests was wrong.
 (on-bad-count    17 test-runner-on-bad-count    test-runner-on-bad-count!)
 ;; Call-back when name in test=end doesn't match test-begin.
 (on-bad-end-name 18 test-runner-on-bad-end-name test-runner-on-bad-end-name!)
 ;; Cumulate count of all tests that have been done.
 (total-count     19 %test-runner-total-count    %test-runner-total-count!)
 ;; Stack (list) of (count-at-start . expected-count):
 (count-list      20 %test-runner-count-list     %test-runner-count-list!)
 (result-alist    21 test-result-alist           test-result-alist!)
 ;; Field can be used by test-runner for any purpose.
 ;; test-runner-simple uses it for a log file.
 (aux-value       22 test-runner-aux-value       test-runner-aux-value!))

(cond-expand
 (guile
  (define-syntax %test-eval
    (syntax-rules ()
      ((_ expr)
       (let ((value #f) (excpt #f))
         (begin
           (set! value (catch #t
                              (lambda () expr)
                              (lambda (key . arg)
                                (set! excpt (append (list key) arg))
                                #f)))
           (cons value excpt)))))))
 (chicken
  (define-syntax %test-eval
    (syntax-rules ()
      ((_ expr)
       (let ((value #f) (excpt #f))
         (begin
           (set! value (condition-case expr
                                       (exc () (begin
                                                 (set! excpt exc)
                                                 #f))))
           (cons value excpt)))))))
 (kawa
  (define-syntax %test-eval
    (syntax-rules ()
      ((_ expr)
       (let ((value #f) (excpt #f))
         (begin
           (set! value (try-catch expr
                                  (exc <java.lang.Throwable> (begin
                                                               (set! excpt exc)
                                                               #f))))
           (cons value excpt)))))))
 (mzscheme
  (define-syntax %test-eval
    (syntax-rules ()
      ((_ expr)
       (let ((value #f) (excpt #f))
         (begin
           (set! value (with-handlers (((lambda (exc) #t)
                                        (lambda (exc)
                                          (set! excpt exc)
                                          #f)))
                                      expr))
           (cons value excpt)))))))
 ((or srfi-34 gambit)
  (define-syntax %test-eval
    (syntax-rules ()
      ((_ expr)
       (let ((value #f) (excpt #f))
         (begin
           (set! value (with-exception-handler (lambda (exc)
                                                 (set! excpt exc)
                                                 #f)
                                               (lambda () expr)))
           (cons value excpt)))))))
 (else
  (define-syntax %test-eval
    (syntax-rules ()
      ((_ expr)
       (let ((value #f) (excpt #f))
         (begin
           (set! value expr)
           (cons value excpt))))))))

(cond-expand
 (guile
  (define-syntax %quote (syntax-rules () ((_ x) (syntax x)))))
 (else
  (define-syntax %quote (syntax-rules () ((_ x) (quote x))))))

(define (%line form)
  (cond-expand
   (guile
    (cond-expand
     (guile-2
      (define (%->form form) (datum->syntax form (syntax->datum form))))
     (else
      ;; Unlike Guile 2.0, Guile 1.8 does not have syntax->datum and
      ;; datum->syntax macros. I think there is another method. FIXME.
      (define (%->form form) #f))))
   ((or kawa mzscheme)
    (define (%->form form) (syntax-object->datum form)))
   (else
    (define (%->form form) #f)))        ; %->form ends here

  (cond-expand
   (guile
    (cond-expand
     (guile-2
      (define (%->file form)
         (let ((filename (assq-ref (syntax-source form) 'filename)))
           (if filename (basename filename) #f))))
     (else
      ;; Unlike Guile 2.0, Guile 1.8 does not have syntax-source macro.
      ;; I think there is another method. FIXME.
      (define (%->file form) #f))))
   (kawa
    (define (%->file form) (syntax-source form)))
   (mzscheme
    (define (%->file form)
      (let ((source (syntax-source form)))
        (cond ((string? source) source)
              ((path? source) (path->string source))
              (else #f)))))
   (else
    (define (%->file form) #f)))        ; %->file ends here

  (cond-expand
   (guile
    (cond-expand
     (guile-2
      (define (%->line form)
         ;; Line-number begins from 1 not 0.
         (1+ (assq-ref (syntax-source form) 'line))))
     (else
      ;; Unlike Guile 2.0, Guile 1.8 does not have syntax-source macro.
      ;; I think there is another method. FIXME.
      (define (%->line form) #f))))
   ((or kawa mzscheme)
    (define (%->line form) (syntax-line form)))
   (else
    (define (%->line form) #f)))        ; %->line ends here

  ;; actual definition of %line
  (let ((source-form (%->form form))
        (source-file (%->file form))
        (source-line (%->line form)))
    (if source-form
        (cons (cons (%quote source-form) source-form)
              (if source-file
                  (cons (cons (%quote source-file) source-file)
                        (if source-line
                            (cons (cons (%quote source-line) source-line)
                                  '())
                            '()))
                  '()))
        '())))

(define (%source-line runner)
  (let ((result (test-result-alist runner)))
    (let ((file (assq 'source-file result))
          (line (assq 'source-line result)))
      (if line
          (string-append (or file "") ":" (number->string (cdr line)) ": ")
          ""))))

(define (test-result-ref runner pname . default)
  (let ((p (assq pname (test-result-alist runner))))
    (cond ((< 1 (length default))
           (let ((msg (string-append
                       "Usage: (test-result-ref runner pname) "
                       "or (test-result-ref runner pname default)")))
             (error msg)))
          (p (cdr p))
          ((not (null? default)) (car default))
          (else #f))))

(define (test-result-set! runner pname value)
  (let ((alist (test-result-alist runner)))
    (let ((p (assq pname alist)))
      (if p
          (set-cdr! p value)
          (test-result-alist! runner (cons (cons pname value) alist))))))

(define (test-result-remove runner pname)
  (let ((alist (test-result-alist runner)))
    (let ((p (assq pname alist)))
      (if p
          (test-result-alist! runner (let loop ((r alist))
                                       (if (eq? r p)
                                           (cdr r)
                                           (cons (car r) (loop (cdr r))))))))))

(define (test-result-clear runner)
  (test-result-alist! runner '()))

(define (test-runner-test-name runner)
  (test-result-ref runner 'test-name ""))

(define (test-runner-group-path runner)
  (reverse (test-runner-group-stack runner)))

(define (test-runner-reset runner)
  (test-result-alist! runner '())
  (test-runner-pass-count!   runner 0)
  (test-runner-fail-count!   runner 0)
  (test-runner-xpass-count!  runner 0)
  (test-runner-xfail-count!  runner 0)
  (test-runner-skip-count!   runner 0)
  (%test-runner-total-count! runner 0)
  (%test-runner-count-list!  runner '())
  (%test-runner-run-list!    runner #t)
  (%test-runner-skip-list!   runner '())
  (%test-runner-fail-list!   runner '())
  (%test-runner-skip-save!   runner '())
  (%test-runner-fail-save!   runner '())
  (test-runner-group-stack!  runner '()))

;; Not part of the specification.  FIXME
;; Controls whether a log file is generated.
(cond-expand
 (srfi-39
  (define test-log-to-file (make-parameter #t)))
 (else
  (define %log-cookie #t)
  (define (test-log-to-file . arg)
    (if (not (null? arg))
        (if (< 1 (length arg))
            (error "Usage: (test-log-to-file) or (test-log-to-file value)")
            (set! %log-cookie (car arg))))
    %log-cookie)))

(define test-on-test-begin-simple #f)
(define test-on-test-end-simple   #f)
(let ()
  (define (%display pair port)
    (display "  " port)
    (display (car pair) port)
    (display ": " port)
    (write (cdr pair) port)
    (newline port))                     ; %display ends here

  ;; actual definition of test-on-test-begin-simple
  (set!
   test-on-test-begin-simple
   (lambda (runner)
     (let ((log (test-runner-aux-value runner)))
       (if (output-port? log)
           (let ((results (test-result-alist runner)))
             (let ((file (assq 'source-file results))
                   (line (assq 'source-line results))
                   (form (assq 'source-form results))
                   (name (assq 'test-name   results)))
               (display "Test begin:" log)
               (newline log)
               (if name (%display name log))
               (if file (%display file log))
               (if line (%display line log))
               (if form (%display form log))))))))

  ;; actual definition of test-on-test-end-simple
  (set!
   test-on-test-end-simple
   (lambda (runner)
     (let ((log (test-runner-aux-value runner))
           (kind (test-result-ref runner 'result-kind)))
       (if (output-port? log)
           (begin
             (display "Test end:" log)
             (newline log)
             (let loop ((alist (test-result-alist runner)))
               (if (pair? alist)
                   (let ((pair (car alist)))
                     ;; Write out properties not written out by on-test-begin.
                     (if (not
                          (memq
                           (car pair)
                           '(test-name source-file source-line source-form)))
                         (%display pair log))
                     (loop (cdr alist)))))))
       (if (memq kind '(fail xpass))
           (let ((results (test-result-alist runner)))
             (let ((source-file (assq 'source-file results))
                   (source-line (assq 'source-line results))
                   (test-name   (assq 'test-name   results)))
               (if (or source-file source-line)
                   (begin
                     (if source-file (display (cdr source-file)))
                     (display ":")
                     (if source-line (display (cdr source-line)))
                     (display ": ")
                     (display (if (eq? kind 'xpass) "XPASS" "FAIL"))
                     (if test-name
                         (begin (display " ") (display (cdr test-name))))
                     (newline))))))
       kind))))

(define (test-on-group-begin-simple runner suite-name count)
  (if (null? (test-runner-group-stack runner))
      (begin
        (display "%%%% Starting test ")
        (display suite-name)
        (let ((log-file (if (procedure? test-log-to-file)
                            (test-log-to-file)
                            test-log-to-file)))
          (if log-file
              (begin
                (if (not (output-port? log-file))
                    (let ((log-file-name (if (string? test-log-to-file)
                                             test-log-to-file
                                             (string-append
                                              suite-name ".log"))))
                      (cond-expand
                       (mzscheme
                        (set! log-file
                              (open-output-file log-file-name
                                                'truncate/replace)))
                       (guile-2
                        (set! log-file
                              #;(with-fluids ((%default-port-encoding "UTF-8"))
                                           (open-output-file log-file-name))
                              (standard-output-port)
                              ))
                       (else
                        (set! log-file (open-output-file log-file-name))))
                      (display "  (Writing full log to \"")
                      (display log-file-name)
                      (display "\")")))
                (test-runner-aux-value! runner log-file)
                (display "%%%% Starting test " log-file)
                (display suite-name log-file)
                (newline log-file))))
        (newline)))
  (let ((log (test-runner-aux-value runner)))
    (if (output-port? log)
        (begin
          (display "Group begin: " log)
          (display suite-name log)
          (newline log)))))

(define (test-on-group-end-simple runner)
  (let ((log (test-runner-aux-value runner)))
    (if (output-port? log)
        (begin
          (display "Group end: " log)
          (display (car (test-runner-group-stack runner)) log)
          (newline log)))))

(define (test-on-final-simple runner)
  (let ((log (test-runner-aux-value runner))
        (pass-count (test-runner-pass-count runner))
        (xfail-count (test-runner-xfail-count runner))
        (xpass-count (test-runner-xpass-count runner))
        (fail-count (test-runner-fail-count runner))
        (skip-count (test-runner-skip-count runner)))
    (define (%display port)
      (define (%display-if value label port)
        (if (> value 0)
            (begin
              (display label port)
              (display value port)
              (newline port))))
      ;; actual definition of %display
      (%display-if pass-count  "# of expected passes      " port)
      (%display-if xfail-count "# of expected failures    " port)
      (%display-if xpass-count "# of unexpected successes " port)
      (%display-if fail-count  "# of unexpected failures  " port)
      (%display-if skip-count  "# of skipped tests        " port))

    ;; actual definition of test-on-final-simple
    (if (output-port? log)
        (begin
          (%display log)
          (if (or (and (procedure? test-log-to-file)
                       (not (output-port? (test-log-to-file))))
                  (not (output-port? test-log-to-file)))
              (if (not (eq? standard-output-port log)) 
                  (close-output-port log)))))
    (%display (current-output-port))
    (list pass-count xfail-count xpass-count fail-count skip-count)))

(define (test-on-bad-count-simple runner count expected-count)
  (define (%display count expected-count port)
    (display "*** Total number of tests was " port)
    (display count port)
    (display " but should be " port)
    (display expected-count port)
    (display ". ***" port)
    (display
     "*** Discrepancy indicates testsuite error or exceptions. ***" port)
    (newline port))                     ; %display ends here

  ;; actual definition of test-on-bad-count-simple
  (let ((log (test-runner-aux-value runner)))
    (if (output-port? log)
        (%display count expected-count log))
    (%display count expected-count (current-output-port))))

(define (test-on-bad-end-name-simple runner begin-name end-name)
  (let ((msg (string-append
              (%source-line runner) "test-end " begin-name
              " does not match test-begin " end-name)))
    (error msg)))

(cond-expand
 (srfi-39
  (define test-runner-current (make-parameter #f)))
 (else
  (define %current-cookie #f)
  (define (test-runner-current . arg)
    (if (not (null? arg))
        (if (< 1 (length arg))
            (let ((msg (string-append
                        "Usage: (test-runner-current) "
                        "or (test-runner-current runner)")))
              (error msg))
            (set! %current-cookie (car arg))))
    %current-cookie)))
;; A safer wrapper to test-runner-current.
(define (test-runner-get)
  (let ((runner (test-runner-current)))
    (if (not runner)
        (error "test-runner not initialized - test-begin missing?"))
    runner))

(define (test-runner-simple)
  (let ((runner (%test-runner-alloc)))
    (test-runner-reset runner)
    (test-runner-on-test-begin! runner test-on-test-begin-simple)
    (test-runner-on-test-end! runner test-on-test-end-simple)
    (test-runner-on-group-begin! runner test-on-group-begin-simple)
    (test-runner-on-group-end! runner test-on-group-end-simple)
    (test-runner-on-final! runner test-on-final-simple)
    (test-runner-on-bad-count! runner test-on-bad-count-simple)
    (test-runner-on-bad-end-name! runner test-on-bad-end-name-simple)
    runner))

(define (test-runner-null)
  (let ((runner (%test-runner-alloc)))
    (test-runner-reset runner)
    (test-runner-on-test-begin! runner (lambda (runner) #f))
    (test-runner-on-test-end! runner (lambda (runner) #f))
    (test-runner-on-group-begin! runner (lambda (runner name count) #f))
    (test-runner-on-group-end! runner (lambda (runner) #f))
    (test-runner-on-final! runner (lambda (runner) #f))
    (test-runner-on-bad-count! runner (lambda (runner count expected) #f))
    (test-runner-on-bad-end-name! runner (lambda (runner begin end) #f))
    runner))

(cond-expand
 (srfi-39
  (define test-runner-factory (make-parameter test-runner-simple)))
 (else
  (define %factory-cookie test-runner-simple)
  (define (test-runner-factory . arg)
    (if (not (null? arg))
        (if (< 1 (length arg))
            (let ((msg (string-append
                        "Usage: (test-runner-factory) "
                        "or (test-runner-factory factory)")))
             (error msg))
            (set! %factory-cookie (car arg))))
    %factory-cookie)))
(define (test-runner-create)
  ((test-runner-factory)))

(define (test-result-kind . rest)
  (let ((runner (if (pair? rest) (car rest) (test-runner-current))))
    (test-result-ref runner 'result-kind)))

(define (test-passed? . rest)
  (let ((runner (if (pair? rest) (car rest) (test-runner-get))))
    (memq (test-result-ref runner 'result-kind) '(pass xpass))))

(define (test-match-name name)
  (lambda (runner)
    (equal? name (test-runner-test-name runner))))

(define (test-match-nth n . count)
  (let ((i 0) (kount (if (null? count) 1 (car count))))
    (if (< 1 (length count))
        (error "Usage: (test-match-nth n) or (test-match-nth n count)")
        (lambda (runner)
          (set! i (+ i 1))
          (and (>= i n) (< i (+ n kount)))))))

(define test-match-any       #f)
(define test-match-all       #f)
(define test-skip            #f)
(define test-expect-fail     #f)
(define %test-should-execute #f)
(let ()
  (define (%any . pred-list)
    (lambda (runner)
      (let ((result #f))
        (let loop ((lst pred-list))
          (if (null? lst)
              result
              (begin
                (if ((car lst) runner)
                    (set! result #t))
                (loop (cdr lst))))))))  ; %any ends here

  (define (%all . pred-list)
    (lambda (runner)
      (let ((result #t))
        (let loop ((lst pred-list))
          (if (null? lst)
              result
              (begin
                (if (not ((car lst) runner))
                    (set! result #f))
                (loop (cdr lst))))))))  ; %all ends here

  (define (%specifier specifier)
    (cond ((procedure? specifier) specifier)
          ((integer? specifier) (test-match-nth 1 specifier))
          ((string? specifier) (test-match-name specifier))
          (else
           (error "not a valid test specifier")))) ; %specifier ends here

  ;; actual definition of test-match-any
  (set!
   test-match-any
   (lambda (pred . args)
     (apply %any (%specifier pred) args)))

  ;; actual definition of test-match-all
  (set!
   test-match-all
   (lambda (pred . args)
     (apply %all (%specifier pred) args)))

  ;; actual definition of test-skip
  (set!
   test-skip
   (lambda (pred . args)
     (let ((runner (test-runner-get)))
       (%test-runner-skip-list! runner
                                (cons (apply
                                       test-match-all (%specifier pred) args)
                                      (%test-runner-skip-list runner))))))

  ;; actual definition of test-expect-fail
  (set!
   test-expect-fail
   (lambda (pred . args)
     (let ((runner (test-runner-get)))
       (%test-runner-fail-list! runner
                                (cons (apply
                                       test-match-all (%specifier pred) args)
                                      (%test-runner-fail-list runner))))))

  ;; actual definition of %test-should-execute
  (set!
   ;; Returns #f, #t, or 'xfail.
   %test-should-execute
   (lambda (runner)
     (let ((run-list (%test-runner-run-list runner)))
       (cond ((or (not (or (eqv? run-list #t)
                           ((apply %any run-list) runner)))
                  ((apply %any (%test-runner-skip-list runner)) runner))
              (test-result-set! runner 'result-kind 'skip)
              #f)
             (((apply %any (%test-runner-fail-list runner)) runner)
              (test-result-set! runner 'result-kind 'xfail)
              'xfail)
             (else #t))))))

(define-syntax test-with-runner
  (syntax-rules ()
    ((test-with-runner runner form ...)
     (let ((saved-runner (test-runner-current)))
       (dynamic-wind
           (lambda () (test-runner-current runner))
           (lambda () form ...)
           (lambda () (test-runner-current saved-runner)))))))

(define (test-apply first . rest)
  (if (test-runner? first)
      (test-with-runner first (apply test-apply rest))
      (let ((runner (test-runner-current)))
        (if runner
            (let ((run-list (%test-runner-run-list runner)))
              (cond ((null? rest)
                     (%test-runner-run-list! runner (reverse run-list))
                     (first))           ; actually apply procedure thunk
                    (else
                     (%test-runner-run-list!
                      runner
                      (if (eq? run-list #t) (list first) (cons first run-list)))
                     (apply test-apply rest)
                     (%test-runner-run-list! runner run-list))))
            (let ((runner (test-runner-create)))
              (test-with-runner runner (apply test-apply first rest))
              ((test-runner-on-final runner) runner))))))

(define (%test-begin suite-name . count)
  (if (not (test-runner-current)) (test-runner-current (test-runner-create)))
  (if (< 1 (length count))
      (error "Usage: (test-begin suite-name) or (test-begin suite-name count)"))
  (let ((runner (test-runner-current))
        (kount (if (null? count) #f (car count))))
    ((test-runner-on-group-begin runner) runner suite-name kount)
    (%test-runner-skip-save! runner
                             (cons (%test-runner-skip-list runner)
                                   (%test-runner-skip-save runner)))
    (%test-runner-fail-save! runner
                             (cons (%test-runner-fail-list runner)
                                   (%test-runner-fail-save runner)))
    (%test-runner-count-list! runner
                              (cons (cons (%test-runner-total-count runner)
                                          kount)
                                    (%test-runner-count-list runner)))
    (test-runner-group-stack! runner
                              (cons suite-name
                                    (test-runner-group-stack runner)))))
(cond-expand
 (kawa
  ;; Kawa has test-begin built in, implemented as:
  ;; (begin
  ;;   (cond-expand (srfi-64 #!void) (else (require 'srfi-64)))
  ;;   (%test-begin suite-name [count]))
  ;; This puts test-begin but only test-begin in the default environment.,
  ;; which makes normal test suites loadable without non-portable commands.
  )
 (else
  (define test-begin %test-begin)))

(define (%test-end suite-name line)
  (let ((runner (test-runner-get)))
    (test-result-alist! runner line)
    (let ((groups (test-runner-group-stack runner))
          (count-list (%test-runner-count-list runner)))
      (if (null? groups)
          (let ((msg (string-append
                      (%source-line runner) "test-end not in a group")))
            (error msg)))
      (if (and suite-name (not (equal? suite-name (car groups))))
          ((test-runner-on-bad-end-name runner) runner suite-name (car groups)))
      (let ((expected-count (cdar count-list))
            (saved-count (caar count-list)))
        (let ((group-count (- (%test-runner-total-count runner) saved-count)))
          (if (and expected-count (not (= expected-count group-count)))
              ((test-runner-on-bad-count runner) runner
                                                 group-count expected-count))
          ((test-runner-on-group-end runner) runner)
          (test-runner-group-stack! runner
                                    (cdr (test-runner-group-stack runner)))
          (%test-runner-skip-list!  runner
                                    (car (%test-runner-skip-save runner)))
          (%test-runner-skip-save!  runner
                                    (cdr (%test-runner-skip-save runner)))
          (%test-runner-fail-list!  runner
                                    (car (%test-runner-fail-save runner)))
          (%test-runner-fail-save!  runner
                                    (cdr (%test-runner-fail-save runner)))
          (%test-runner-count-list! runner (cdr count-list))
          (if (null? (test-runner-group-stack runner))
              ((test-runner-on-final runner) runner)))))))

(define %test-assert #f)
(define %test-comp   #f)
(define %test-error  #f)
(let ()
  (define (%begin runner)
    (%test-should-execute runner)
    ((test-runner-on-test-begin runner) runner)
    (not (eq? 'skip (test-result-ref runner 'result-kind))))
  
  (define (%end runner result)
    (test-result-set! runner
                      'result-kind
                      (if (eq? (test-result-ref runner 'result-kind) 'xfail)
                          (if result 'xpass 'xfail)
                          (if result 'pass 'fail))))

  (define (%report runner kind)
    (case kind
      ((pass)
       (test-runner-pass-count! runner
                                (+ 1 (test-runner-pass-count runner))))
      ((fail)
       (test-runner-fail-count! runner
                                (+ 1 (test-runner-fail-count runner))))
      ((xpass)
       (test-runner-xpass-count! runner
                                 (+ 1 (test-runner-xpass-count runner))))
      ((xfail)
       (test-runner-xfail-count! runner
                                 (+ 1 (test-runner-xfail-count runner))))
      (else
       (test-runner-skip-count! runner
                                (+ 1 (test-runner-skip-count runner)))))
    (%test-runner-total-count! runner
                               (+ 1 (%test-runner-total-count runner)))
    ((test-runner-on-test-end runner) runner))

  ;; actual definition of %test-assert
  (set!
   %test-assert
   (lambda (eval-pair line)
     (let ((runner (test-runner-get))
           (value  (car eval-pair))
           (excpt  (cdr eval-pair)))
       (test-result-alist! runner line)
       (if (%begin runner)
           (if excpt
               (begin
                 (test-result-set! runner 'actual-error excpt)
                 (%end runner #f))
               (begin
                 (test-result-set! runner 'actual-value value)
                 (%end runner value))))
       (%report runner (test-result-ref runner 'result-kind)))))

  ;; actual definition of %test-comp
  (set!
   %test-comp
   (lambda (pred-or-error expected eval-pair line)
     (let ((runner (test-runner-get))
           (value  (car eval-pair))
           (excpt  (cdr eval-pair)))
       (test-result-alist! runner line)
       (if (%begin runner)
           (begin
             (test-result-set! runner 'expected-value expected)
             (if excpt
                 (begin
                   (test-result-set! runner 'actual-error excpt)
                   (%end runner #f))
                 (begin
                   (test-result-set! runner 'actual-value value)
                   (%end runner (if (procedure? pred-or-error)
                                    (pred-or-error expected value)
                                    (and (>= value
                                             (- expected pred-or-error))
                                         (<= value
                                             (+ expected pred-or-error)))))))))
       (%report runner (test-result-ref runner 'result-kind)))))

  ;; definition of %test-error
  (set!
   %test-error
   (lambda (etype eval-pair line)
     (let ((runner (test-runner-get))
           (value  (car eval-pair))
           (excpt  (cdr eval-pair)))
       (cond-expand
        (guile
         (define (%exception)
           (let ((key (car excpt)) (args (cdr excpt)))
             (cond ((or (and (boolean? etype) etype) (equal? etype key))
                    #t)
                   ((string? etype)
                    (if (null? args)
                        (string-match etype key)
                        (let ((message (cadr args)))
                          (cond ((string-match etype message)
                                 #t)
                                ((< 2 (length args))
                                 (string-match
                                  etype
                                  (apply
                                   simple-format #f message (caddr args))))
                                (else #f)))))
                   ((procedure? etype)
                    (etype excpt))
                   ((condition-type? etype)
                    (and (condition? excpt) (condition-has-type? excpt etype)))
                   (else #f)))))
        (kawa
         (define (%exception)
           (cond ((or (and (boolean? etype) etype) (equal? etype excpt))
                  #t)
                 ((and (instance? etype <gnu.bytecode.ClassType>)
                       (gnu.bytecode.ClassType:isSubclass
                        etype
                        <java.lang.Throwable>))
                  (instance? excpt etype))
                 (else #f))))
        (srfi-35
         (define (%exception)
           (cond ((or (and (boolean? etype) etype) (equal? etype excpt))
                  #t)
                 ((procedure? etype)
                  (etype excpt))
                 ((condition-type? etype)
                  (and (condition? excpt) (condition-has-type? excpt etype)))
                 (else #f))))
        (else
         (define (%exception)
           (cond ((or (and (boolean? etype) etype) (equal? etype excpt))
                  #t)
                 ((procedure? etype)
                  (etype excpt))
                 (else #f)))))          ; %exception ends here

       ;; actual definition of %test-error
       (test-result-alist! runner line)
       (if (%begin runner)
           (begin
             (test-result-set! runner 'expected-error etype)
             (if excpt
                 (begin
                   (test-result-set! runner 'actual-error excpt)
                   (%end runner (%exception)))
                 (begin
                   (test-result-set! runner 'actual-value value)
                   (%end runner #f)))))
       (%report runner (test-result-ref runner 'result-kind))))))

(cond-expand
 (guile
  (define (test-read-eval-string string)
    (let ((port (open-input-string string)))
      (let ((form (read port)))
        (if (eof-object? (read-char port))
            (primitive-eval form)
            (error "(not at eof)"))))))
 (else
  (define (test-read-eval-string string)
    (let ((port (open-input-string string)))
      (let ((form (read port)))
        (if (eof-object? (read-char port))
            (eval form)
            (error "(not at eof)")))))))

(cond-expand
 ((or guile kawa mzscheme)
  (define-syntax test-end
    (lambda (x)
      (syntax-case (list x (list (%quote quote) (%line x))) ()
        (((_) line)
         (syntax (%test-end #f line)))
        (((_ suite-name) line)
         (syntax (%test-end suite-name line)))))))
 (else
  (define-syntax test-end
    (syntax-rules ()
      ((_)
       (%test-end #f '()))
      ((_ suite-name)
       (%test-end suite-name '()))))))

(define-syntax test-group
  (syntax-rules ()
    ((test-group suite-name . body)
     (let ((runner (test-runner-current)))
       ;; Ideally should also set line-number, if available.
       (test-result-alist! runner (list (cons 'test-name suite-name)))
       (if (%test-should-execute runner)
           (dynamic-wind
               (lambda () (test-begin suite-name))
               (lambda () . body)
               (lambda () (test-end   suite-name))))))))

(define-syntax test-group-with-cleanup
  (syntax-rules ()
    ((test-group-with-cleanup suite-name form cleanup-form)
     (test-group suite-name
                 (dynamic-wind
                     (lambda () #f)
                     (lambda () form)
                     (lambda () cleanup-form))))
    ((test-group-with-cleanup suite-name cleanup-form)
     (test-group-with-cleanup suite-name #f cleanup-form))
    ((test-group-with-cleanup suite-name form1 form2 form3 . rest)
     (test-group-with-cleanup suite-name (begin form1 form2) form3 . rest))))

(cond-expand
 ((or guile kawa mzscheme)
  (define-syntax test-assert
    (lambda (x)
      (syntax-case (list x (list (%quote quote) (%line x))) ()
        (((_ name expr) line)
         (syntax
          (%test-assert (%test-eval expr) (cons (cons 'test-name name) line))))
        (((_ expr) line)
         (syntax (%test-assert (%test-eval expr) line)))))))
 (else
  (define-syntax test-assert
    (syntax-rules ()
      ((_ name expr)
       (%test-assert (%test-eval expr) (cons (cons 'test-name name) '())))
      ((_ expr)
       (%test-assert (%test-eval expr) '()))))))

(cond-expand
 ((or guile kawa mzscheme)
  (define-syntax test-equal
    (lambda (x)
      (syntax-case (list x (list (%quote quote) (%line x))) ()
        (((_ name expected expr) line)
         (syntax
          (%test-comp equal? expected (%test-eval expr)
                      (cons (cons 'test-name name) line))))
        (((_ expected expr) line)
         (syntax (%test-comp equal? expected (%test-eval expr) line)))))))
 (else
  (define-syntax test-equal
    (syntax-rules ()
      ((_ name expected expr)
       (%test-comp equal? expected (%test-eval expr)
                   (cons (cons 'test-name name) '())))
      ((_ expected expr)
       (%test-comp equal? expected (%test-eval expr) '()))))))

(cond-expand
 ((or guile kawa mzscheme)
  (define-syntax test-eqv
    (lambda (x)
      (syntax-case (list x (list (%quote quote) (%line x))) ()
        (((_ name expected expr) line)
         (syntax
          (%test-comp eqv? expected (%test-eval expr)
                      (cons (cons 'test-name name) line))))
        (((_ expected expr) line)
         (syntax (%test-comp eqv? expected (%test-eval expr) line)))))))
 (else
  (define-syntax test-eqv
    (syntax-rules ()
      ((_ name expected expr)
       (%test-comp eqv? expected (%test-eval expr)
                   (cons (cons 'test-name name) '())))
      ((_ expected expr)
       (%test-comp eqv? expected (%test-eval expr) '()))))))

(cond-expand
 ((or guile kawa mzscheme)
  (define-syntax test-eq
    (lambda (x)
      (syntax-case (list x (list (%quote quote) (%line x))) ()
        (((_ name expected expr) line)
         (syntax
          (%test-comp eq? expected (%test-eval expr)
                      (cons (cons 'test-name name) line))))
        (((_ expected expr) line)
         (syntax (%test-comp eq? expected (%test-eval expr) line)))))))
 (else
  (define-syntax test-eq
    (syntax-rules ()
      ((_ name expected expr)
       (%test-comp eq? expected (%test-eval expr)
                   (cons (cons 'test-name name) '())))
      ((_ expected expr)
       (%test-comp eq? expected (%test-eval expr) '()))))))

(cond-expand
 ((or guile kawa mzscheme)
  (define-syntax test-approximate
    (lambda (x)
      (syntax-case (list x (list (%quote quote) (%line x))) ()
        (((_ name expected expr err) line)
         (syntax
          (%test-comp err expected (%test-eval expr)
                      (cons (cons 'test-name name) line))))
        (((_ expected expr err) line)
         (syntax (%test-comp err expected (%test-eval expr) line)))))))
 (else
  (define-syntax test-approximate
    (syntax-rules ()
      ((_ name expected expr err)
       (%test-comp err expected (%test-eval expr)
                   (cons (cons 'test-name name) '())))
      ((_ expected expr err)
       (%test-comp err expected (%test-eval expr) '()))))))

(cond-expand
 ((or guile kawa mzscheme)
  (define-syntax test-error
    (lambda (x)
      (syntax-case (list x (list (%quote quote) (%line x))) ()
        (((_ name etype expr) line)
         (syntax
          (%test-error etype (%test-eval expr)
                       (cons (cons 'test-name name) line))))
        (((_ etype expr) line)
         (syntax (%test-error etype (%test-eval expr) line)))
        (((_ expr) line)
         (syntax (%test-error #t (%test-eval expr) line)))))))
 (else
  (define-syntax test-error
    (syntax-rules ()
      ((_ name etype expr)
       (%test-error etype (%test-eval expr) (cons (cons 'test-name name) '())))
      ((_ etype expr)
       (%test-error etype (%test-eval expr) '()))
      ((_ expr)
       (%test-error #t (%test-eval expr) '()))))))
