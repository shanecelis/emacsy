;; convenience-lambda.scm
;;
;; This syntax was inspired by arc and Clojure's anonymous procedure
;; syntax.  
;;
;; #.\ (+ %1 %2) -> (lambda (%1 %2) (+ %1 %2))
;; #.\ (+ % %%)  -> (lambda (% %%)  (+ % %%))
;;
;; The .\ is supposed to approximate the lowercase lambda character in
;; ascii.
;;
;; Shane Celis

(define-module (convenience-lambda)
  #:use-module (ice-9 regex))

(eval-when (compile load eval)
(define (convenience-lambda char port)
(let ((uses-numbers? 'unknown)) 
    (define (range a b)
     (if (> a b)
         '()
         (cons a (range (1+ a) b))))
   (define (scan-tree lst)
     "scan-tree :: tree -> number"
     (cond
      ((symbol? lst)
       (let ((str (symbol->string lst)))
         (if (char=? #\% (string-ref str 0))
             (if (string-match "^%[0-9]+" str)
                 (if uses-numbers? 
                     (begin
                       (set! uses-numbers? #t)
                       (string->number (string-trim str #\%)))
                     (error "Pick a convention. Use %, %% or %1, %2 not both."))
                 (if (string-match "^%+" str)
                     (if (or (eq? uses-numbers? 'unknown) 
                             (eq? uses-numbers? #f)) 
                         (begin 
                           (set! uses-numbers? #f)
                           (string-length str))
                         (error "Pick a convention. Use %, %% or %1, %2 not both.")
                         )
                     (error "Expected %1 or % to name positional variables; found '~a' instead." str)))
             0)))
      ((pair? lst)
       (max (scan-tree (car lst)) (scan-tree (cdr lst))))
      (else 0)))
   (if (char=? #\\ (read-char port))
       (let* ((content (read port))
              (arg-count (scan-tree content)))
         (display (number->string arg-count))
         `(lambda ,(map (lambda (x) 
                          (string->symbol
                           (if uses-numbers?
                               (string-concatenate (list "%" (number->string x)))
                               (make-string x #\%)))) 
                        (range 1 arg-count))
            ,content))
       (error "Expected form like #.\\ (+ %1 %2)"))))

(read-hash-extend #\. convenience-lambda))
