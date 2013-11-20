(use-modules (emacsy coroutine)
             (check))

(define a (make-coroutine 
           (lambda ()
             (couser-data)
             #;(yield (lambda (resume)
                      (resume 'b)
                      ))
             ;'a
             )
           'a
           'a-user-data))

(check (a) => 'a-user-data)
