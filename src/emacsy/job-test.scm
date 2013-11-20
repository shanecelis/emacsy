(use-modules (emacsy coroutine)
             (emacsy agenda)
             (emacsy job)
             (check))

(use-private-modules (emacsy job))

(set! job-id-next 1)

(check *current-job-list* => '())

(define a (make-job (lambda ()
                      (get-job-id))))


(check *current-job-list* => (list (%make-job 1 'baby #f #f)))

(check (a) => 1)

(check *current-job-list* => (list (%make-job 1 'zombie 1 #f)))

(check-throw (a) => 'job-already-started)

(check *current-job-list* => (list (%make-job 1 'zombie 1 #f)))

(set! *current-job-list* '())

(define b (make-job (lambda ()
                      1
                      (wait)
                      2)))

(check *current-job-list* => (list (%make-job 2 'baby #f #f)))
(check (b) => *unspecified*)
(check *current-job-list* => (list (%make-job 2 'running #f #f)))
(check-throw (b) => 'job-already-started)
(check *current-job-list* => (list (%make-job 2 'running #f #f)))
(update-agenda)
(check *current-job-list* => (list (%make-job 2 'zombie 2 #f)))

(set! *current-job-list* '())

(define c (make-job (lambda ()
                      1
                      (wait)
                      2)))

(check *current-job-list* => (list (%make-job 3 'baby #f #f)))
(check (c) => *unspecified*)
(check *current-job-list* => (list (%make-job 3 'running #f #f)))
(check-throw (c) => 'job-already-started)
(check *current-job-list* => (list (%make-job 3 'running #f #f)))
(suspend-job (car *current-job-list*))
(check *current-job-list* => (list (%make-job 3 'suspended #f #f)))
(update-agenda)
(check *current-job-list* => (list (%make-job 3 'suspended #f #f)))
(continue-job (car *current-job-list*))
(check *current-job-list* => (list (%make-job 3 'running #f #f)))
(update-agenda)
(check *current-job-list* => (list (%make-job 3 'zombie 2 #f)))

(set! *current-job-list* '())

(define d (make-job (lambda ()
                      1
                      (job-exit 3)
                      2)))

(check *current-job-list* => (list (%make-job 4 'baby #f #f)))
(check (d) => 3)
(check *current-job-list* => (list (%make-job 4 'zombie 3 #f)))

(check-exit)
