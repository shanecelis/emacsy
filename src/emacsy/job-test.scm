(use-modules (emacsy coroutine)
             (emacsy agenda)
             (emacsy job)
             (ice-9 receive)
             (check))

(use-private-modules (emacsy job))

(set! next-job-id 1)

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
(check (job-state (car *current-job-list*)) => 'suspended)
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

(set! *current-job-list* '())

(define e (make-job 
           (lambda ()
             (receive (proc job) 
                 (make-job (lambda ()
                             (wait)
                             'f))
               (wait)
               (proc)
               (wait-for-job job)))))


(check *current-job-list* => (list (%make-job 5 'baby #f #f)))
(e)
(check *current-job-list* => (list (%make-job 6 'baby #f #f)
                                   (%make-job 5 'running #f #f)))
(update-agenda)
(check *current-job-list* => (list (%make-job 6 'running #f #f)
                                   (%make-job 5 'running #f #f)))
(update-agenda)
(check *current-job-list* => (list (%make-job 6 'zombie 'f #f)
                                   (%make-job 5 'zombie 'f #f)))

(check (format #f "~a" (car *current-job-list*)) => "#<job id: 6 state: zombie exit-value: f>")

(check-report)
(check-exit)
