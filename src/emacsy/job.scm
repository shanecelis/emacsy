(define-module (emacsy job)
  #:use-module (srfi srfi-9)
  #:use-module (emacsy coroutine)
  #:use-module (emacsy agenda)
  #:export (<job>
            job?
            make-job
            get-job-id
            suspend-job
            continue-job))
 
(define-record-type <job>
  (%make-job job-id job-state job-exit-value job-cont) 
  job?
  (job-id job-id)
  (job-state job-state set-job-state!)
  (job-exit-value job-exit-value set-job-exit-value!)
  (job-cont job-cont set-job-cont!))

(define *current-job-list* '())

(define next-job-id 1)

(define* (make-job thunk)
  "Creates a coroutine that has some job control."
  (let ((job (%make-job next-job-id 'baby #f #f)))
   (define (handler cont key . args)
     (define (resume . args)
       (format #t "resuming job ~a~%" (job-id job))       
       ;; Call continuation that resumes the procedure.
       (call-with-prompt 'coroutine-prompt 
                         (lambda () (apply cont args))
                         handler))
     (define (job-resume . args)
       (if (eq? (job-state job) 'running)
           (apply resume args)
           (begin
             (set-job-cont! job job-resume)
             (format #t "job ~a unable to resume because it is ~a~%" 
                     (job-id job)
                     (job-state job)))))
     (case key
       ((callback)
        (when (procedure? (car args))
          (apply (car args) job-resume (cdr args))))
       ((user-data)
        (resume job))))
   (set! next-job-id (1+ next-job-id))
   (set! *current-job-list* (cons job *current-job-list*))
   (lambda () 
     (if (eq? (job-state job) 'baby)
         (begin 
           (set-job-state! job 'running)
           (format #t "starting job ~a~%" (job-id job))
           (call-with-prompt 'coroutine-prompt (lambda () (job-exit (thunk))) handler))
         (throw 'job-already-started)))))

(define (suspend-job job)
  "Suspend a job that is currently running."
  (set-job-state! job 'suspended))

(define (continue-job job)
  "Continue a suspended job and schedule it to be run."
  (when (eq? (job-state job) suspended)
    (set-job-state! job 'running)
    ;; job isn't resumable or callable.
    (agenda-schedule (job-cont job))
    (set-job-cont! job #f)))

(define (wait-for-job job)
  "Waits for a job to complete."
  ;; XXX This should be smarter than polling.
  (yield (lambda (resume)
           (while #t
             (when (eq? (job-state job) 'zombie)
                 (agenda-schedule (lambda () (resume (job-exit-value job)))))
             (wait)))))

(define (job-exit return-value)
  "Exit the job with the given return-value."
  (let ((job (couser-data))) 
    (set-job-state! job 'zombie)
    (set-job-exit-value! job return-value))
  (yield (lambda (resume)
           return-value)))

(define (get-job-id)
  "Returns the job-id within the job."
  (job-id (couser-data)))
