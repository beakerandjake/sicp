; SECTION 3.3.4 A Simulator for Digital Circuits

; ================================================
; Queue
; ================================================

; returns an empty queue
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?) (null? front-ptr))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (front-queue)
      (if (empty-queue?)
        (error "FRONT called with an empty queue" queue)
        (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair))
              (else
                (set-cdr! rear-ptr new-pair)
                (set-rear-ptr! new-pair)))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue" queue))
            (else (set-front-ptr! (cdr front-ptr)))))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            (else (error "Unknown operation: Queue" m))))
      dispatch))

; returns true if the queue is empty
(define (empty-queue? queue)
  ((queue 'empty-queue?)))

; returns the object at the front of the queue, signaling an error if the queue is empty
(define (front-queue queue)
  ((queue 'front-queue)))

; inserts the item at the rear of the queue, returning the modified queue
(define (insert-queue! queue item)
  ((queue 'insert-queue!) item))

; removes the item at the front of the queue and returns the modified queue as its value, signaling an errror if the queue is empty before deletion
(define (delete-queue! queue)
  ((queue 'delete-queue!)))

; ================================================
; Agenda
; ================================================

; returns a new time segment
(define (make-time-segment time queue)
  (cons time queue))

; returns the time portion of the time segment
(define (segment-time s) (car s))

; returns the queue portion of the time segment
(define (segment-queue s) (cdr s))

; returns a new empty agenda
(define make-agenda (list 0))

; returns the current simulation time.
(define (current-time agenda) (car agenda))

; updates the current time of the agenda
(define (set-current-time agenda time)
  (set-car! agenda time))

; returns true if the specified agenda is empty
(define empty-agenda? 0)

; returns the first item on the agenda 
(define first-agenda-item 0)

; modifies the agenda by removing the first items
(define remove-first-agenda-item! 0)

; modifies the agenda by adding the given action procedure to be run at the specified time
(define add-to-agenda! 0)


; ================================================
; Wires
; ================================================

; calls each procedure in a list of no-argument procedures
(define call-each
  (lambda (procedures)
    (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures))))))


; returns a new wire
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
        (begin (set! signal-value new-value)
               (call-each action-procedures))
        'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
        (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operator: WIRE" m))))
    dispatch))

; returns the signal of the wire.
(define (get-signal wire) (wire 'get-signal))

; mutates the wire, updating the signal
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

; adds a new action on the wire which is invoked when the signal changes. 
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))


; adds to the main agenda
(define (after-delay delay action)
  (add-to-agenda (+ delay (current-time the-agenda))
                 action
                 the-agenda))

; executes each procedure on the agenda in sequence
(define (propagate)
  (if (empty-agenda? the-agenda)
    'done
    (let ((first-item (first-agenda-item the-agenda)))
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (propagate))))


