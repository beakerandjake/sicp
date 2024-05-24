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
        (error "FRONT called with an empty queue" front-ptr)
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
             (error "DELETE! called with an empty queue" front-ptr))
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
(define (make-agenda) (list 0))

; returns the current simulation time.
(define (current-time agenda) (car agenda))

; updates the current time of the agend
(define (set-current-time! agenda time)
  (set-car! agenda time))

; returns all of the segments of the agenda
(define (segments agenda) (cdr agenda))

; sets the segment list on the agenda.
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

; returns the first segment of the agenda. 
(define (first-segment agenda) (car (segments agenda)))

; skips the first segment and returns the remaining sgements of the agenda.
(define (rest-segments agenda) (cdr (segments agenda)))

; updates the current time of the agenda
(define (set-current-time agenda time)
  (set-car! agenda time))

; returns true if the specified agenda is empty
(define (empty-agenda? agenda)
  (null? (segments agenda)))

; modifies the agenda by adding the given action procedure to be run at the specified time
(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
      (insert-queue! (segment-queue (car segments))
                     action)
      (let ((rest (cdr segments)))
        (if (belongs-before? rest)
          (set-cdr! segments
                    (cons (make-new-time-segment time action)
                          (cdr segments)))
          (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
      (set-segments! agenda
                     (cons (make-new-time-segment time action) 
                           segments))
      (add-to-segments! segments))))


; returns the first item from the agenda and updates the current time. 
(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
    (error "Agenda is empty: FIRST-AGENDA-ITEM" agenda)
    (let ((first-seg (first-segment agenda)))
      (set-current-time! agenda
                         (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))

; modifies the agenda by removing the first items
(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))



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

; ================================================
; Simulate
; ================================================

; executes each procedure on the agenda in sequence
(define (propagate)
  (if (empty-agenda? the-agenda)
    'done
    (let ((first-item (first-agenda-item the-agenda)))
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (propagate agenda))))

; ================================================
; Primitive digital logic functions
; ================================================

; returns the logical not of the signal
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

; returns the logical and of the two signals
(define (logical-and a b)
  (if (and (= 1 a) (= 1 b))
    1
    0))

; returns the logicla or of the two signals
(define (logical-or a b)
  (if (or (= 1 a) (= 1 b))
    1 
    0))

; connects an input wire to the inverter which inverts the signal to the output wire
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)

; builds an gate from the two input wires and outputs the logical and to the output wire.
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
            (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)
    

; builds or gate from the two input wires and outputs the logical or to the output wire.
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
            (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

; returns a new half adder 
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

; reutrns a new full adder
;                     _______
;                    |       |-------------- sum
; A -----------------| half- |
;                    | adder |------,
;                    |_______|      |
;          _______     |           _|__
;         |       |----'          |    |
; B ------| half- |               | or |---- c(out)
;         | adder |---------------|____|
; c(in)---|_______|
;
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-age c1 c2 c-out)
    'ok))

