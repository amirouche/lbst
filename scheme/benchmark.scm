(import (chezscheme))
(import (lbst))
65;6800;1c

(define pk
  (lambda args
    (display ";; ") (write args) (newline)
    (car (reverse args))))

(define check~lbst-benchmark
  (lambda ()

    (define MAX (expt 2 42))


    (define scale 1000000000)

    (define (jiffies-per-second)
      scale)

    (define (current-jiffy)
      (let ((t (current-time 'time-monotonic)))
        (+ (* scale (time-second t))
           (time-nanosecond t))))

    (define timeit
      (lambda (thunk)
        (let* ((start (current-jiffy))
               (out (thunk))
               (delta (- (current-jiffy) start)))
          (pk (inexact (/ delta scale)))
          out)))

    (define (make-random-values count)
      (let loop ((index count)
                 (out '()))
        (if (fxzero? index)
            out
            (loop (fx- index 1)
                  (let ((bytevector (make-bytevector 8))
                        (value (random MAX)))
                    (bytevector-u64-set! bytevector 0 value 'little)
                    (cons (cons bytevector value) out))))))

    (let ((values (time (make-random-values (pk 'kv-count (fx* (expt 2 16)))))))
      (pk (timeit (lambda ()
                    (let foo ((index 100)
                              (out 0))
                      (if (fxzero? index)
                          out
                          (let loop ((values values)
                                     (lbst (make-lbst)))
                            (if (null? values)
                                (let ((lbst (lbst-start lbst)))
                                  (foo (fx- index 1) (+ out (lbst-value lbst))))
                                (loop (cdr values) (lbst-set lbst (caar values) (cdar values)))))))))))




    ))

(check~lbst-benchmark)
