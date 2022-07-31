(library (lbst)

  (export make-lbst
          lbst-set
          lbst-ref
          lbst-delete
          lbst-size
          lbst-start
          lbst-end
          lbst-search
          lbst-next
          lbst-previous
          lbst-key
          lbst-value

          lbst->alist

          check~lbst-001
          check~lbst-002
          check~lbst-003
          check~lbst-004
          check~lbst-005
          check~lbst-006
          check~lbst-007
          check~lbst-008
          check~lbst-009
          check~lbst-010
          check~lbst-011
          check~lbst-012
          check~lbst-013
          check~lbst-014
          check~lbst-015
          )

  (import (chezscheme) (r999))

  (define-record-type* <lbst>
    (make-lbst* key value size left right stack)
    lbst?
    (key lbst-key)
    (value lbst-value)
    (size lbst-size)
    (left lbst-left)
    (right lbst-right)
    ;; Keep around the top-down path of lbst in the stack in order to
    ;; be later able to "backtrack".
    (stack lbst-stack))

  (define lbst-null (make-lbst* #vu8() #f 0 #f #f '()))

  (define (make-lbst)
    lbst-null)

  (define lbst-null?
    (lambda (lbst)
      (eq? lbst lbst-null)))

  (define bit-length fxlength)

  (define lbst<?
    (lambda (a b)
      (fx<? (bit-length a) (bit-length b))))

  (define too-big?
    (lambda (a b)
      (lbst<? a (fxarithmetic-shift-right b 1))))

  (define lbst-join
    (lambda (key value left right)
      (make-lbst* key
                 value
                 (fx+ (lbst-size left)
                      (lbst-size right)
                      1)
                 left
                 right
                 '())))

  (define lbst-single-left-rotation
    (lambda (key value left right)
      (lbst-join (lbst-key right)
                 (lbst-value right)
                 (lbst-join key value left (lbst-left right))
                 (lbst-right right))))

  (define lbst-double-left-rotation
    (lambda (key value left right)
      (lbst-join (lbst-key (lbst-left right))
                 (lbst-value (lbst-left right))
                 (lbst-join key
                            value
                            left
                            (lbst-left (lbst-left right)))
                 (lbst-join (lbst-key right)
                            (lbst-value right)
                            (lbst-right (lbst-left right))
                            (lbst-right right)))))

  (define lbst-single-right-rotation
    (lambda (key value left right)
      (lbst-join (lbst-key left)
                 (lbst-value left)
                 (lbst-left left)
                 (lbst-join key
                            value
                            (lbst-right left)
                            right))))

  (define lbst-double-right-rotation
    (lambda (key value left right)
      (lbst-join (lbst-key (lbst-right left))
                 (lbst-value (lbst-right left))
                 (lbst-join (lbst-key left)
                            (lbst-value left)
                            (lbst-left left)
                            (lbst-left (lbst-right left)))
                 (lbst-join key
                            value
                            (lbst-right (lbst-right left))
                            right))))


  (define lbst-rebalance
    (lambda (key value left right)
      (if (too-big? (lbst-size left) (lbst-size right))
          (if (not (lbst<? (lbst-size (lbst-right right))
                           (lbst-size (lbst-left right))))
              (lbst-single-left-rotation key value left right)
              (lbst-double-left-rotation key value left right))
          (if (too-big? (lbst-size right) (lbst-size left))
              (if (not (lbst<? (lbst-size (lbst-left left))
                               (lbst-size (lbst-right left))))
                  (lbst-single-right-rotation key value left right)
                                      (lbst-double-right-rotation key value left right))
              ;; otherwise join both trees with a top level node
              (lbst-join key value left right)))))

  (define (bytevector-compare bytevector other)
    ;; Returns the symbol 'smaller if BYTEVECTOR is before OTHER,
    ;; returns the bytevector 'equal if they are equal and otherwise
    ;; returns 'bigger
    (let ((end (fxmin (bytevector-length bytevector)
                      (bytevector-length other))))
      (let loop ((index 0))
        (if (fx=? end index)
            ;; BYTEVECTOR and OTHER are equal until index; BYTEVECTOR
            ;; is smaller lexicographically, if it is smaller in
            ;; length.
            (if (fx=? (bytevector-length bytevector)
                      (bytevector-length other))
                'equal
                (if (fx<? (bytevector-length bytevector)
                          (bytevector-length other))
                    'smaller
                    'bigger))
            (let ((delta (fx- (bytevector-u8-ref bytevector index)
                              (bytevector-u8-ref other index))))
              (if (fxzero? delta)
                  (loop (fx+ 1 index))
                  (if (fxnegative? delta)
                      'smaller
                      'bigger)))))))

  (define lbst-set
    (lambda (lbst key value)
      ;; XXX: the empty bytevector aka. #vu8 is forbidden as key, guess why.
      (assert (not (bytevector=? key #vu8())))
      (if (lbst-null? lbst)
          (make-lbst* key value 1 lbst-null lbst-null '())
          (case (bytevector-compare key (lbst-key lbst))
            (smaller (lbst-rebalance (lbst-key lbst)
                                     (lbst-value lbst)
                                     (lbst-set (lbst-left lbst) key value)
                                     (lbst-right lbst)))
            (equal (lbst-join key value (lbst-left lbst) (lbst-right lbst)))
            (bigger (lbst-rebalance (lbst-key lbst)
                                    (lbst-value lbst)
                                    (lbst-left lbst)
                                    (lbst-set (lbst-right lbst) key value)))
            (else (error 'lbst "Unexpected bytevector-compare return value"))))))

  (define lbst-balanced?
    (lambda (lbst)
      (if (lbst-null? lbst)
          #t
          (and (not (too-big? (lbst-size (lbst-left lbst)) (lbst-size (lbst-right lbst))))
               (not (too-big? (lbst-size (lbst-right lbst)) (lbst-size (lbst-left lbst))))
               (lbst-balanced? (lbst-left lbst))
               (lbst-balanced? (lbst-right lbst))))))

  (define lbst-start
    (lambda (lbst)
      (let loop ((lbst lbst)
                 (stack (lbst-stack lbst)))
        (if (not (lbst-null? (lbst-left lbst)))
            (loop (lbst-left lbst)
                  (cons lbst stack))
            (make-lbst* (lbst-key lbst)
                        (lbst-value lbst)
                        (lbst-size lbst)
                        (lbst-left lbst)
                        (lbst-right lbst)
                        stack)))))

  (define lbst-end
    (lambda (lbst)
      (let loop ((lbst lbst)
                 (stack (lbst-stack lbst)))
        (if (not (lbst-null? (lbst-right lbst)))
            (loop (lbst-right lbst)
                  (cons lbst stack))
            (make-lbst* (lbst-key lbst)
                        (lbst-value lbst)
                        (lbst-size lbst)
                        (lbst-left lbst)
                        (lbst-right lbst)
                        stack)))))

  (define lbst-next
    (lambda (lbst)
      (if (lbst-null? (lbst-right lbst))
          (let loop ((lbst lbst)
                     (stack (lbst-stack lbst)))
            (if (null? stack)
                #f
                (let ((parent (car stack)))
                  (if (bytevector=? (lbst-key (lbst-left parent)) (lbst-key lbst))
                      (make-lbst* (lbst-key parent)
                                  (lbst-value parent)
                                  (lbst-size parent)
                                  (lbst-left parent)
                                  (lbst-right parent)
                                  (cdr stack))
                      (loop (car stack) (cdr stack))))))
          (let loop ((lbst (lbst-right lbst))
                     (stack (cons lbst (lbst-stack lbst))))
            (if (lbst-null? (lbst-left lbst))
                (make-lbst* (lbst-key lbst)
                            (lbst-value lbst)
                            (lbst-size lbst)
                            (lbst-left lbst)
                            (lbst-right lbst)
                            stack)
                (loop (lbst-left lbst) (cons lbst stack)))))))

  (define lbst-previous
    (lambda (lbst)
      (if (lbst-null? (lbst-left lbst))
          (let loop ((lbst lbst)
                     (stack (lbst-stack lbst)))
            (if (null? stack)
                #f
                (let ((parent (car stack)))
                  (if (bytevector=? (lbst-key (lbst-right parent)) (lbst-key lbst))
                      (make-lbst* (lbst-key parent)
                                  (lbst-value parent)
                                  (lbst-size parent)
                                  (lbst-left parent)
                                  (lbst-right parent)
                                  (cdr stack))
                      (loop (car stack) (cdr stack))))))
          (let loop ((lbst (lbst-left lbst))
                     (stack (cons lbst (lbst-stack lbst))))
            (if (lbst-null? (lbst-right lbst))
                (make-lbst* (lbst-key lbst)
                            (lbst-value lbst)
                            (lbst-size lbst)
                            (lbst-left lbst)
                            (lbst-right lbst)
                            stack)
                (loop (lbst-right lbst) (cons lbst stack)))))))

  (define lbst->alist
    (lambda (lbst)
      (let loop ((lbst (lbst-end lbst))
                 (out '()))
        (if (not lbst)
            out
            (loop (lbst-previous lbst) (cons (cons (lbst-key lbst) (lbst-value lbst)) out))))))

  (define (make-lbst** lbst stack)
    (make-lbst* (lbst-key lbst)
                (lbst-value lbst)
                (lbst-size lbst)
                (lbst-left lbst)
                (lbst-right lbst)
                stack))

  (define lbst-search
    (lambda (lbst key)
      (if (lbst-null? lbst)
          (values #f lbst-null)
          (let loop ((lbst lbst)
                     (stack (lbst-stack lbst)))
            (case (bytevector-compare key (lbst-key lbst))
              (smaller (if (null? stack)
                           (if (lbst-null? (lbst-left lbst))
                               (values 'smaller (make-lbst** lbst stack))
                               (loop (lbst-left lbst) (cons lbst stack)))

                           (let ((parent (car stack)))
                             (if (bytevector=? (lbst-key (lbst-right parent)) (lbst-key lbst))
                                 (if (eq? (bytevector-compare (lbst-key parent) key) 'smaller)
                                     (if (lbst-null? (lbst-left lbst))
                                         (values 'bigger (make-lbst** lbst stack))
                                         (loop (lbst-left lbst) (cons lbst stack)))
                                     (loop (car stack) (cdr stack)))
                                 (if (lbst-null? (lbst-left lbst))
                                     (values 'bigger (make-lbst** lbst stack))
                                     (loop (lbst-left lbst) (cons lbst stack)))))))
              (bigger (if (null? stack)

                          (if (lbst-null? (lbst-right lbst))
                              (values 'smaller (make-lbst** lbst stack))
                              (loop (lbst-right lbst) (cons lbst stack)))

                          (let ((parent (car stack)))
                            (if (bytevector=? (lbst-key (lbst-left parent)) (lbst-key lbst))
                                (if (eq? (bytevector-compare key (lbst-key parent)) 'bigger)
                                    (if (lbst-null? (lbst-right lbst))
                                        (values 'smaller (make-lbst** lbst stack))
                                        (loop (lbst-right lbst) (cons lbst stack)))
                                    (loop (car stack) (cdr stack)))
                                (if (lbst-null? (lbst-right lbst))
                                    (values 'smaller (make-lbst** lbst stack))
                                    (loop (lbst-right lbst) (cons lbst stack)))))))
              (else (values 'equal (make-lbst** lbst stack))))))))

  (define lbst-ref
    (lambda (lbst key)
      (call-with-values (lambda () (lbst-search lbst key))
        (lambda (position lbst)
          (if (eq? position 'equal)
              lbst
              #f)))))

  (define lbst-delete
    (lambda (lbst key)

      (define lbst-delete-min
        (lambda (lbst)
          (assert (not (lbst-null? lbst)))
          (if (lbst-null? (lbst-left lbst))
              (lbst-right lbst)
              (lbst-rebalance (lbst-key lbst-key)
                              (lbst-value lbst-value)
                              (lbst-delete-min (lbst-left lbst))
                              (lbst-right lbst)))))

      (define lbst-concat3
        (lambda (key value left right)
          (if (lbst-null? (lbst-left left))
              (lbst-set right key value)
              (if (lbst-null? (lbst-right left))
                  (lbst-set left key value)
                  (cond
                   ((too-big? (lbst-size left) (lbst-size right))
                    (lbst-rebalance (lbst-key right)
                                    (lbst-value right)
                                    left
                                    (lbst-concat3 key
                                                  value
                                                  (lbst-left right)
                                                  (lbst-right right))))
                   ((too-big? (lbst-size right) (lbst-size left))
                    (lbst-rebalance (lbst-key left)
                                    (lbst-value left)
                                    (lbst-concat3 key
                                                  value
                                                  (lbst-left left)
                                                  (lbst-right left))
                                    right))
                   (else (lbst-rebalance key value left right)))))))

      (define lbst-concat2
        (lambda (lbst other)
          (if (lbst-null? lbst)
              other
              (if (lbst-null? other)
                  lbst
                  (call-with-values (lambda () (lbst-search other #vu8()))
                    (lambda (_ min)
                      (lbst-concat3 (lbst-key lbst)
                                    (lbst-value lbst)
                                    lbst
                                    (lbst-delete-min other))))))))


      (if (lbst-null? lbst)
          #f
          (case (bytevector-compare key (lbst-key lbst))
            (bigger (lbst-rebalance (lbst-key lbst)
                                    (lbst-value lbst)
                                    (lbst-left lbst)
                                    (lbst-delete (lbst-right lbst) key)))
            (smaller (lbst-rebalance (lbst-key lbst)
                                     (lbst-value lbst)
                                     (lbst-delete (lbst-left lbst) key)
                                     (lbst-right lbst)))
            (else (lbst-concat2 (lbst-left lbst) (lbst-right lbst)))))))



  (define pk
    (lambda args
      (display ";; ") (write args) (newline)
      (car (reverse args))))

  (define check~lbst-001
    (lambda ()
      (assert (lbst-null? lbst-null))))

  (define check~lbst-002
    (lambda ()
      (let* ((lbst (make-lbst))
             (lbst (lbst-set lbst #vu8(42) 42))
             (lbst (lbst-set lbst #vu8(13) 13))
             (lbst (lbst-set lbst #vu8(14) 14))
             (lbst (lbst-set lbst #vu8(101) 101))
             )
        (assert (equal? (lbst->alist lbst)
                        '((#vu8(13) . 13)
                          (#vu8(14) . 14)
                          (#vu8(42) . 42)
                          (#vu8(101) . 101)))))))

  (define check~lbst-003
    (lambda ()
      (let* ((lbst (make-lbst))
             (lbst (lbst-set lbst #vu8(42) 42))
             (lbst (lbst-set lbst #vu8(13) 13))
             (lbst (lbst-set lbst #vu8(14) 14))
             (lbst (lbst-set lbst #vu8(101) 101)))
        (assert (fx<? (lbst-value lbst) (lbst-value (lbst-next lbst)))))))

  (define check~lbst-004
    (lambda ()
      (let* ((lbst (make-lbst))
             (lbst (lbst-set lbst #vu8(42) 42))
             (lbst (lbst-set lbst #vu8(13) 13))
             (lbst (lbst-set lbst #vu8(14) 14))
             (lbst (lbst-set lbst #vu8(101) 101)))
        (assert (fx<? (lbst-value lbst) (lbst-value (lbst-next lbst)))))))

  (define check~lbst-005
    (lambda ()
      (let* ((lbst (make-lbst))
             (lbst (lbst-set lbst #vu8(42) 42))
             (lbst (lbst-set lbst #vu8(13) 13))
             (lbst (lbst-set lbst #vu8(14) 14))
             (lbst (lbst-set lbst #vu8(101) 101)))
        (let* ((a (lbst-value (lbst-previous lbst)))
               (b (lbst-value (lbst-next (lbst-previous lbst))))
               (c (lbst-value (lbst-previous (lbst-next lbst))))
               (d (lbst-value (lbst-next lbst))))
          (assert (fx<=? a b c d))))))

  (define check~lbst-006
    (lambda ()
      (let* ((lbst (make-lbst))
             (lbst (lbst-set lbst #vu8(42) 42))
             (lbst (lbst-set lbst #vu8(13) 13))
             (lbst (lbst-set lbst #vu8(14) 14))
             (lbst (lbst-set lbst #vu8(101) 101)))
        (assert (fx=? (lbst-value (lbst-start lbst)) 13)))))

  (define check~lbst-007
    (lambda ()
      (let* ((lbst (make-lbst))
             (lbst (lbst-set lbst #vu8(42) 42))
             (lbst (lbst-set lbst #vu8(13) 13))
             (lbst (lbst-set lbst #vu8(14) 14))
             (lbst (lbst-set lbst #vu8(101) 101)))
        (assert (fx=? (lbst-value (lbst-end lbst)) 101)))))

  ;; same as lbst->alist but we start from the first key until
  ;; the end.
  (define lbst->alist/reversed
    (lambda (lbst)
      (let loop ((lbst (lbst-start lbst))
                 (out '()))
        (if (not lbst)
            out
            (loop (lbst-next lbst) (cons (cons (lbst-key lbst) (lbst-value lbst)) out))))))

  (define check~lbst-008
    (lambda ()
      (let* ((lbst (make-lbst))
             (lbst (lbst-set lbst #vu8(42) 42))
             (lbst (lbst-set lbst #vu8(13) 13))
             (lbst (lbst-set lbst #vu8(14) 14))
             (lbst (lbst-set lbst #vu8(101) 101)))

        (assert (equal? (reverse (lbst->alist/reversed lbst)) (lbst->alist lbst))))))

  (define check~lbst-009
    (lambda ()

      (let* ((lbst (make-lbst))
             (lbst (lbst-set lbst #vu8(42) 42))
             (lbst (lbst-set lbst #vu8(13) 13))
             (lbst (lbst-set lbst #vu8(14) 14))
             (lbst (lbst-set lbst #vu8(101) 101)))

        (let loop ((vs (list 13 14 42 101)))
          (unless (null? vs)
            (call-with-values (lambda () (lbst-search lbst (bytevector (car vs))))
              (lambda (position lbst)
                (assert (eq? position 'equal))
                (assert (= (lbst-value lbst) (car vs)))
                (loop (cdr vs)))))))))

  (define check~lbst-010
    (lambda ()

      (let* ((lbst (make-lbst))
             (lbst (lbst-set lbst #vu8(42) 42))
             (lbst (lbst-set lbst #vu8(13) 13))
             (lbst (lbst-set lbst #vu8(14) 14))
             (lbst (lbst-set lbst #vu8(101) 101)))

        (let loop ((vs (list (list 7 'bigger 13)
                             (list 15 'bigger 42)
                             (list 208 'smaller 101)
                             (list 13 'equal 13))))
          (unless (null? vs)
            (call-with-values (lambda () (lbst-search lbst (bytevector (list-ref (car vs) 0))))
              (lambda (position lbst)
                (assert (eq? (list-ref (car vs) 1) position))
                (assert (= (lbst-value lbst) (list-ref (car vs) 2)))
                (loop (cdr vs)))))))))

  (define check~lbst-011
    (lambda ()
      (let* ((lbst (make-lbst))
             (lbst (lbst-set lbst #vu8(42) 42))
             (lbst (lbst-set lbst #vu8(13) 13))
             (lbst (lbst-set lbst #vu8(14) 14))
             (lbst (lbst-set lbst #vu8(101) 101)))

        (let loop ((vs (list 42 13 14 101)))
          (unless (null? vs)
            (assert (equal? (car vs) (lbst-value (lbst-ref lbst (bytevector (car vs))))))
            (loop (cdr vs)))))))

  (define check~lbst-012
    (lambda ()
      (let* ((lbst (make-lbst))
             (lbst (lbst-set lbst #vu8(42) 42))
             (lbst (lbst-set lbst #vu8(13) 13))
             (lbst (lbst-set lbst #vu8(14) 14))
             (lbst (lbst-set lbst #vu8(101) 101)))

        (let loop ((vs (list 41 0 7 9 255)))
          (unless (null? vs)
            (assert (not (lbst-ref lbst (bytevector (car vs)))))
            (loop (cdr vs)))))))

  (define check~lbst-013
    (lambda ()
      (let* ((lbst (make-lbst))
             (lbst (lbst-set lbst #vu8(42) 42))
             (lbst (lbst-set lbst #vu8(13) 13))
             (lbst (lbst-set lbst #vu8(14) 14))
             (lbst (lbst-set lbst #vu8(101) 101)))

        (let ((new (lbst-delete lbst #vu8(42))))
          (assert (equal? (lbst->alist new)
                          '((#vu8(13) . 13) (#vu8(14) . 14) (#vu8(101) . 101))))))))

  (define bytevector<?
    (lambda (a b)
      (case (bytevector-compare a b)
        (smaller #t)
        (else #f))))

  (define check~lbst-014
    (lambda ()
      (let loop ((lbst (make-lbst))
                 (out '())
                 (count 16))
        (if (fxzero? count)
            (begin
              (assert (equal? (lbst->alist lbst) (sort (lambda (a b) (bytevector<? (car a) (car b))) out))))
            (let ((key (make-bytevector 8))
                  (value (random (expt 2 64))))
              (bytevector-u64-set! key 0 value 'little)
              (loop (lbst-set lbst key value) (cons (cons key value) out) (fx- count 1)))))))

  (define check~lbst-015
    (lambda ()
      (let loop ((lbst (make-lbst))
                 (out '())
                 (count 16))
        (if (fxzero? count)
            (begin
              (assert (equal? (lbst->alist/reversed lbst) (reverse (sort (lambda (a b) (bytevector<? (car a) (car b))) out)))))
            (let ((key (make-bytevector 8))
                  (value (random (expt 2 64))))
              (bytevector-u64-set! key 0 value 'little)
              (loop (lbst-set lbst key value) (cons (cons key value) out) (fx- count 1)))))))

  )
