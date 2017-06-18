(define HASH_MASK #xfff)
(define (make-assoc-hash)
  (make-vector (+ 1 HASH_MASK) '()))
(define (assoc-hash-insert! table key value)
  (let ((index (& (string-hash key) HASH_MASK)))
    (vector-set! table index
      (cons (cons key value) (vector-ref table index)))))
(define (assoc-hash-insert-replace! table key value)
  (let* ((index (& (string-hash key) HASH_MASK))
         (existent (assoc key (vector-ref table index))))
    (if existent
      (set-cdr! existent value)
      (vector-set! table index
        (cons (cons key value) (vector-ref table index))))))
(define (assoc-hash key table)
  (assoc key (vector-ref table (& (string-hash key) HASH_MASK))))
