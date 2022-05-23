#lang racket
; ---------------------------------------------------------------------------------------------------

(define V '(0 1 2 3 4 5))
(define E '((5 2) (5 0) (4 0) (4 1) (2 3) (3 1)))

;; getting the tuples from a set in accordance with the func.
;; func, list -> list of tuples.
(define (get-tuples func st)
  (foldl func '() st))


;; get tuples with 1 in them.
(get-tuples (lambda (arg result)
              (cond [(findf (lambda (arg1) 
                              (or (equal? 1 arg1))) arg) (cons arg result)]
                    [else result])) E)

;; get tuples with second element as 2.
(get-tuples (lambda (arg result)
              (cond [(equal? (cadr arg) 2) (cons arg result)]
                    [else result])) E)

;; get minimal vertices from the vertices list V.
;; list, list -> list of elements.
(define (get-minimal-elements V E)
  (foldl (lambda (arg result)
           (cond [(empty? (get-tuples 
                            (lambda (arg1 result)
                              (cond [(equal? (cadr arg1) arg) (cons arg1 result)]
                                    [else result])) E)) (cons arg result)]
                 [else result])) '() V))


(get-minimal-elements '(2 3 4 5)  '((3 4) (3 5)))

;; the algorithm to get the topological sort.
;; list, list => list
(define (topo-sort V E)
  (foldl (lambda (arg result)
           (define v (caadr result))
           (define e (cdadr result))
           (define res (car result))
           (define minimal (get-minimal-elements v e))
           (cond [(empty? minimal) result]
                 [else (list (cons (car minimal) res)
                             (cons (filter (lambda (arg)
                                             (not (equal? arg (car minimal)))) v)
                                   (filter (lambda (arg)
                                             (not (or  (equal? (car arg) (car minimal))
                                                       (equal? (cdr arg)
                                                               (car minimal))))) e)))]))
         (list '() (cons V E)) V))

(topo-sort V E)

; ---------------------------------------------------------------------------------------------------
