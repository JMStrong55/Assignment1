;Question 3
(define (pascal n)
    (if (= n 0)
        1
        (* 2 (pascal (- n 1)))))

(define (pascal-display n) display (pascal n))

;Question 4

;Question 5
(define (deleteitem lst)
      (if (< (length lst) 3)
          lst
          (append (list (car lst))
                  (list (cadr lst))
                  (cdddr lst))))

;Question 6

;Question 7
(define (last-element lst)
    (if (null? (cdr lst))
        (list (car lst))
        (last-element (cdr lst))))

;Question 8
(define (EXP-DEPTH lst)
        (define (helper lst depth max-depth)
          (cond
            ((null? lst) max-depth)
            ((not (pair? lst)) max-depth)
            (else
             (max (helper (car lst) (+ depth 1) (+ depth 1))
                  (helper (cdr lst) depth max-depth)))))
        (helper lst 0 0))

;Question 9

;Question 10
