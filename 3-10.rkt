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

;Question 8

;Question 9

;Question 10
