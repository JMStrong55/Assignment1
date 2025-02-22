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
(define (swap-pairs lst)
  (define (swap-recursive sublist)
    (cond
      ((not (list? sublist)) sublist)
      ((null? sublist) '())
      ((null? (cdr sublist)) sublist)
      (else
       (let ((first (car sublist))
             (second (cadr sublist))
             (rest (cddr sublist)))
         (append
          (if (and (pair? sublist) (pair? (cdr sublist))) (list second first) (list first))
          (swap-recursive rest))))))

  (map swap-recursive (swap-recursive lst)))


(display (swap-pairs '(a b c (a b) (c d))))
(newline)

;Question 7
(define (last-element lst)
    (if (null? (cdr lst))
        (list (car lst))
        (last-element (cdr lst))))

;Question 8

;Question 9
(define (EXP-DEPTH lst)
        (define (helper lst depth max-depth)
          (cond
            ((null? lst) max-depth)
            ((not (pair? lst)) max-depth)
            (else
             (max (helper (car lst) (+ depth 1) (+ depth 1))
                  (helper (cdr lst) depth max-depth)))))
        (helper lst 0 0))

;Question 10
