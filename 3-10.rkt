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
      ((null? (cdr sublist)) (list (swap-recursive (car sublist))))
      (else
       (let ((first (swap-recursive (car sublist)))
             (second (swap-recursive (cadr sublist)))
             (rest (swap-recursive (cddr sublist))))
         (append (list second first) rest)))))

  (swap-recursive lst))

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
(define (subsets left right)
  (if (zero? (string-length right))
      (list left)  ; Base case: return a list containing the current subset
      (append (subsets (string-append left (substring right 0 1)) (substring right 1))  ; Pick first character
              (subsets left (substring right 1)))))  ; Do not pick first character


(subsets "" "abc")
