;CS 4337.007
	;Group Members
		;Jonathan Strong - JMS210010
		;Oluwadamilare Sunmola - OAS220005



; Question 3: Pascal's Triangle Row Sum
(define (pascal n)
  (if (= n 0)
      1
      (* 2 (pascal (- n 1)))))

(define (pascal-display n)
  (display "Pascal Sum Row ") (display n) (display ": ")
  (display (pascal n)) (newline))

(pascal-display 0)
(pascal-display 1)
(pascal-display 2)
(pascal-display 3)
(pascal-display 4)

; Question 4: Sum of Odd Numbers Less than 8 in a List
(define (sumodd lst)
  (define (flatten lst)
    (cond
      ((null? lst) '())
      ((pair? (car lst))
       (append (flatten (car lst)) (flatten (cdr lst))))
      (else
       (cons (car lst) (flatten (cdr lst))))))

  (define (sum-odd-less-than-8 lst)
    (cond
      ((null? lst) 0)
      ((and (integer? (car lst))
            (odd? (car lst))
            (< (car lst) 8))
       (+ (car lst) (sum-odd-less-than-8 (cdr lst))))
      (else (sum-odd-less-than-8 (cdr lst)))))

  (sum-odd-less-than-8 (flatten lst)))

(display "Sum of Odd Numbers: ")
(display (sumodd '((2 6) 1 3 ( () 5) 8))) ; Expected Output: 9
(newline)

; Question 5: Delete Third Element in a List
(define (deleteitem lst)
  (if (< (length lst) 3)
      lst
      (append (list (car lst))
              (list (cadr lst))
              (cdddr lst))))

(display "Delete third element: ")
(display (deleteitem '(a (c d) (b) e))) ; Expected Output: '(a (c d) e)
(newline)

; Question 6: Swap Successive Elements in a List
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

(display "Swapped list: ")
(display (swap-pairs '(a b c (a b) (c d)))) ; Expected Output: '(b a (b a) c (d c))
(newline)

; Question 7: Last Element of a List
(define (last-element lst)
  (if (null? (cdr lst))
      (list (car lst))
      (last-element (cdr lst))))

(display "Last element: ")
(display (last-element '(a (b) c (d e)))) ; Expected Output: '(d e)
(newline)

; Question 8: Extract Leaves of a Tree in Right-to-Left Order
(define (leaves tree)
  (cond
    ((null? tree) '())  
    ((not (pair? tree)) (list tree))  
    (else (append (leaves (cdr tree)) (leaves (car tree))))))

(display "Leaves in right-to-left order: ")
(display (leaves '(((1 2) (3 4 5)) ((1) (3 4) (5))))) ; Expected Output: '(5 4 3 1 5 4 3 2 1)
(newline)

; Question 9: Maximum Depth of Nested Parentheses
(define (EXP-DEPTH lst)
  (define (helper lst depth max-depth)
    (cond
      ((null? lst) max-depth)
      ((not (pair? lst)) max-depth)
      (else
       (max (helper (car lst) (+ depth 1) (+ depth 1))
            (helper (cdr lst) depth max-depth)))))
  (helper lst 0 0))

(display "Max depth of nested lists: ")
(display (EXP-DEPTH '(A B ((C (D (E) F)) G) H))) ; Expected Output: 5
(newline)

; Question 10: Generate All Subsets of a Set
(define (subsets s)
  (if (null? s)
      '(()) 
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(display "Subsets of a set: ")
(display (subsets '(a b c))) ; Expected Output: '((a b c) (b c) (a c) (c) (a b) (b) (a) ())
(newline)
