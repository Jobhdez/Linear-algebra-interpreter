#lang racket
(require test-engine/racket-tests)

;; An Expr is one of:
;; - Atom
;; - SL

;; An SL is one of:
;; '()
;; - (cons Expr SL)

;; An Atom is one of:
;; - Vector
;; - Matrice
;; - Definition
;; - Variable
;; - Application

;; A Application (ie procedure) is one:
;; - Symbol
;; - Linear-Combination
;; - Norm
;;

;; eval: Expr -> Expr
(define (eval expr env)
  (cond ((vec? expr) expr)
	((matrix? expr) expr)
	((definition? expr) (eval-definition expr env))
	((variable? expr) (lookup-variable-value expr env))
	((application? expr) (applyl (eval (operator expr) env)
				     (map eval
					     (operands expr))))
	(else (error "Unknown Linear Algebra Expression"))))


;; A Vector is a:
;; - List-of-numbers

;; A List-of-numbers is one:
;; - '()
;; - (cons Number List-of-numbers)

;; make-vector: Number1 Number2 ... NumberN -> Vector
;; constructs a Vector from components, in this case Numbers.
(define (make-vector . numbers)
  numbers)

;; first-element: Vector -> Number
(define (first-element vec)
  (car vec))
(check-expect (first-element (make-vector 2 3 4 5 6)) 2)
;; rest-elements: Vector -> Vector
(define (rest-elements vec)
  (cdr vec))
(check-expect (rest-elements (make-vector 2 3 4)) (make-vector 3 4))
;; vectorp: Expr -> Boolean
(define (vec? expr)
  (list-of-numbers? expr))
(check-expect (vec? (make-vector 2 3 4)) #t)

;; list-of-numbersp: Vector -> boolean
(define (list-of-numbers? vec)
  (cond ((null? vec) #t)
	((not (number? (first-element vec))) #f)
	(else (and (number? (first-element vec))
		(list-of-numbers? (cdr vec))))))
(check-expect (list-of-numbers? (make-vector 2 3 4)) #t)
(check-expect (list-of-numbers? (make-vector 2 'a 3)) #f)
;; A Matrix is a:
;; - A List-of-rows

;; A List-of-rows is a:
;; - '()
;; - (cons Column List-of-rows)

;; A Column is a:
;; - Number

;; make-matrix vectors -> Matrix
(define (make-matrix . args) args)

;; matrix? Expr -> bool
(define (matrix? expr)
  (list-of-vectors? expr))

(define (list-of-vectors? expr)
  (cond ((null? expr) #t)
	((not (vec? (first-element expr))) #f)
	(else (and (vec? (first-element expr))
		(list-of-vectors? (cdr expr))))))

;; get-element: Matrix Row Column -> Number
;; given a matrix it gets the element in row i and
;; column j.
;; given: (get-element '((2 3 4) (5 6 7)) 2 1)
;; expect: 6
(define (get-element matrix row column)
  (let ((rowx (getrow matrix row (length matrix))))
    (list-ref  rowx column)))
(check-expect (get-element (make-matrix '(2 3 4) '(5 6 7)) 2 1)
              6)
;; A Row is a list of numbers
;; getrow: matrix row number -> row
;; given: (getrow '((2 3 4) (3 4 5)) 2 2)
;; expect: (3 4 5)
(define (getrow matrix row number-of-rows)
  (cond ((equal? number-of-rows 1) (car matrix))
	(else (getrow (cdr matrix)
		      row
		      (- number-of-rows 1)))))
(check-expect (getrow (make-matrix '(2 3 4) '(3 4 5)) 2 2)
              '(3 4 5))

;; A Definition is an Expr
(define (make-defintion var value) (list 'define var value))
(define (get-defintion-variable expr) (car (cdr expr)))
(define (get-definition-value expr) (car (cdr (cdr expr))))
(define (definition? expr) (equal?  (car expr) 'define))

;; A Variable is an Atom:

(define (variable? expr) (symbol? expr))

;; An Application is an Expr
(define (application? expr) (pair? expr))
(define (operator expr) (car expr))
(define (operands expr) (cdr expr))
(define (first-operand expr) (car expr))
(define (rest-operands expr) (cdr expr))

;; eval-definition: Expr Env -> Expr
;; this procedeure puts an entry in the environment, namely a variable bound to a value.
(define (eval-definition expr env)
  (hash-set env
            (get-defintion-variable expr)
            (eval (get-definition-value expr) env)))

;; lookup-variable-value: Var Env ->  Expr
(define (lookup-variable-value var env)
  (hash-ref env var))
;; applyl: Procedure Arguments -> Expr
(define (applyl proc args)
  (apply-in-underlying-racket
   (primitive-implementation proc) args))

(define apply-in-underlying-racket  apply)

(define (primitive-implementation proc)
  (car (cdr proc)))


(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

;;; Primitive Procedures

;; getnorm: Expr -> Number
;; computes the size of a vector
(define (get-norm vec p)
  (define (term vec)
    (expt (abs (first-element vec)) p))
  (expt (sum vec term p cdr)
	(expt 1 p)))
(check-expect (get-norm (make-vector 1 2 3) 1) 6)

(define (sum expr term p next)
  (if (null? expr)
      0
      (+ (term expr)
         (sum (cdr expr) term p next))))

(define (add expr1 expr2)
  (if (vec? expr1)
      (add-vectors expr1 expr2)
      (+ expr1 expr1)))

(define (add-vectors expr1 expr2)
  (cond ((null? expr1) expr2)
        ((null? expr2) expr1)
        ((equal? expr1 0) expr2)
        ((equal? expr2 0) expr1)
        ((and (= (length expr1) 1)
              (= (length expr2) 1))
         (cons (+ (car expr1) (car expr2)) '()))
        (else (cons (+ (car expr1) (car expr2))
                    (add-vectors (cdr expr1) (cdr expr2))))))
(check-expect (add-vectors (make-vector 2 3) (make-vector 3 4))
              (make-vector 5 7))


;; combine: Matrix Vector -> Vector
;; given: (combine '((2 3) (3 4)) '(2 3) 1)
;; expect:'(13 18)
;; given: (combine '((2 3) (3 4)) '(3)) 1)
;; (6 9)  + (9 12) = (15 21)
;; given: (combine '((2 3) (3 4)) '() 1)
;; '((2 3) (3 4))
(define (combine matrix vec i)
  (if (>= i (length vec))
      0
      (add-vectors (term matrix vec i)
                   (combine matrix vec (next i)))))
(check-expect (combine (make-matrix '(2 3) '(4 5))
                       (make-vector 3 4) 0)
                (make-vector 18 32))

;; term: Matrix Vec Number -> Vector
;; given: (term '((2 3 4) '(3 4 5)) '(3 4) 0)
;; expect: '(6 9)
(define (term matrix vec i)
  (multiply (list-ref vec i) (get-columni matrix (+ i 1))))
(check-expect (term (make-matrix '(2 3 5) '(3 4 5))
                    (make-vector 3 4) 0)
              (make-vector 6 9))
(define (next i) (+ i 1))

;; multiply: Number Vector -> Vector
;; given: (multiply 3 '(3 4 5))
;; expect: '(9 12 15)
(define (multiply scalar columnvector)
  (map (lambda (x) (* scalar x))
       columnvector))
(check-expect (multiply 3 (make-vector 3 4 5)) (make-vector 9 12 15))

;; get-columni: Matrix Number -> Vector
;; given: (get-columni '((2 3 4) (3 4 6)) 1)
;; expect: '(2 3)
(define (get-columni matrix i)
  (cond ((null? matrix) '())
        (else (cons (list-ref (car matrix)
			      (- i  1))
		    (get-columni (cdr matrix)
				 i)))))
(check-expect (get-columni (make-matrix '(2 3 4) '(3 4 6)) 1)
              (make-vector 2 3))

;; transpose:matrix -> matrix
;; given a matrix the procedure 'transpose' computes
;; the transpose of the matrix. 
;; given: (transpose '((2 3 4) (5 6 7)))
;; expect: '((2 5) (3 6) (4 7))
;;note, as of 9/18, this procedure is not working
(define (transpose matrix)
  (define (row->column vec)
    (cond ((null? vec) '())
	  (else (cons (list (car vector))
		    (row->column (cdr vector))))))
  (define (getfirst matrix) (map car matrix))
  (define (getcdr matrix) (map cdr matrix))
  (cond ((null? matrix) '())
	((equal? (length matrix) 1) (cons (list (car matrix))
				       (transpose (cdr matrix))))
	(else (cons (getfirst matrix)
                    (transpose (getcdr matrix))))))

(define primitive-procedures
  (list (list 'norm get-norm)
	(list 'linear-combination combine)
	(list 'transpose-matrices transpose)
	;(list 'multiply-matrices multiply-matrices)
	;(list 'multiply-vectors  multiply-vectors)
	;(list 'transpose-matrix-product transpose-matrix-product)
	;(list 'eigendecompose-matrix eigendecompose-matrix)
	(list 'car car)
	(list 'cdr cdr)
	(list 'cons cons)
	(list '* *)
	(list '+ +)
	(list '- -)))
(test)
