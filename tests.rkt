#lang racket
(require test-engine/racket-tests)
(require src)

(check-expect (first-element (make-vector 2 3 4 5 6)) 2)

(check-expect (rest-elements (make-vector 2 3 4)) (make-vector 3 4))

(check-expect (vec? (make-vector 2 3 4)) #t)

(check-expect (list-of-numbers? (make-vector 2 3 4)) #t)
(check-expect (list-of-numbers? (make-vector 2 'a 3)) #f)

(check-expect (get-element (make-matrix '(2 3 4) '(5 6 7)) 2 1)
              6)

(check-expect (getrow (make-matrix '(2 3 4) '(3 4 5)) 2 2)
              '(3 4 5))

(check-expect (get-norm (make-vector 1 2 3) 1) 6)

(check-expect (add-vectors (make-vector 2 3) (make-vector 3 4))
              (make-vector 5 7))

(check-expect (combine (make-matrix '(2 3) '(4 5))
                       (make-vector 3 4) 0)
                (make-vector 18 32))

(check-expect (term (make-matrix '(2 3 5) '(3 4 5))
                    (make-vector 3 4) 0)
              (make-vector 6 9))

(check-expect (multiply 3 (make-vector 3 4 5)) (make-vector 9 12 15))

(check-expect (get-columni (make-matrix '(2 3 4) '(3 4 6)) 1)
              (make-vector 2 3))
	      
(check-expect (transpose (make-matrix '(2 3 4) '(5 6 7)))
              (make-matrix '(2 5) '(3 6) '(4 7)))

(check-expect (multiply-matrices (make-matrix '(1 2 3) '(4 5 6))
                                 (make-matrix '(7 8) '(9 10) '(11 12))
                                 1)
              (make-matrix '(58 64) '(139 154)))


(check-expect (add-col (make-vector 2 3 5)) 10)

(check-expect (row-times-column (make-matrix (make-vector 1 2 3) (make-vector 4 5 6))
                                (make-matrix (make-vector 7 8) (make-vector 9 10) (make-vector 11 12))
                                1)
              (make-matrix (make-vector 7 18 33) (make-vector 8 20 36)))


