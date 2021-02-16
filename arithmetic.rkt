#lang sicp


;;; Table ADT
(define tbl '())

(define (key alist) (car alist))
(define (value alist) (cadr alist))

;; put (key1 key2 value)
; binding value with list(key1 key2)
(define (put op type item)
  (define (put-helper plist tbl)
    (cond ((null? tbl) (list (list plist item)))
          ((equal? (key (car tbl)) plist) tbl)
          (else (cons (car tbl) (put-helper plist (cdr tbl))))))
  (set! tbl (put-helper (list op type) tbl)))

;; get value
; binding with list(key1 key2)
(define (get op type)
  (define (get-helper plist tbl)
    (cond ((null? tbl) #f)
          ((equal? (key (car tbl)) plist) (value (car tbl)))
          (else (get-helper plist (cdr tbl)))))
  (get-helper (list op type) tbl))


;; tag
(define (attach-type type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (car datum))

(define (contents datum)
  (cdr datum))

;;; install packages
;;ordinary number
(define (install-scheme-number-package)
  (define (type x) (attach-type 'scheme-number x))
  
  ;put operator
  (put 'ADD '(scheme-number scheme-number)
       (lambda (x y) (type (+ x y))))
  (put 'MUL '(scheme-number scheme-number)
       (lambda (x y) (type (* x y))))
  (put 'make 'scheme-number (lambda (x) (type x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;;rational number
(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  
  ; ADD
  (define (ADD-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  
  ; MUL
  (define (MUL-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (type x) (attach-type 'rational x))
  
  ; put operator
  (put 'ADD '(rational rational)
       (lambda (x y) (type (ADD-rat x y))))
  (put 'MUL '(rational rational)
       (lambda (x y) (type (MUL-rat x y))))
  (put 'make 'rational
       (lambda (n d) (type (make-rat n d))))
  'done)

; define make-rat
(define (make-rat n d)
  ((get 'make 'rational) n d))

(define (square x) (* x x))

;; complex1 - rectangular
(define (install-rectangular-package)

  ; Bert's data structure
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-rectangular x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-polar r a)
    (cons (* r (cos a)) (* r (sin a))))

  ; put operator
  (define (type x) (attach-type 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-rectangular 'rectangular 
       (lambda (x y) (type (make-rectangular x y))))
  (put 'make-polar 'rectangular
       (lambda (r a) (type (make-polar r a))))
  'done)

;; complex2 - polar
(define (install-polar-package)

  ; Ernie's data structure
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-polar r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-rectangular x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ; put operator
  (define (type x) (attach-type 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-rectangular 'polar
       (lambda (x y) (type (make-rectangular x y))))
  (put 'make-polar 'polar
       (lambda (r a) (type (make-polar r a))))
  'done)


;; complex number
(define (install-complex-package)
  
  ; procedure from rectangular/polar package
  (define (make-rectangular x y)
    ((get 'make-rectangular 'rectangular) x y))
  (define (make-polar r a)
    ((get 'make-polar 'polar) r a))
  
  ; ADD
  (define (ADD-complex z1 z2)
    (make-rectangular (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))

  ; MUL
  (define (MUL-complex z1 z2)
    (make-polar (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  
  ; put operator
  (define (type z) (attach-type 'complex z))
  (put 'ADD '(complex complex)
       (lambda (z1 z2) (type (ADD-complex z1 z2))))
  (put 'MUL '(complex complex)
       (lambda (z1 z2) (type (MUL-complex z1 z2))))
  (put 'make-rectangular 'complex
       (lambda (x y) (type (make-rectangular x y))))
  (put 'make-polar 'complex
       (lambda (r a) (type (make-polar r a))))
  'done)


;; define make-complex
(define (make-complex-rectangular x y)
  ((get 'make-rectangular 'complex) x y))
(define (make-complex-polar r a)
  ((get 'make-polar 'complex) r a))


;; install packages
(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)


;;; Generic arithmetic procedure
; apply-generic
(define (ADD x y) (apply-generic 'ADD x y))
(define (MUL x y) (apply-generic 'MUL x y))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;; type change
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get type1 type2))
                      (t2->t1 (get type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t1->t2 a2)))
                        (else (error "No method for these types"
                                     (list op type-tags))))))
              (error "No method for these types" (list op type-tags)))))))


;;; Test code
( define c1 ( make-complex-rectangular 3 5))
( define c2 ( make-complex-rectangular 1 2))
( define c3 ( make-complex-polar 3 5))
( define c4 ( make-complex-polar 1 2))
( define r1 ( make-rat 3 5))
( define r2 ( make-rat 5 6))
( define n1 ( make-scheme-number 5))
( define n2 ( make-scheme-number 8))

( ADD c1 c2)
( ADD c1 c4)
( MUL c1 c4)
( ADD r1 r2)
( MUL r1 r2)
( ADD n1 n2)
( MUL n1 n2)

