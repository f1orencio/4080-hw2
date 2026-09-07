#lang racket

(define (make-number value)
  (let ((this value))
    (list
      (lambda ()
        this)
      (lambda ()
        (number->string this)))))

(define (make-binary left operator right)
  (let ((this-left left)
        (this-operator operator)
        (this-right right))
    (list
      (lambda ()
        (let ((l (evaluate this-left))
              (r (evaluate this-right)))
          (cond
            ((equal? this-operator "+") (+ l r))
            ((equal? this-operator "-") (- l r))
            ((equal? this-operator "*") (* l r))
            ((equal? this-operator "/") (/ l r))
            (else (error "Unknown operator")))))

      (lambda ()
        (string-append
          "("
          (print-expr this-left)
          " "
          this-operator
          " "
          (print-expr this-right)
          ")")))))

(define (evaluate expr)
  ((car expr)))

(define (print-expr expr)
  ((cadr expr)))

(define one
  (make-number 1))

(define two
  (make-number 2))

(define three
  (make-number 3))

(define multiplication
  (make-binary two "*" three))

(define expression
  (make-binary one "+" multiplication))

(display "Expression: ")
(display (print-expr expression))
(newline)

(display "Result: ")
(display (evaluate expression))
(newline)