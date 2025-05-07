;; ==================================================
;;  TLS Syntax Checker
;;  - Validates expressions against TLS Scheme fragment
;; ==================================================

;; <expression> ::= <atom> | <compound>

;; <atom> ::= <number> | <boolean> | <symbol>

;; <boolean> ::= #t | #f

;; <compound> ::= <quote> | <lambda> | <cond> | <application>

;; <quote> ::= (quote <expression>)

;; <lambda> ::= (lambda (<variables>) <expression>)
;; <variables> ::= ε | <symbol> <variables>

;; <cond> ::= (cond <clauses>)
;; <clauses> ::= <clause> | <clause> <clauses>
;; <clause> ::= (<test> <expression>) | (else <expression>)
;; <test> ::= <expression>

;; <application> ::= (<operator> <operands>)
;; <operator> ::= <expression>
;; <operands> ::= ε | <expression> <operands>

;; <primitive> ::= cons | car | cdr | null? | eq? | atom? | zero? | add1 | sub1 | number?

;; Helper function to check if an element is a member of a list
(define member?
  (lambda (x lst)
    (cond
      ((null? lst) #f)
      ((eq? x (car lst)) #t)
      (else (member? x (cdr lst))))))

;; List of valid primitives in TLS
(define primitive-functions
  '(cons car cdr null? eq? atom? zero? add1 sub1 number?))

;; Mapping of primitives to their expected number of arguments
(define primitive-arity
  '((cons . 2) (car . 1) (cdr . 1) (null? . 1) (eq? . 2) 
    (atom? . 1) (zero? . 1) (add1 . 1) (sub1 . 1) (number? . 1)))

;; Lookup the arity of a primitive function
(define lookup-primitive-arity
  (lambda (name)
    (let loop ((arities primitive-arity))
      (cond
        ((null? arities) 'not-found)
        ((eq? name (caar arities)) (cdar arities))
        (else (loop (cdr arities)))))))

;; Check if an expression is a valid atom
(define valid-atom?
  (lambda (expr env)
    (cond
      ((number? expr) #t)
      ((eq? expr #t) #t)
      ((eq? expr #f) #t)
      ((symbol? expr) 
       (or (member? expr primitive-functions)
           (member? expr env)
           (begin
             (display "Warning: Unbound variable: ")
             (display expr)
             (newline)
             #f)))
      (else #f))))

;; Check if a list of expressions contains only valid symbols
(define valid-formals?
  (lambda (formals)
    (cond
      ((null? formals) #t)
      ((not (list? formals)) #f)
      ((not (symbol? (car formals)))
       (begin
         (display "Error: Lambda formal must be a symbol: ")
         (display (car formals))
         (newline)
         #f))
      ((member? (car formals) (cdr formals))
       (begin
         (display "Error: Duplicate parameter in lambda formals: ")
         (display (car formals))
         (newline)
         #f))
      (else (valid-formals? (cdr formals))))))

;; Check if a cond clause is well-formed
(define valid-cond-clause?
  (lambda (clause env)
    (cond
      ((not (list? clause))
       (begin
         (display "Error: Cond clause must be a list: ")
         (display clause)
         (newline)
         #f))
      ((< (length clause) 2)
       (begin
         (display "Error: Cond clause needs test and expression: ")
         (display clause)
         (newline)
         #f))
      ((> (length clause) 2)
       (begin
         (display "Error: Cond clause has too many expressions: ")
         (display clause)
         (newline)
         #f))
      ((eq? (car clause) 'else) #t)
      (else (valid-syntax? (car clause) env)))))

;; Check if cond expression is well-formed
(define valid-cond?
  (lambda (expr env)
    (cond
      ((< (length expr) 2)
       (begin
         (display "Error: Cond expression needs at least one clause: ")
         (display expr)
         (newline)
         #f))
      (else
       (let ((clauses (cdr expr)))
         (let check-clauses ((remaining clauses)
                            (seen-else #f))
           (cond
             ((null? remaining) #t)
             ((and seen-else (not (null? remaining)))
              (begin
                (display "Error: Clause after else in cond: ")
                (display remaining)
                (newline)
                #f))
             ((eq? (caar remaining) 'else)
              (and (valid-cond-clause? (car remaining) env)
                   (check-clauses (cdr remaining) #t)))
             (else
              (and (valid-cond-clause? (car remaining) env)
                   (check-clauses (cdr remaining) seen-else))))))))))

;; Check if lambda expression is well-formed
(define valid-lambda?
  (lambda (expr env)
    (cond
      ((< (length expr) 3)
       (begin
         (display "Error: Lambda requires formals and body: ")
         (display expr)
         (newline)
         #f))
      ((> (length expr) 3)
       (begin
         (display "Error: Lambda has too many expressions: ")
         (display expr)
         (newline)
         #f))
      ((not (list? (cadr expr)))
       (begin
         (display "Error: Lambda formals must be a list: ")
         (display (cadr expr))
         (newline)
         #f))
      (else
       (and (valid-formals? (cadr expr))
            (valid-syntax? (caddr expr) 
                          (append (cadr expr) env)))))))

;; Check if quote expression is well-formed
(define valid-quote?
  (lambda (expr env)
    (cond
      ((not (= (length expr) 2))
       (begin
         (display "Error: Quote requires exactly one argument: ")
         (display expr)
         (newline)
         #f))
      (else #t))))

;; Check if application is well-formed
(define valid-application?
  (lambda (expr env)
    (cond
      ((null? expr)
       (begin
         (display "Error: Empty application: ")
         (display expr)
         (newline)
         #f))
      (else
       (let ((operator (car expr))
             (operands (cdr expr)))
         (and (valid-syntax? operator env)
              (every? (lambda (e) (valid-syntax? e env)) operands)
              (cond
                ((and (symbol? operator) 
                      (member? operator primitive-functions))
                 (let ((expected-args (lookup-primitive-arity operator))
                       (actual-args (length operands)))
                   (if (= expected-args actual-args)
                       #t
                       (begin
                         (display "Error: Primitive ")
                         (display operator)
                         (display " expects ")
                         (display expected-args)
                         (display " arguments, got ")
                         (display actual-args)
                         (newline)
                         #f))))
                (else #t))))))))

;; Check if every element of a list satisfies the predicate
(define every?
  (lambda (pred lst)
    (cond
      ((null? lst) #t)
      ((pred (car lst)) (every? pred (cdr lst)))
      (else #f))))

;; Main function to check if an expression is well-formed
(define valid-syntax?
  (lambda (expr env)
    (cond
      ((not (pair? expr)) (valid-atom? expr env))
      ((null? expr) #t)
      ((eq? (car expr) 'quote) (valid-quote? expr env))
      ((eq? (car expr) 'lambda) (valid-lambda? expr env))
      ((eq? (car expr) 'cond) (valid-cond? expr env))
      (else (valid-application? expr env)))))

;; Check if a program has valid TLS syntax
(define check-syntax
  (lambda (expr)
    (valid-syntax? expr '())))

;; ========================================================
;; Test cases
;; ========================================================

;; Test 1: Valid expressions
(display "Test 1: Valid number: ")
(display (check-syntax 42))
(newline)

;; Test 2: Valid application of primitive
(display "Test 2: Valid primitive application: ")
(display (check-syntax '(add1 5)))
(newline)

;; Test 3: Valid lambda expression
(display "Test 3: Valid lambda: ")
(display (check-syntax '(lambda (x) (add1 x))))
(newline)

;; Test 4: Valid cond expression
(display "Test 4: Valid cond: ")
(display (check-syntax '(cond ((zero? 0) 'zero) (else 'not-zero))))
(newline)

;; Test 5: Invalid - wrong number of arguments to primitive
(display "Test 5: Invalid primitive arity: ")
(display (check-syntax '(add1 1 2)))
(newline)

;; Test 6: Invalid - malformed lambda
(display "Test 6: Invalid lambda formals: ")
(display (check-syntax '(lambda 5 (add1 x))))
(newline)

;; Test 7: Invalid - duplicate lambda parameters
(display "Test 7: Duplicate lambda parameters: ")
(display (check-syntax '(lambda (x x) (add1 x))))
(newline)

;; Test 8: Invalid - malformed cond
(display "Test 8: Malformed cond: ")
(display (check-syntax '(cond)))
(newline)

;; Test 9: Invalid - else not last in cond
(display "Test 9: Else not last in cond: ")
(display (check-syntax '(cond (else 1) ((zero? 0) 2))))
(newline)

;; Test 10: Invalid - unbound variable
(display "Test 10: Unbound variable: ")
(display (check-syntax '(lambda (x) (add1 y))))
(newline)

;; Test 11: Valid complex expression
(display "Test 11: Complex valid expression: ")
(display (check-syntax '((lambda (x) 
                          ((lambda (y) (add1 y)) 
                           (add1 x))) 
                        5)))
(newline)

;; Test 12: Invalid quote
(display "Test 12: Invalid quote: ")
(display (check-syntax '(quote)))
(newline)