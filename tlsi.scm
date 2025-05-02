;; [ANDREW MOBUS]
;; [TLSI PROJECT]
;; [DRAFTING]
;; [12025-05-01]



;; Reformatted basic code segments
;; appears to be the entier thing & match up to the
;; explicit code sections from TLS chapter 10
;; but I need to double check that

;; I have a really distinctive style of scheme since
;; it kind of hurts to look at for me otherwise
;; which should help differentiate our project

;; We should try to add the *probe type but that
;; will require more real understanding of wtf
;; we're doing, so the first step is just to figure out
;; where we would put it in the code from a scaffolding
;; perspective so that it doesn't break anything

;; the first thing is just to double check whether these
;; code snippets are the exact same ones from the book
;; in which case we can be pretty transparent that
;; we just copied all the code sections verbatim
;; then starting changing things to help clarify the
;; code for ourselves (which is true!)

;; I'll probably have done that by the time you read this
;; but thought I'd get y'all something to look at and
;; start thinking of how we can test & verify that the
;; interpreter works & isn't like missing some big pieces

;; then the next major step is the syntax checker as said
;; I think we should go with integrating it more into the
;; main interpreter, but that really depends on a lot


;;;----------------------------------------------------------------------------|

;; YET_EMPTY_FUNCTIONS
;; unless these really do work as they are?
;; they might be basic aliases in order to
;; differentiate the TLSI from underlying R5RS

;; ; TODO:(1)
;; (define first car)
;;
;; ; TODO:(2)
;; (define second cadr)
;;
;; ; TODO:(3)
;; (define third caddr)
;;
;; ; TODO:(4)
;; (define new-entry build-pair)
;;
;; ; TODO:(5)
;; (define extend-table cons)
;;
;; ; TODO:(6)
;; (define cond-lines-of cdr)
;;
;; ; TODO:(7)
;; (define question-of first)
;;
;; ; TODO:(8)
;; (define answer-of second)
;;
;; ; TODO:(9)
;; (define function-of car)
;;
;; ; TODO:(10)
;; (define arguments-of cdr)
;;
;; ; TODO:(10)
;; (define table-of first)
;;
;; ; TODO:(11)
;; (define formals-of second)
;;
;; ; TODO:(12)
;; (define body-of third)


;; Associated Comments:
; (4) Build entry from a set of names and a list of values
; (5) Table(environment) is list of entries (?)
; (6) List of predicates and actions
; (11) get table of *lambda
; (12) get formal arguments of *lambda
; (13) get body of *lambda


;;;----------------------------------------------------------------------------|

(define atom?
(lambda (x)
(and
  (not (pair? x))
  (not (null? x))
) ; END_AND
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|


(define add1
(lambda (num)
  (+ num 1)
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|


(define sub1
(lambda (num)
  (- num 1)
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|


(define eq-atom?
(lambda (atom-one atom-two)
(cond
((and (number? atom-one) (number? atom-two))
(= atom-one atom-two))
((or (number? atom-one) (number? atom-two))
#f)
(else
(eq? atom-one atom-two)))
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|


(define eq-list?
(lambda (list-one list-two)
(cond
((and (null? list-one) (null? list-two)) #t)
((or (null? list-one) (null? list-two)) #f)
(else
(and (tls-equal? (car list-one) (car list-two))
(eq-list? (cdr list-one) (cdr list-two)))))
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|

;; s-expressions equal check

(define tls-equal?
(lambda (sexpr-one sexpr-two)
(cond
((and (atom? sexpr-one) (atom? sexpr-two)) (eq-atom? sexpr-one sexpr-two))
((or (atom? sexpr-one) (atom? sexpr-two)) #f)
(else (eq-list? sexpr-one sexpr-two))
) ; END_COND
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|

; TODO:(1)
(define first car)

;;;----------------------------------------------------------------------------|

; TODO:(2)
(define second cadr)

;;;----------------------------------------------------------------------------|

; TODO:(3)
(define third caddr)

;;;----------------------------------------------------------------------------|

;; Build pair
(define build-pair
(lambda (sexpr-one sexpr-two)
(cons sexpr-one
(cons sexpr-two '()))
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|

; TODO:(4)
(define new-entry build-pair)

;;;----------------------------------------------------------------------------|

;; look up name in entry
;; call entry-f if not found

(define lookup-in-entry
(lambda (name entry entry-f)
(lookup-in-entry-help name
(first entry)  ; names
(second entry) ; values
entry-f)
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|

;; refactor this

(define lookup-in-entry-help
(lambda (name names values entry-f)
(cond
(  (null? names) (entry-f name)  )
(  (eq? (car names) name) (car values)  )
(else
(  lookup-in-entry-help
   name
    (cdr names)
    (cdr values)
   entry-f)
) ; END_ELSE
) ; END_COND
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|

; TODO:(5)
(define extend-table cons)

;;;----------------------------------------------------------------------------|

;; look for name in table
;; call table-f if not found

(define lookup-in-table
(lambda (name table table-f)
(cond
(  (null? table) (table-f name)  )
              (else
(lookup-in-entry name
    (car table)    ; current entry (?)
    (lambda (name) ; if not found in current entry, look in next entry (?)
    (lookup-in-table name (cdr table) table-f))
) ; END_LOOKUP_IN_ENTRY
) ; END_ELSE
) ; END_COND
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|

;; budget eval

(define value
(lambda (ze-expr)
(  meaning ze-expr '() ) ;'() is empty table
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|

(define meaning
(lambda (ze-expr table)
( (expression-to-action ze-expr)
    ze-expr
    table
) ; UNSURE
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|

;; produce correct function for each possible S-expression

(define expression-to-action
(lambda (ze-expr)
(cond
(  (atom? ze-expr) (atom-to-action ze-expr)  )
(       else (list-to-action ze-expr)        )
) ; END_COND
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|

;; call *const function if ze-expr is a
;; number, boolean, or primitive operator
;; otherwise, it's a variable

(define atom-to-action
(lambda (ze-expr)
(cond

(  (number? ze-expr)      *const)
(  (eq? ze-expr #t)       *const)
(  (eq? ze-expr #f)       *const)
(  (eq? ze-expr 'cons)    *const)
(  (eq? ze-expr 'car)     *const)
(  (eq? ze-expr 'cdr)     *const)
(  (eq? ze-expr 'null?)   *const)
(  (eq? ze-expr 'eq?)     *const)
(  (eq? ze-expr 'atom?)   *const)
(  (eq? ze-expr 'zero?)   *const)
(  (eq? ze-expr 'add1)    *const)
(  (eq? ze-expr 'sub1)    *const)
(  (eq? ze-expr 'number?) *const)

      (else *identifier)

) ; END_COND
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|

;; check the car of ze-expr to see
;; if it is quote, lambda, or cond
;; else it is *application

(define list-to-action
(lambda (ze-expr)
(cond

(  (atom? (car ze-expr))

(cond

(  (eq? (car ze-expr) 'quote)  *quote )
(  (eq? (car ze-expr) 'lambda) *lambda)
(  (eq? (car ze-expr) 'cond)   *cond  )

        (else *application)

) ; END_COND_2
) ; UNSURE

          (else *application)
) ; END_COND_1
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|

(define *const
(lambda (ze-expr table)
(cond
  ( (number? ze-expr) ze-expr) ; return itself
  ( (eq? ze-expr #t) #t)       ; return itself
  ( (eq? ze-expr #f) #f)       ; return itself
(else
  (build-pair 'primitive ze-expr)) ; attach primitive tag on expression
) ; END_COND
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|

(define *quote
(lambda (ze-expr table)
(second ze-expr) ; Return value following quote
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|

(define *identifier ;; these are variables
(lambda (ze-expr table)
(lookup-in-table ; Look for symbol (in the table)
  ze-expr
  table
  initial-table
) ; END_LOOKUP
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|

;; Called when identifier is not found in environment
(define initial-table
  (lambda (name)
    (begin
      (display "Error: Unbound variable ")
      (display name)
      (newline)
      'unbound-variable)
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|

(define *lambda
(lambda (ze-expr table)
(  build-pair 'non-primitive ; for lambdas, attach non-primitive tag
(  cons table (cdr ze-expr)  ) ; add args and body to table
) ; END_BUILD
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|

(define *cond
(lambda (ze-expr table)
(evcon (cond-lines-of ze-expr) table)
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|

; TODO:(6)
(define cond-lines-of cdr)

;;;----------------------------------------------------------------------------|

(define evcon
(lambda (lines table)
(cond
  (
   (else? (question-of (car lines)))
   (meaning (answer-of (car lines)) table)
   )
  (
   (meaning (question-of (car lines)) table)
   (meaning (answer-of (car lines)) table)
   )
  (else (evcon (cdr lines) table) )
) ; END_COND
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|

(define else? ; checks whether predicate is else
(lambda (predicate)
(cond
  (  (atom? predicate) (eq? predicate 'else)  )
                  (else #f)
) ; END_COND
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|

; TODO:(7)
(define question-of first)

;;;----------------------------------------------------------------------------|

; TODO:(8)
(define answer-of second)

;;;----------------------------------------------------------------------------|

;; Action for application
;; Apply the meaning of function
;; to meaning of argument
;; to get new expression and environment

(define *application
(lambda (ze-expr table)
(tls-apply
(  meaning (function-of ze-expr) table  )
(  evlis (arguments-of ze-expr) table   )
) ; END_APPLY
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|

; TODO:(9)
(define function-of car)

; TODO:(10)
(define arguments-of cdr)

;;;----------------------------------------------------------------------------|

(define evlis
(lambda (args table)
(cond
(  (null? args) '()  )
(else
(cons
(  meaning (car args) table  )
(  evlis   (cdr args) table  )
) ; END_CONS
) ; END_ELSE
) ; END_COND
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|

(define tls-apply
(lambda (fun vals)
(cond
(
    (primitive? fun)
    (tls-apply-primitive (second fun) vals)
)
(
    (non-primitive? fun)
    (tls-apply-closure (second fun) vals)
)
) ; END_COND
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|

(define primitive?
(lambda (l)
(eq? (first l) 'primitive)
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|

(define non-primitive?
(lambda (l)
(eq? (first l) 'non-primitive)
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|

(define apply-primitive
(lambda (name vals)
(cond

;;;--------------------|

( (eq? name 'null?)
  (null? (first vals))
) ; END_CASE

( (eq? name 'atom?)
  (:atom? (first vals))
) ; END_CASE

;;;--------------------|

( (eq? name 'zero?)
  (zero? (first vals))
) ; END_CASE

( (eq? name 'number?)
  (number? (first vals))
) ; END_CASE

;;;--------------------|

( (eq? name 'car)
  (car (first vals))
) ; END_CASE

( (eq? name 'cdr)
  (cdr (first vals))
) ; END_CASE

;;;--------------------|

( (eq? name 'add1)
  (add1 (first vals))
) ; END_CASE

( (eq? name 'sub1)
  (sub1 (first vals))
) ; END_CASE

;;;--------------------|

( (eq? name 'cons)
  (cons (first vals) (second vals)  )
) ; END_CASE

( (eq? name 'eq?)
  (eq? (first vals) (second vals)  )
) ; END_CASE

;;;--------------------|

) ; END_COND
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|

(define :atom?
(lambda (x)
(cond
(  (atom? x) #t)
(  (null? x) #f)
(  (eq? (car x) 'primitive) #t)
(  (eq? (car x) 'non-primitive) #t)
            (else #f)
) ; END_COND
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|

(define apply-closure
(lambda (closure vals)
(meaning
  (body-of closure)
  (extend-table
  (new-entry (formals-of closure) vals)
  (table-of closure)
  ) ; END_EXTEND_TABLE
) ; END_MEANING
) ; END_LAMBDA
) ; END_DEFINE

;;;----------------------------------------------------------------------------|

; TODO:(11)
(define table-of first)

; TODO:(12)
(define formals-of second)

; TODO:(13)
(define body-of third)

;;;----------------------------------------------------------------------------|

;; [FIN]


;; TEST EXAMPLES

;; CONSTANTS

(value 42)           ; => 42
(value #t)           ; => #t
(value 'add1)        ; => (primitive add1)


;; QUOTED LIST



;; PRIMITIVE APPLICATION

(value (add1 4))     ; => 5
(value (sub1 4))     ; => 3
(value (zero? 0))    ; => #t

;; CONDITIONALS


;; LAMBDA(FUNCTION)

(value
  ((lambda (x) (add1 x)) 5))  ; => 6

;; NESTED FUNCTION

(value
  ((lambda (x)
     ((lambda (y) (add1 y)) x))
   10)) ; => 11

