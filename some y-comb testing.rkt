(define Y-comb
  (lambda (f)
    
    (
     (lambda (x)
       (f (lambda (v) ((x x) v)))
       )

     (lambda (x)
       (f (lambda (v) ((x x) v)))
       )
     )
    ); END_LAMBDA
  );END_DEFINE

(define fact
  (
   (lambda (f)
    
    (
     (lambda (x)
       (f (lambda (v) ((x x) v)))
       )

     (lambda (x)
       (f (lambda (v) ((x x) v)))
       )
     )
    )
   (lambda (recur)
     (lambda (n)
       (if (= n 0)
           1
           (* n (recur (- n 1)))
           )
       )
     )
   )
  )

(value '(((
  (lambda (f)
    (
     (lambda (x)
       (f (lambda (v) ((x x) v))))
     (lambda (x)
       (f (lambda (v) ((x x) v))))
    ))
  (lambda (recur)
    (lambda (a)
      (lambda (b)
        (cond
          ((zero? a) b)
          (else
           (add1 ((recur (sub1 a)) b)))
          )
        )
      )
    )
  )
 3) 4))
