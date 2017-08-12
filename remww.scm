(define is-reg-overwritten-before-being-read?
    (lambda (reg list-of-inst)
        (cond ((null? list-of-inst) #f)
              ((not (equal? #f (member reg (cadar list-of-inst)))) #f)
              ((not (equal? #f (member reg (caddar list-of-inst)))) #t)
              (else (is-reg-overwritten-before-being-read? reg (cdr list-of-inst))))))

(define is-inst-redundant?
    (lambda (inst list-of-inst)
        (if (equal? #t (andmap (lambda (reg) (is-reg-overwritten-before-being-read? reg list-of-inst)) (caddr inst))) #t #f)))
        
 (define remww-loop
     (lambda (list-of-inst)
        (cond ((null? list-of-inst) '())
              ((equal? #t (is-inst-redundant? (car list-of-inst) (cdr list-of-inst))) (remww-loop (cdr list-of-inst)))
              ((equal? #f (is-inst-redundant? (car list-of-inst) (cdr list-of-inst))) (cons (car list-of-inst) (remww-loop (cdr list-of-inst))))
         )))
(define remww
    (lambda (list-of-inst)
        (let* ( (old (remww-loop list-of-inst))
               (new (remww-loop old)) )
        (if (equal? old new) new
            (begin (set! old new) (set! new (remww-loop new)) (remww new))))))