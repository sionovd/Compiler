(define map
        (lambda (func lst)
                (if (null? lst) '() (cons (func (car lst)) (map func (cdr lst))))))
                
(define list (lambda elements elements))

(define not (lambda (x) (if x #f #t)))

(define binary-append 
            (lambda (lst1 lst2)
  (if (null? lst1) lst2
         (cons (car lst1)
               (binary-append (cdr lst1) lst2)))))
               
(define append
    (lambda lists 
        (cond ((null? lists) '())
              ((null? (cdr lists)) (car lists))
              ((null? (cdr (cdr lists))) (binary-append (car lists) (car (cdr lists))))
              (else (append-helper (binary-append (car lists) (car (cdr lists))) (cdr (cdr lists))))
                )))
                
(define append-helper
    (lambda lists 
        (cond ((null? (car (cdr lists))) (car lists))
              (else (append-helper (binary-append (car lists) (car (car (cdr lists)))) (cdr (car (cdr lists)))))
                )))

(define cadr
    (lambda (lst)
        (car (cdr lst))))
        
(define cdar
    (lambda (lst)
        (cdr (car lst))))
        
(define caar
    (lambda (lst)
        (car (car lst))))
            
(define <-helper
    (lambda args
        (cond ((null? (cdar args)) #t) 
        ((not (or (> (caar args) (car (cdar args))) (= (caar args) (car (cdar args)))))  (<-helper (cdar args)))
        (else #f))))

(define <
    (lambda args
        (cond ((null? (cdr args)) #t) 
        ((not (or (> (car args) (cadr args)) (= (car args) (cadr args))))  (<-helper (cdr args)))
        (else #f))))
           
            
