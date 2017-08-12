(load "pc.scm")


(define convert-string
  (lambda (parser string)
    (parser (string->list string)
	    (lambda (e s) (append (list e) (convert-string parser (list->string s))))
	    (lambda (w) (list)))))
	    
(define <sexpr-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <InfixExpression>))
       (*delayed (lambda () <sexpr>))
       (*disj 2)
       (*caten 2)
       done))

(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))

(define <line-comment>
  (let ((<end-of-line-comment>
         (new (*parser (char #\newline))
              (*parser <end-of-input>)
              (*disj 2)
              done)))
    (new (*parser (char #\;))
         
         (*parser <any-char>)
         (*parser <end-of-line-comment>)
         *diff *star
         
         (*parser <end-of-line-comment>)
         (*caten 3)	 
         done)))

(define <comment>
  (disj <line-comment>
        <sexpr-comment>))

(define <skip>
  (disj <comment>
        <whitespace>))

(define ^^<wrapped>
  (lambda (<wrapper>)
    (lambda (<p>)
      (new (*parser <wrapper>)
           (*parser <p>)
           (*parser <wrapper>)
           (*caten 3)
           (*pack-with
            (lambda (_left e _right) e))
           done))))

(define ^<skipped*> (^^<wrapped> (star <skip>)))



(define <sexpr>
  (^<skipped*>
   (new 
    (*delayed (lambda () <Boolean>))
    (*delayed (lambda () <Char>))
    (*delayed (lambda () <Number>))
    (*delayed (lambda () <Symbol>))
    (*delayed (lambda () <String>))
    (*delayed (lambda () <ProperList>))
    (*delayed (lambda () <ImproperList>))
    (*delayed (lambda () <Vector>))
    (*delayed (lambda () <Quoted>))
    (*delayed (lambda () <QuasiQuoted>))
    (*delayed (lambda () <Unquoted>))
    (*delayed (lambda () <UnquoteAndSpliced>))
    (*delayed (lambda () <InfixExtension>))
    (*disj 13)
    done)))


(define <Boolean>
  (new (*parser (word-ci "#t"))
       (*pack
        (lambda (_) #t))
       
       (*parser (word-ci "#f"))
       (*pack
        (lambda (_) #f))
       
       (*disj 2)
       done))

(define <CharPrefix>
  (new (*parser (word "#\\"))
       
       (*pack
        (lambda (_)  '#\\)
        ) 
       done
       ))



(define ^<ListedChar> (lambda (str rep)
                        (new (*parser (word str))
                             (*pack (lambda (_) rep))
                             done)))

(define <NamedChar> 
  (new (*parser (^<ListedChar> "lambda" (integer->char 955)))
       (*parser (^<ListedChar> "newline" #\newline))
       (*parser (^<ListedChar> "nul" #\nul))
       (*parser (^<ListedChar> "page" #\page))
       (*parser (^<ListedChar> "return" #\return))
       (*parser (^<ListedChar> "space" #\space))
       (*parser (^<ListedChar> "tab" #\tab))
       (*disj 7)
       done))


(define <HexChar>
  (new (*parser (range-ci #\0 #\F))
       (*parser (range #\: #\@))
       *diff
       (*parser (range #\[ #\`))
       *diff
       done))

(define <VisibleSimpleChar>
  (new (*parser (range #\! (integer->char 127)))
       (*parser <HexChar>)
       *not-followed-by
       done))


(define <HexUnicodeChar>
  (^<skipped*> (new (*parser (char #\x))
                    (*parser <HexChar>) *plus
                    (*guard (lambda (hexChars) (< (string->number(list->string hexChars) 16)  1114111)))
                    (*caten 2)
                    (*pack-with (lambda (xx hexChars) (integer->char (string->number(list->string hexChars) 16))))
                    done)))


(define <Natural>
  (new (*parser (range #\0 #\9)) *plus
       (*pack
        (lambda (num)
          (string->number
           (list->string
            `(,@num)))))
       done))

(define <StringLiteralChar>
  (new (*parser <any-char>)
       (*parser (char #\\))
       *diff
       done))

(define <StringMetaChar>
  (new (*parser (^<ListedChar>  "\\\\" #\\))
       (*parser (^<ListedChar>  "\\\"" #\"))
       (*parser (^<ListedChar>  "\\n" #\newline))
       (*parser (^<ListedChar>  "\\r" #\return))
       (*parser (^<ListedChar>  "\\t" #\tab))
       (*parser (^<ListedChar>  "\\f" #\page))
       (*disj 6)
       done))


(define <SymbolChar>
  (new (*parser (range #\0 #\9))
       (*parser (range #\a #\z))
       (*parser (range #\A #\Z))
       (*pack char-downcase)
       (*parser (char #\!))
       (*parser (char #\$))
       (*parser (char #\^))
       (*parser (char #\*))
       (*parser (char #\-))
       (*parser (char #\_))
       (*parser (char #\=))
       (*parser (char #\+))
       (*parser (char #\<))
       (*parser (char #\>))
       (*parser (char #\?))
       (*parser (char #\/))
       (*disj 15)
       done))

(define <InfixChar>
  (new (*parser (range #\0 #\9))
       (*parser (range #\a #\z))
       (*parser (range #\A #\Z))
       (*pack char-downcase)
       (*parser (char #\!))
       (*parser (char #\$))
       (*parser (char #\_))
       (*parser (char #\=))
       (*parser (char #\<))
       (*parser (char #\>))
       (*parser (char #\?))
       (*disj 10)
       done))

(define <Symbol>
  (^<skipped*> (new (*parser <SymbolChar>) *plus 
                    (*pack
                     (lambda (symChars) (string->symbol (list->string `(,@symChars)))))
                    done)))


(define <Char>
  (new (*parser <CharPrefix>)
       (*parser <NamedChar>)
       (*parser <HexUnicodeChar>)
       (*parser <VisibleSimpleChar>)
       (*disj 3)
       (*caten 2)
       (*pack-with (lambda (prefix ch) ch))
       done))

(define <Integer>
  (new (*parser (char #\+))
       (*parser <Natural>)
       (*caten 2)
       (*pack-with
        (lambda (++ n) n))
       
       (*parser (char #\-))
       (*parser <Natural>)
       (*caten 2)
       (*pack-with
        (lambda (-- n) (- n)))
       
       (*parser <Natural>)
       
       (*disj 3)
       
       done))

(define <Fraction>
  (new (*parser <Integer>)
       (*parser (char #\/))
       (*parser <Natural>)
       (*caten 3)
       (*pack-with
        (lambda (num div den)
          (/ num den)))
       done))

(define <Number>
  (new (*parser <Fraction>)
       (*parser <Integer>)
       (*disj 2)
       (*parser (range #\a #\z))
       (*parser (range #\A #\Z))
       (*parser (char #\!))
       (*parser (char #\$))
       (*parser (char #\^))
       (*parser (char #\*))
       (*parser (char #\-))
       (*parser (char #\_))
       (*parser (char #\=))
       (*parser (char #\+))
       (*parser (char #\<))
       (*parser (char #\>))
       (*parser (char #\?))
       (*parser (char #\/))
       (*disj 14)
       *not-followed-by
       done))

(define <InfixNumber>
  (new (*parser <Fraction>)
       (*parser <Integer>)
       (*disj 2)
       (*parser (range #\a #\z))
       (*parser (range #\A #\Z))
       (*disj 2)
       *not-followed-by
       done))

(define <StringHexChar> 
  (new (*parser (char #\\))
       (*parser (char #\x))
       (*caten 2)
       (*pack-with (lambda (bs xx) `(,bs ,xx)))
       (*parser <HexChar>) *star
       (*guard (lambda (hexChars) (< (string->number(list->string hexChars) 16)  1114111)))
       (*parser (char #\;))
       (*caten 3)
       (*pack-with (lambda (pre hexChars semicolon) (integer->char (string->number(list->string hexChars) 16))))
       done))

(define <StringChar> 
  (new (*parser <StringLiteralChar>)
       (*parser <StringMetaChar>)
       (*parser <StringHexChar>)
       (*disj 3)
       done))



(define <String> 
  (new (*parser (char #\"))
       (*parser <StringChar>) 
       (*parser (char #\"))
       *diff
       *star
       (*parser (char #\"))
       (*caten 3)
       (*pack-with (lambda (dq1 chars dq2) (if (and (not (null? chars )) (string? (car chars))) chars (list->string chars))))
       done ))

(define <ProperList>
  (new (*parser (char #\())
       (*parser <sexpr>) *star
       (*parser (char #\)))
       (*caten 3)
       (*pack-with (lambda (pL s-ex pR) s-ex)) 
       done))

(define <ImproperList>
  (new (*parser (char #\())
       (*parser <sexpr>) *plus
       (*parser (char #\.))
       (*parser <sexpr>)
       (*parser (char #\)))
       (*caten 5)
       (*pack-with (lambda (pl s-ex* dot s-ex pr) `(,@s-ex* . ,s-ex)))
       done))

(define <Vector>
  (new (*parser (char #\#))
       (*parser (char #\())
       (*parser <sexpr>) *star
       (*parser (char #\)))
       (*caten 4)
       (*pack-with (lambda (h-tag pl s-ex* pr) (list->vector s-ex*)))
       done))

(define <Quoted>
  (new (*parser (char #\'))
       (*parser <sexpr>)
       (*caten 2)
       (*pack-with (lambda (geresh s-ex) `',s-ex))
       done))

(define <QuasiQuoted>
  (new (*parser (char #\`))
       (*parser <sexpr>)
       (*caten 2)
       (*pack-with (lambda (geresh s-ex) (list 'quasiquote s-ex)))
       done))

(define <Unquoted>
  (new (*parser (char #\,))
       (*parser <sexpr>)
       (*caten 2)
       (*pack-with (lambda (psik s-ex) (list 'unquote s-ex)))
       done))

(define <UnquoteAndSpliced>
  (new (*parser (char #\,))
       (*parser (char #\@)) 
       (*parser <sexpr>)
       (*caten 3)
       (*pack-with (lambda (psik strudel s-ex) (list 'unquote-splicing s-ex)))
       done))

(define <InfixPrefixExtensionPrefix>
  (new (*parser (word "##"))
       (*pack (lambda (_) '()))
       (*parser (char #\#))
       (*parser (char #\%))
       (*caten 2)
       (*pack-with (lambda (hash modulo) '#%))
       (*disj 2)
       done))


(define helper1 (lambda (curr next)
                  (list (car next) curr (cdr next))))

(define helper2 (lambda (next curr)
                  (list 'expt next curr)))

(define helper3 (lambda (vecName ie) (if (and (list? ie) (not (null? ie)) (eq? (car ie) (integer->char 91))) 
                                         (list 'vector-ref vecName (cadr ie)) 
                                         (append (list vecName) ie))))

(define <InfixNeg>
  (new (*parser (char #\-))
       (*pack (lambda (_) '-))
       done))

(define <InfixSexprEscape>
  (new (*parser <InfixPrefixExtensionPrefix>)
       (*parser <sexpr>)
       (*caten 2)
       (*pack-with (lambda (prefix s-ex) s-ex))
       done))

(define <InfixExpression>
  (^<skipped*> (new
                (*delayed (lambda () <MultiDivLayer>))
                (*parser (char #\+))
                (*pack (lambda (_) '+))
                (*parser (char #\-))
                (*pack (lambda (_) '-))
                (*disj 2)
                (*delayed (lambda () <MultiDivLayer>))
                (*caten 2)
                (*pack-with (lambda (operator exp) (cons operator exp)))
                *star
                (*caten 2)
                (*pack-with (lambda (exp1 lst) (fold-left helper1  exp1 lst)))
                done)))

(define <InfixExtension>
  (new (*parser <InfixPrefixExtensionPrefix>)
       (*parser <InfixExpression>)
       (*caten 2)
       (*pack-with (lambda (infixPrefix exp) exp))
       done))

(define <PowerSymbol>
  (new (*parser (char #\^))
       (*parser (word "**"))
       (*disj 2)
       (*pack (lambda (_) 'expt))
       done))

(define <InfixSymbol>
  (new (*parser <InfixChar>) *plus 
       (*pack
        (lambda (infixChars) (string->symbol (list->string `(,@infixChars)))))
       done))



(define <AtomicLayer> 
  (^<skipped*> (new 
                
                
                
                (*parser <InfixNumber>)
                
                (*parser <InfixNeg>)
                
                (*parser <whitespace>)
                *star
                (*parser <InfixNumber>)
                (*parser <whitespace>)
                *star
                (*caten 3)
                (*pack-with (lambda (spaceL num spaceR) num))
                (*caten 2)
                (*pack-with (lambda (neg num) `(- ,num)))
                (*disj 2)
                
                (*parser <InfixNeg>)
                *maybe
                (*parser <whitespace>)
                *star
                (*parser <InfixSymbol>)
                (*parser <whitespace>)
                *star
                (*caten 3)
                (*pack-with (lambda (spacesL symbol spacesR) symbol))
                
                
                (*parser <whitespace>)
                *star
                (*parser (char #\())
                (*parser <InfixExpression>)
                (*parser (char #\)))
                (*caten 3)
                (*pack-with (lambda (pl ie pr) `,ie))
                (*parser <whitespace>)
                *star
                (*caten 3)
                (*pack-with (lambda (spacesL exp spacesR) exp))
                
                
                (*parser <whitespace>)
                *star
                (*parser <InfixSexprEscape>)
                (*parser <whitespace>)
                *star
                (*caten 3)
                (*pack-with (lambda (spacesL escape-sexpr spacesR) escape-sexpr))
                
                (*disj 3)
                (*caten 2)
                (*pack-with (lambda (neg exp) (if (eq? (car neg) #f) exp `(- ,exp))))
                (*disj 2)
                done)))

(define <VectorFuncLayer> 
  (^<skipped*> (new 
                (*parser <AtomicLayer>)
                
                (*parser (char #\())
                (*parser <InfixExpression>)
                (*parser (char #\,))
                (*parser <InfixExpression>)
                (*caten 2)
                (*pack-with (lambda (comma ie) ie))
                *star
                (*caten 2)
                (*pack-with (lambda (first-op rest-op) (append (list first-op) rest-op)))
                (*parser <whitespace>)
                *star
                (*parser (word ""))
                (*parser <whitespace>)
                *star
                (*caten 3)
                (*pack-with (lambda (spacesL w spacesR) '()))
                (*disj 2)
                (*parser (char #\)))
                (*caten 3)
                (*pack-with (lambda (pl arg-list pr) arg-list))
                
                (*parser (char #\[))
                (*parser <InfixExpression>)
                (*parser (char #\]))
                (*caten 3)
                (*pack-with (lambda (pl ie pr) (list pl ie pr)))
                
                (*disj 2)
                *star
                (*caten 2)
                (*pack-with (lambda (l4 ie*) (if (and (list? l4) (eq? (car l4) '-)) (append (list '-) (if (null? ie*) (cdr l4) (list (fold-left helper3 (cadr l4) ie*)))) (if (null? ie*) l4 (fold-left helper3 l4 ie*)))))
                
                (*parser <InfixSexprEscape>)
                (*disj 2)
                done)))

(define <PowerLayer> 
  (^<skipped*> (new (*parser <VectorFuncLayer>)
                    (*parser <PowerSymbol>)
                    (*caten 2)
                    (*pack-with (lambda (l3 ps) l3))
                    *star
                    (*parser <VectorFuncLayer>)
                    (*caten 2)
                    (*pack-with (lambda (l3* l3-final) (fold-right helper2 l3-final l3*)))
                    (*parser <InfixSexprEscape>)
                    (*disj 2)
                    done)))

(define <MultiDivLayer> 
  (^<skipped*> (new
                (*parser <InfixSexprEscape>)
                (*parser <PowerLayer>)
                (*parser (char #\*))
                (*pack (lambda (_) '*))
                (*parser (char #\/))
                (*pack (lambda (_) '/))
                (*disj 2)
                (*parser <PowerLayer>)
                (*caten 2)
                (*pack-with (lambda (operator exp) (cons operator exp)))
                *star
                (*caten 2)
                (*pack-with (lambda (exp1 lst) (fold-left helper1 exp1 lst)))
                (*disj 2)
                done)))


; -------------------------------------------------------------------------------------------------------------;
; -------------------------------------- end of assignment 1 code ---------------------------------------------;
; -------------------------------------------------------------------------------------------------------------;



(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
           (eq? (car e) tag)
           (pair? (cdr e))
           (null? (cddr e))))))

(define quote? (^quote? 'quote))
(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

(define const?
  (let ((simple-sexprs-predicates
         (list boolean? char? number? string?)))
    (lambda (e)
      (or (ormap (lambda (p?) (p? e))
                 simple-sexprs-predicates)
          (quote? e)))))

(define quotify
  (lambda (e)
    (if (or (null? e)
            (pair? e)
            (symbol? e)
            (vector? e))
        `',e
        e)))

(define unquotify
  (lambda (e)
    (if (quote? e)
        (cadr e)
        e)))

(define const-pair?
  (lambda (e)
    (and (quote? e)
         (pair? (cadr e)))))

(define ?
  (lambda (name . guards)
    (let ((guard?
           (lambda (e)
             (andmap 
              (lambda (g?) (g? e))
              guards))))
      (lambda (value)
        (if (guard? value)
            (list value)
            #f)))))


; -------------------------------------------------------------------------------------------------------------;
; -------------------------------------- end of quasiquote code -----------------------------------------------;
; -------------------------------------------------------------------------------------------------------------;


(define *void-object* (if #f #f))

(define *reserved-words*
  '(and begin cond define do else if lambda
        let let* letrec or quasiquote unquote
        unquote-splicing quote set!))

(define reserved-word?
  (lambda (e)
    (ormap
     (lambda (kw) (eq? e kw))
     *reserved-words*)))

(define var?
  (lambda (e)
    (and (symbol? e) (not (reserved-word? e)))))

(define not-reserved?
  (lambda (e)
    (not (reserved-word? e))))

(define not-reserved?-list (lambda (lst) (andmap not-reserved? lst)))

(define simple-const?
  (let ((preds (list null? vector? boolean? char? number? string?)))
    (lambda (e) (ormap (lambda (p?) (p? e)) preds))))

(define not-null? (lambda(x) (not (null? x))))

(define let-element? (lambda (x) (and (list? x) (eq? (length x) 2) (var? (car x)) (not-reserved? (cadr x)))))

(define let-bindings? (lambda (bindings) (or (null? bindings) (andmap let-element? bindings))))

(define cond-element? (lambda (pair)  (and (list? pair) (eq? (length pair) 2) (not (eq? 'else (car pair))))))

(define cond-pairs? (lambda (lst)  (andmap cond-element? lst)))

(define qq? (lambda (lst)  (and (list? lst) (eq? (car lst) 'quasiquote))))

(define no-dups? 
  (lambda (x)
    (let ((func (lambda (var) 
                  (length (filter (lambda (y) (eq? y var)) x)))))
      (if (list? x) (eq? (length x) (fold-right + 0 (map func x))) #t))))

(define find-dups
  (lambda (x)
    (letrec ((dup-list (list))
             (func (lambda (var) 
                     (if (> (length (filter (lambda (y) (equal? y var)) x)) 1) (if (not(member var dup-list)) (set! dup-list (append dup-list (list var))))))))
      (if (list? x) (begin (map func x) dup-list) (list)))))


(define match
  (letrec ((match
               (lambda (pat e ret-vals ret-fail)
                 (cond ((and (pair? pat) (pair? e))
                        (match (car pat) (car e)
                          (lambda (vals-car)
                            (match (cdr pat) (cdr e)
                              (lambda (vals-cdr)
                                (ret-vals
                                 (append vals-car vals-cdr)))
                              ret-fail))
                          ret-fail))
                       ((and (vector? pat) (vector? e)
                             (= (vector-length pat) (vector-length e))
                             (match (vector->list pat) (vector->list e)
                               ret-vals ret-fail)))
                       ((procedure? pat)
                        (let ((v (pat e)))
                          (if v (ret-vals v) (ret-fail))))
                       ((equal? pat e) (ret-vals '()))
                       (else (ret-fail))))))
    (lambda (pat e ret-with ret-fail)
      (match pat e
        (lambda (vals) (apply ret-with vals))
        ret-fail))))

;;; composing patterns

(define pattern-rule
  (lambda (pat handler)
    (lambda (e failure)
      (match pat e handler failure))))

(define compose-patterns
  (letrec ((match-nothing
            (lambda (e failure)
              (failure)))
           (loop
            (lambda (s)
              (if (null? s)
                  match-nothing
                  (let ((match-rest
                         (loop (cdr s)))
                        (match-first (car s)))
                    (lambda (e failure)
                      (match-first e
                                   (lambda ()
                                     (match-rest e failure)))))))))
    (lambda patterns
      (loop patterns))))

(define expand-qq
  (letrec ((expand-qq
            (lambda (e)
              (cond ((unquote? e) (cadr e))
                    ((unquote-splicing? e)
                     (error 'expand-qq
                            "unquote-splicing here makes no sense!"))
                    ((pair? e)
                     (let ((a (car e))
                           (b (cdr e)))
                       (cond ((unquote-splicing? a)
                              `(append ,(cadr a) ,(expand-qq b)))
                             ((unquote-splicing? b)
                              `(cons ,(expand-qq a) ,(cadr b)))
                             (else `(cons ,(expand-qq a) ,(expand-qq b))))))
                    ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
                    ((or (null? e) (symbol? e)) `',e)
                    (else e))))
           (optimize-qq-expansion (lambda (e) (optimizer e (lambda () e))))
           (optimizer
            (compose-patterns
             (pattern-rule
              `(append ,(? 'e) '())
              (lambda (e) (optimize-qq-expansion e)))
             (pattern-rule
              `(append ,(? 'c1 const-pair?) (cons ,(? 'c2 const?) ,(? 'e)))
              (lambda (c1 c2 e)
                (let ((c (quotify `(,@(unquotify c1) ,(unquotify c2))))
                      (e (optimize-qq-expansion e)))
                  (optimize-qq-expansion `(append ,c ,e)))))
             (pattern-rule
              `(append ,(? 'c1 const-pair?) ,(? 'c2 const-pair?))
              (lambda (c1 c2)
                (let ((c (quotify (append (unquotify c1) (unquotify c2)))))
                  c)))
             (pattern-rule
              `(append ,(? 'e1) ,(? 'e2))
              (lambda (e1 e2)
                (let ((e1 (optimize-qq-expansion e1))
                      (e2 (optimize-qq-expansion e2)))
                  `(append ,e1 ,e2))))
             (pattern-rule
              `(cons ,(? 'c1 const?) (cons ,(? 'c2 const?) ,(? 'e)))
              (lambda (c1 c2 e)
                (let ((c (quotify (list (unquotify c1) (unquotify c2))))
                      (e (optimize-qq-expansion e)))
                  (optimize-qq-expansion `(append ,c ,e)))))
             (pattern-rule
              `(cons ,(? 'e1) ,(? 'e2))
              (lambda (e1 e2)
                (let ((e1 (optimize-qq-expansion e1))
                      (e2 (optimize-qq-expansion e2)))
                  (if (and (const? e1) (const? e2))
                      (quotify (cons (unquotify e1) (unquotify e2)))
                      `(cons ,e1 ,e2))))))))
    (lambda (e)
      (optimize-qq-expansion
       (expand-qq e)))))

(define beginify
  (lambda (s)
    (cond
      ((null? s) *void-object*)
      ((null? (cdr s)) (car s))
      (else `(begin ,@s)))))

(define identify-lambda
  (lambda (argl ret-simple ret-opt ret-var)
    (cond 
      ((null? argl) (ret-simple '()))
      ((var? argl) (ret-var argl))     
      (else (identify-lambda (cdr argl)
                             (lambda (s) (ret-simple `(,(car argl) ,@s))) ;simple
                             (lambda (s opt) (ret-opt `(,(car argl) ,@s) opt)) ;opt
                             (lambda (var) (ret-opt `(,(car argl)) var)))))))

(define parse
  (let ((run 
         (compose-patterns
          (pattern-rule
           (? 'c simple-const?)
           (lambda (c) `(const ,c)))
          (pattern-rule
           `(quote ,(? 'c))
           (lambda (c) `(const ,c)))
          (pattern-rule
           (? 'v var?)
           (lambda (v) `(var ,v)))
          (pattern-rule
           `(if ,(? 'test) ,(? 'dit))
           (lambda (test dit) `(if3 ,(parse test) ,(parse dit) (const ,*void-object*))))
          (pattern-rule
           `(if ,(? 'test) ,(? 'dit) ,(? 'dif))
           (lambda (test dit dif) `(if3 ,(parse test) ,(parse dit) ,(parse dif))))            
          (pattern-rule
           `(or ,@(? 'args))
           (lambda (args) (cond ((null? args) (parse #f)) ((= (length args) 1) (parse (car args)))  (else `(or ,(map parse args))))))
          (pattern-rule 
           `(lambda ,(? 'args no-dups?) ,@(? 'body not-null?))
           (lambda (args body) (identify-lambda args 
                                                ;;simple
                                                (lambda (args) (cons 'lambda-simple (cons args (list (parse `(begin ,@body))))))
                                                ;;opt
                                                (lambda (args opt) (cons 'lambda-opt (cons args (cons opt (list (parse `(begin ,@body)))))))
                                                ;;var
                                                (lambda (args) (cons 'lambda-var (cons args (list (parse `(begin ,@body))))))
                                                )
             ))
          (pattern-rule 
           `(let ,(? 'bindings let-bindings?) ,@(? 'body))
           (lambda (bindings body) (parse `((lambda ,(map car bindings) ,@body) ,@(map cadr bindings)))))
          (pattern-rule
           `(let* () ,(? 'expr) . ,(? 'exprs list?))
           (lambda (expr exprs) (parse `(let () ,(beginify (cons expr exprs))))))
          (pattern-rule
           `(let* ((,(? 'var var?) ,(? 'val)) . ,(? 'bindings let-bindings?)) ,@(? 'exprs))
           (lambda (var val bindings exprs) (if (null? bindings) (parse `(let ((,var ,val)) ,(beginify exprs))) (parse `(let ((,var ,val)) (let* ,bindings ,(beginify exprs)))))))
          (pattern-rule
           `(begin .,(? 'expressions))
           (lambda (expressions)
             (letrec ((lst (list))
                      (cleaner (lambda (begin-follow) (for-each (lambda (exp) (cond ((and (list? exp) (equal? (car exp) 'begin)) 
                                                                                     (cleaner (cdr exp)))
                                                                                    ((not (null? exp)) (set! lst (append lst (list exp))))))
                                                                begin-follow) lst)))
               (cleaner expressions) 
               (cond ((> (length lst) 1) `(seq ,(map parse lst))) ((= (length lst) 1) (parse (car lst))) 
                     ((null? lst) `(const ,*void-object*))))))
          (pattern-rule
           `(define ,(? 'v var?) ,(? 'expr))
           (lambda (v expr) `(def ,(parse v) ,(parse expr))))
          (pattern-rule
           `(define (,(? `v var?) ,@(? 'args)) ,@(? 'exprs not-null?))
           (lambda (v args exprs) (parse `(define ,v (lambda ,args ,@exprs)))))
          (pattern-rule 
           `(set! ,(? 'v var?) ,(? 'expr))
           (lambda (v expr) `(set ,(parse v) ,(parse expr))))
          (pattern-rule
           `(letrec ,(? 'bindings let-bindings?) ,@(? 'body))
           (lambda (bindings body) (parse `(let ,(map (lambda (pair) `(,(car pair) #f)) bindings) (begin ,@(map (lambda (pair) `(set! ,(car pair) ,(cadr pair))) bindings) (let () ,@body)))))) 
          (pattern-rule 
           `(,(? 'v not-reserved?) ,@(? 'args))
           (lambda (v args) `(applic ,(parse v) ,(map parse args))))
          (pattern-rule
           `(and . ,(? 'args)) 
           (lambda (args) (cond ((null? args) (parse #t)) ((= (length args) 1) (parse (car args)))  ((= (length args) 2) (parse `(if ,(car args) ,(cadr args) #f)))    (else (parse `(if ,(car args) (and ,@(cdr args)) #f))))))
          (pattern-rule
           `(cond (,(? 'pred) ,(? 'exp) . ,(? 'more)) . ,(? 'rest))
           (lambda (pred exp more rest) (cond ((eq? pred 'else) (parse `(begin ,exp ,@more))) ((null? rest) (parse `(if ,pred (begin ,exp ,@more))))
                                              (else (parse `(if ,pred (begin ,exp ,@more) (cond ,@rest)))))))
          (pattern-rule 
           `,(? 'lst qq?)
           (lambda (lst) (parse (expand-qq  (cadr lst)))))
          )))
    (lambda (e)
      (run e
           (lambda ()
             (error 'parse
                    (format "I can't recognize this: ~s" e)))))))


(define ret-ds+es 
  (lambda (ds es) 
    (letrec ((beginning '())
             (ending '())
             (split-es
              (lambda (es)
                (cond ((null? es) beginning)
                      
                      ((and (list? (car es)) (not (null? (car es))) (or (equal? (caar es) 'lambda-simple) (equal? (caar es) 'lambda-opt) (equal? (caar es) 'lambda-var))) 
                       (if (equal? (caar es) 'lambda-opt)
                           (begin
                             (set! ending (append ending (cdddar es)))
                             `(,@beginning (,(caar es) ,(cadar es) ,(caddar es)
                                                       (applic 
                                                        (lambda-simple ,(map cadadr ds) (seq (,@(map (lambda (def) `(set ,(cadr def) ,(caddr def))) ds) ,@ending))) 
                                                        ,(map (lambda (x) `(const #f)) ds))))
                             )
                           (begin
                             (set! ending (append ending (cddar es)))
                             `(,@beginning (,(caar es) ,(cadar es) 
                                                       (applic 
                                                        (lambda-simple ,(map cadadr ds) (seq (,@(map (lambda (def) `(set ,(cadr def) ,(caddr def))) ds) ,@ending))) 
                                                        ,(map (lambda (x) `(const #f)) ds))))
                             )
                           ))
                      
                      ((list? (car es)) 
                       (begin (set! beginning (if (null? beginning) (cons (car es) beginning) (append beginning (list (car es))))) 	  (split-es (cdr es))))
                      
                      ((or (equal? (car es) 'lambda-simple) (equal? (car es) 'lambda-opt) (equal? (car es) 'lambda-var)) 
                       (if (equal? (car es) 'lambda-opt) 
                           (begin
                             (set! ending (append ending (cdddr es)))
                             `(,@beginning ,(car es) ,(cadr es) ,(caddr es) 
                                           (applic 
                                            (lambda-simple ,(map cadadr ds) (seq (,@(map (lambda (def) `(set ,(cadr def) ,(caddr def))) ds) ,@ending))) 
                                            ,(map (lambda (x) `(const #f)) ds)))
                             )
                           (begin
                             (set! ending (append ending (cddr es)))
                             `(,@beginning ,(car es) ,(cadr es) 
                                           (applic 
                                            (lambda-simple ,(map cadadr ds) (seq (,@(map (lambda (def) `(set ,(cadr def) ,(caddr def))) ds) ,@ending))) 
                                            ,(map (lambda (x) `(const #f)) ds)))
                             )))  
                      
                      (else (begin (set! beginning (if (null? beginning) (cons (car es) beginning) (cons beginning  (car es)))) (split-es (cdr es))))
                      )))) 
      (split-es es)        
      )))     

(define get-ds (lambda (ds es) ds))  

(define separate
  (lambda (pes ret-ds+es)
    (if (null? pes) (ret-ds+es '() '())
        
        (separate (cdr pes)
                  (lambda (ds es)
                    (cond ((and (pair? (car pes)) (equal? (caar pes) 'def))
                           (ret-ds+es (cons (car pes) ds) es))
                          ((and (pair? (car pes)) (equal? (caar pes) 'seq))
                           (separate (cadar pes)
                                     (lambda (ds1 es1) (ret-ds+es (append ds1 ds) (append es1 es)))))
                          (else (ret-ds+es ds (cons (car pes) es))))
                    )))))



(define eliminate-nested-defines 
  (lambda (parsed-exp)
    (letrec ((cleaner (lambda (innerexp)
                        (if (or (not (list? innerexp)) (null? (separate innerexp get-ds))) innerexp
                            (separate innerexp ret-ds+es))))
             (eliminator (lambda (innerexp)
                           (cond ((and (pair? innerexp) (is-lambda? innerexp))
                                  (if (equal? (car innerexp) 'lambda-opt)
                                      `(,(car innerexp) ,(cadr innerexp) ,(caddr innerexp) ,(eliminator (cadddr (cleaner innerexp))))
                                      `(,(car innerexp) ,(cadr innerexp) ,(eliminator (caddr (cleaner innerexp))))))
                                 ((list? innerexp) (map (lambda (x) (eliminator (cleaner x))) innerexp))
                                 (else innerexp))
                           )))
      (eliminator parsed-exp))))

(define remove-applic-lambda-nil
  (lambda (exp)
    (letrec (
             (cleaner (lambda (innerexp)
                        (cond 
                          ((and (pair? innerexp) (equal? (car innerexp) 'applic) (equal? (caadr innerexp) 'lambda-simple) (null? (cadadr innerexp)))
                           `,(cleaner (car (cddadr innerexp))))
                          
                          ((list? innerexp) (map cleaner innerexp))
                          
                          (else innerexp))
                        ))) 
      (cleaner exp))
    ))

(define has-bound-occurence?
  (lambda (var ast scope params) 
    (letrec ((flag #f)
             (started-search? #f)
             (search (lambda (var ast scope params)
                       (cond 
                         ((and (pair? ast) (equal? (car ast) 'lambda-simple)) 
                          (if (not started-search?) (begin (set! started-search? #t) 
                                                           (search var (caddr ast) (append scope params) (cadr ast))) 
                              (if (not (member var (cadr ast)))
                                  (search var (caddr ast) (append scope params) (cadr ast))
                                  )))
                         ((and (pair? ast) (equal? (car ast) 'lambda-opt))
                          (if (not started-search?) (begin (set! started-search? #t)
                                                           (search var (cadddr ast) (append scope params) (append (cadr ast) (list (caddr ast)))))
                              (if (not (member var (append (cadr ast) (list (caddr ast))))) 
                                  (search var (cadddr ast) (append scope params) (append (cadr ast) (list (caddr ast)))))))
                         ((and (pair? ast) (equal? (car ast) 'lambda-var))
                          (if (not started-search?) (begin (set! started-search? #t) (search var (caddr ast) (append scope params) (list (cadr ast)))) 
                              (if (not (equal? var ast))
                                  (search var (caddr ast) (append scope params) (list (cadr ast))))))
                         ((and (pair? ast) (equal? (car ast) 'var) (equal? (cadr ast) var)) 
                          (begin (if (not started-search?) (set! started-search? #t)) 
                                 (if (and (member var scope) (not (member var params))) (set! flag #t))))
                         ((list? ast) (begin (if (not started-search?) (set! started-search? #t)) 
                                             (map (lambda (x) (search var x scope params)) ast)))
                         ))))
      (begin (search var ast scope params) 
             flag))))

(define is-being-set?           
  (lambda (var expr)
    (letrec ((flag #f)
             (search (lambda (var-to-find exp)  
                       (if (and (pair? exp) (equal? (car exp) 'set) (equal? (caadr exp) 'var) (equal? (cadadr exp) var-to-find)) (set! flag #t)
                           (for-each (lambda (x) 
                                       (cond ((and (list? x) (not (null? x)) (equal? (car x) 'set) (equal? (caadr x) 'var) (equal? (cadadr x) var-to-find)) (set! flag #t))			
                                             ((list? x) (search var-to-find x)))) 
                                     exp))
                       )))
      (begin (search var expr) flag)
      )))

(define has-get-occurence?
  (lambda (var ast scope params) 
    (letrec ((flag #f)
             (search (lambda (var ast scope params)
                       (cond 
                         ((and (pair? ast) (equal? (car ast) 'lambda-simple))
                          (search var (caddr ast) (append scope params) (cadr ast)))
                         ((and (pair? ast) (equal? (car ast) 'lambda-opt))
                          (search var (cadddr ast) (append scope params) (append (cadr ast) (list (caddr ast)))))
                         ((and (pair? ast) (equal? (car ast) 'lambda-var))
                          (search var (caddr ast) (append scope params) (list (cadr ast))))
                         ((and (pair? ast) (equal? (car ast) 'set))
                          (search var (caddr ast) scope params))
                         ((and (pair? ast) (equal? (car ast) 'var) (equal? (cadr ast) var)) 
                          (if (or (member var scope) (member var params)) (set! flag #t)))
                         ((pair? ast) (map (lambda (x) (search var x scope params)) ast))
                         ))))
      (begin (search var ast scope params) flag))))

(define should-box?
  (lambda (pes var exp scope parameters) 
    (and (has-bound-occurence? var exp scope parameters) (is-being-set? var pes) (has-get-occurence? var pes scope parameters)))) 

(define create-sets
  (lambda (pes params exp scope) 
    (map (lambda (param) `(set (var ,param) (box (var ,param)))) (filter (lambda (x) (should-box? pes x exp scope params)) params))))

(define is-lambda? (lambda (exp) (or (equal? (car exp) 'lambda-simple) (equal? (car exp) 'lambda-opt) (equal? (car exp) 'lambda-var))))

(define boxer-simple
  (lambda (parsed-exp innerexp body scope parameters) 
    (cond ((null? (create-sets parsed-exp (cadr innerexp) innerexp (append scope (cadr innerexp)))) 
           `(lambda-simple ,(cadr innerexp) ,(boxer parsed-exp (caddr innerexp) (caddr innerexp) (append scope (cadr innerexp)) (cadr innerexp))))
          ((and (pair? (caddr innerexp)) (equal? (caaddr innerexp) 'seq))  
           `(lambda-simple ,(cadr innerexp) (seq ,(append (create-sets parsed-exp (cadr innerexp) innerexp (append scope (cadr innerexp)))  (boxer parsed-exp (car (cdaddr innerexp)) (car (cdaddr innerexp)) (append scope (cadr innerexp)) (cadr innerexp))))))
          
          (else `(lambda-simple ,(cadr innerexp) (seq ,(append (create-sets parsed-exp (cadr innerexp) innerexp (append scope (cadr innerexp))) (list (boxer parsed-exp (caddr innerexp) (caddr innerexp) (append scope (cadr innerexp)) (cadr innerexp))))))))))

(define boxer-var
  (lambda (parsed-exp innerexp body scope parameters) 
    (cond ((null? (create-sets parsed-exp (list (cadr innerexp)) innerexp 
                               (append scope (list (cadr innerexp))))) 
           `(lambda-var ,(cadr innerexp) ,(boxer parsed-exp (caddr innerexp) (caddr innerexp) (append scope (list (cadr innerexp))) (list (cadr innerexp)))))
          ((and (pair? (caddr innerexp)) (equal? (caaddr innerexp) 'seq)) 
           `(lambda-var ,(cadr innerexp) 
                        (seq ,(append (create-sets parsed-exp (list (cadr innerexp)) innerexp 
                                                   (append scope (list (cadr innerexp))))  (boxer parsed-exp (car (cdaddr innerexp)) (car (cdaddr innerexp)) (append scope (list (cadr innerexp))) (list (cadr innerexp)))))))
          (else `(lambda-var ,(cadr innerexp) (seq ,(append (create-sets parsed-exp (cadr innerexp) innerexp (append scope (list (cadr innerexp)))) (list (boxer (caddr innerexp) (caddr innerexp) (append scope (list (cadr innerexp))) (list (cadr innerexp)))))))))))

(define boxer-opt
  (lambda (parsed-exp innerexp body scope parameters) 
    (cond ((null? (create-sets parsed-exp (append (cadr innerexp) (list (caddr innerexp))) innerexp 
                               (append scope (append (cadr innerexp) (list (caddr innerexp)))))) 
           `(lambda-opt ,(cadr innerexp) ,(caddr innerexp) ,(boxer parsed-exp (cadddr innerexp) (cadddr innerexp) (append scope (append (cadr innerexp) (list (caddr innerexp)))) (append (cadr innerexp) (list (caddr innerexp))))))
          ((and (pair? (cadddr innerexp)) (equal? (car (cadddr innerexp)) 'seq)) 
           `(lambda-opt ,(cadr innerexp) ,(caddr innerexp) (seq ,(append (create-sets parsed-exp (append (cadr innerexp) (list (caddr innerexp))) innerexp (append scope (append (cadr innerexp) (list (caddr innerexp)))))  (boxer parsed-exp (cadr (cadddr innerexp)) (cadr (cadddr innerexp)) (append scope (append (cadr innerexp) (list (caddr innerexp)))) (append (cadr innerexp) (list (caddr innerexp))))))))
          
          (else `(lambda-opt ,(cadr innerexp) ,(caddr innerexp) 
                             (seq ,(append (create-sets parsed-exp (append (cadr innerexp) (list (caddr innerexp))) innerexp 
                                                        (append scope (append (cadr innerexp) (list (caddr innerexp))))) 
                                           (list (boxer parsed-exp (cadddr innerexp) (cadddr innerexp) (append scope (append (cadr innerexp) (list (caddr innerexp)))) (append (cadr innerexp) (list (caddr innerexp))))))))))))


(define boxer
  (lambda (parsed-exp innerexp body scope parameters) 
    (cond 
      ((and (pair? innerexp) (equal? (car innerexp) 'lambda-simple))
       (boxer-simple parsed-exp innerexp body scope parameters))
      
      ((and (pair? innerexp) (equal? (car innerexp) 'lambda-var))
       (boxer-var parsed-exp innerexp body scope parameters))
      
      ((and (pair? innerexp) (equal? (car innerexp) 'lambda-opt))
       (boxer-opt parsed-exp innerexp body scope parameters))
      
      ((and (pair? innerexp) (equal? (car innerexp) 'set) (should-box? parsed-exp (cadadr innerexp) body scope parameters)) `(box-set (var ,(cadadr innerexp)) ,(boxer parsed-exp (caddr innerexp) body scope parameters)))
      
      ((and (list? innerexp) (equal? (length innerexp) 2) (equal? (car innerexp) 'var) (should-box? parsed-exp (cadr innerexp) body scope parameters))  `(box-get (var ,(cadr innerexp))))
      
      ((list? innerexp) (map (lambda (x) (boxer parsed-exp x body scope parameters))  innerexp))
      
      (else innerexp ))))


(define box-set 
  (lambda (parsed-exp)                                        
    (boxer parsed-exp parsed-exp parsed-exp '() '())))

(define get-minor-index (lambda (param-to-find params count) 
                          (if (equal? param-to-find (car params)) count (get-minor-index param-to-find (cdr params) (+ count 1)))))

(define is-in-list-of-params?
  (lambda (var-to-find list-of-params)
    (cond 
      ((null? list-of-params) #f)
      ((member var-to-find (car list-of-params)) #t)
      (else (is-in-list-of-params? var-to-find (cdr list-of-params))))))

(define create-bvar
  (lambda (var-to-create list-of-params major-index)
    (cond 
      ((member var-to-create (car list-of-params)) `(bvar ,var-to-create ,major-index ,(get-minor-index var-to-create (car list-of-params) 0)))
      (else (create-bvar var-to-create (cdr list-of-params) (+ 1 major-index))))))


(define analyse (lambda (ast scope params) 
                  (cond 
                    ((and (pair? ast) (is-lambda? ast))
                     (cond ((equal? (car ast) 'lambda-var) 
                            `(,(car ast) ,(cadr ast) ,(analyse (caddr ast) (append (list params) scope) (list (cadr ast)))))
                           ((equal? (car ast) 'lambda-simple) 
                            `(,(car ast) ,(cadr ast) ,(analyse (caddr ast) (append (list params) scope) (cadr ast))))
                           (else 
                            `(,(car ast) ,(cadr ast) ,(caddr ast) ,(analyse (cadddr ast) (append (list params) scope) (append (cadr ast) (list (caddr ast))))))))
                    ((and (pair? ast) (equal? (car ast) 'var)) 
                     (cond 
                       ((member (cadr ast) params) `(pvar ,(cadr ast) ,(get-minor-index (cadr ast) params 0)))
                       ((is-in-list-of-params? (cadr ast) scope) (create-bvar (cadr ast) scope 0))
                       (else `(fvar ,(cadr ast)))))
                    ((list? ast) (map (lambda (x) (analyse x scope params)) ast))
                    (else ast))))

(define pe->lex-pe (lambda (pe) (analyse pe '() '())))

(define annotate (lambda (expr tp?)
                   (cond ((and (pair? expr) (or (equal? (car expr) 'fvar) (equal? (car expr) 'pvar) (equal? (car expr) 'bvar) (equal? (car expr) 'const)))
                          expr)
                         ((and (pair? expr) (equal? (car expr) 'applic))
                          (if tp? 
                              `(tc-applic ,(annotate (cadr expr) #f) ,(map (lambda (x) (annotate x #f)) (caddr expr))) 
                              `(applic ,(annotate (cadr expr) #f) ,(map (lambda (x) (annotate x #f)) (caddr expr)))))
                         ((and (pair? expr) (equal? (car expr) 'or))
                          `(or ,(append (reverse (map (lambda (x) (annotate x #f)) (cdr (reverse (cadr expr))))) (list (annotate (car (reverse (cadr expr))) tp?)))))
                         ((and (pair? expr) (equal? (car expr) 'if3))
                          `(if3 ,(annotate (cadr expr) #f) ,(annotate (caddr expr) tp?) ,(annotate (cadddr expr) tp?)))
                         ((and (pair? expr) (or (equal? (car expr) 'def) (equal? (car expr) 'set) (equal? (car expr) 'box-set)))
                          `(,(car expr) ,(cadr expr) ,(annotate (caddr expr) #f)))
                         ((and (pair? expr) (is-lambda? expr))
                          (if (equal? (car expr) 'lambda-opt) 
                              `(,(car expr) ,(cadr expr) ,(caddr expr) ,(annotate (cadddr expr) #t))
                              `(,(car expr) ,(cadr expr) ,(annotate (caddr expr) #t))
                              ))
                         ((and (pair? expr) (equal? (car expr) 'seq))
                          `(seq ,(append (reverse (map (lambda (x) (annotate x #f)) (cdr (reverse (cadr expr))))) (list (annotate (car (reverse (cadr expr))) tp?)))))
                         (else expr)
                         )))

(define annotate-tc (lambda (parsed-expr) (annotate parsed-expr #f)))

(define file->string
    (lambda (in-file)
      (let ((in-port (open-input-file in-file)))
        (letrec ((run
                  (lambda ()
                    (let ((ch (read-char in-port)))
                      (if (eof-object? ch)
                          (begin
                            (close-input-port in-port)
                            '())
                          (cons ch (run)))))))
          (list->string (run))))))
          
(define string->file
    (lambda (code out-file)
        (call-with-output-file out-file  
			(lambda (output-port)
				(display code output-port))
				'truncate)))
          
(define full
    (lambda (s-expression)
        (annotate-tc (pe->lex-pe (box-set (remove-applic-lambda-nil (eliminate-nested-defines (parse s-expression))))))))
 
(define is-x?
    (lambda (exp x)
        (and (pair? exp) (equal? (car exp) x))))
        
(define label-count 0)
        
; pre-condition: label-name is a string
(define make-label
    (lambda (label-name)
        (string-append label-name (number->string (begin (set! label-count (+ label-count 1)) label-count)))))
   
(define code-gen-if3
    (lambda (pes env-counter param-counter)
        (let ((label_else (make-label "L_if3_else_"))
              (label_exit (make-label "L_if3_exit_")))
        (string-append (code-gen (car pes) env-counter param-counter) 
                                                "MOV(R0,INDD(R0,1));\n"
						"CMP(R0,IMM(0));\n" 
						"JUMP_EQ(" label_else ");\n"
						(code-gen (cadr pes) env-counter param-counter)
						"JUMP(" label_exit ");\n"
						label_else ":\n"
						(code-gen (caddr pes) env-counter param-counter)
						label_exit ":\n"))))
          
(define code-gen-seq
    (lambda (pes env-counter param-counter)
        (apply string-append (map (lambda (x) (code-gen x env-counter param-counter)) pes))))
        
(define code-gen-applic
    (lambda (pes env-counter param-counter)
        (string-append (apply string-append 
						(map (lambda (x) (string-append (code-gen x env-counter param-counter) "PUSH(R0);\n")) 
							 (reverse (cadr pes))))
						"PUSH(IMM(" (number->string (length (cadr pes))) "));\n"
						(code-gen (car pes) env-counter param-counter)
						"CMP(IND(R0),IMM(T_CLOSURE));\n" 
						"JUMP_NE(L_error_cannot_apply_non_clos);\n" 
						"PUSH(INDD(R0, 1));\n" 
						"CALLA(INDD(R0, 2));\n" 
						"DROP(1);\n" 
						"POP(R1);\n" 
						"DROP(R1);\n")))
						
(define code-gen-tc-applic
	(lambda (pes env-counter A_params)
		(let ((B_params (length (cadr pes)))
			  (sizeB (+ 3 (length (cadr pes))))
			  (sizeA (+ A_params 4))
			  (loop_start (make-label "tc_applic_loop_start_"))
			  (loop_finish (make-label "tc_applic_loop_finish_")))
		(string-append (apply string-append 
						(map (lambda (x) (string-append (code-gen x env-counter A_params) 
														"PUSH(R0);\n")) 
							 (reverse (cadr pes))))								    ;;; push An-1, An-2, ... , A0
						"PUSH(IMM(" (number->string B_params) "));\n"    				;;; push n 
						(code-gen (car pes) env-counter A_params)
						"CMP(IND(R0),IMM(T_CLOSURE));\n" 						    ;;; [[proc]]
						"JUMP_NE(L_error_cannot_apply_non_clos);\n"
						"PUSH(INDD(R0, 1));\n" 										;;; push env
						"PUSH(FPARG(-1));\n"										;;; push return address
						"MOV(R1, FPARG(-2));\n"										;;; save old fp
						"MOV(R2, IMM(0));\n"				;;;;;; i = 0
						loop_start ":\n"
						"CMP(R2, IMM(" (number->string sizeB) "));\n"		;;;; i < sizeB
						"JUMP_GE(" loop_finish ");\n"
						"MOV(R3, IMM("  (number->string A_params) "));\n"
						"ADD(R3, 1);\n"
						"SUB(R3, R2);\n"			;;;;;; R3 = n+1-i
						"MOV(R4, R2);\n"
						"NEG(R4);\n"
						"INCR(R4);\n"
						"SUB(R4, IMM(3));\n"		;;;;;; R4 = -i-3
						"MOV(FPARG(R3), FPARG(R4));\n"
						"INCR(R2);\n"
						"JUMP(" loop_start ");\n"
						loop_finish ":\n"
						"DROP(" (number->string sizeA) ");\n"
						"MOV(FP, R1);\n"
						"JUMPA(INDD(R0, 2));\n"))))
                
(define code-gen-or 
    (lambda (pes env-counter param-counter) 
      (let ((label_exit (make-label "L_or_exit_"))
            (or_lst_without_last_element  (if (null? pes) #f (reverse (cdr (reverse pes))))))
        (if (equal? or_lst_without_last_element #f)
            (code-gen-const #f)       
            (string-append (apply string-append (map (lambda (or-element)
									(string-append (code-gen or-element env-counter param-counter)
                                         "CMP(R0,IMM(SOB_FALSE));\n" "JUMP_NE(" label_exit ");\n"))
                               or_lst_without_last_element))
                   (code-gen (car (list-tail pes (- (length pes) 1))) env-counter param-counter)
                   label_exit ":\n")))
            ))
    
(define code-gen-pvar 
    (lambda (pvar_min)
        (string-append "MOV(R0,FPARG(2+" (number->string (cadr pvar_min)) "));\n")))
    
(define code-gen-bvar 
    (lambda (bvar_maj_min)
        (string-append "MOV(R0,FPARG(0));\n" 
                       "MOV(R0,INDD(R0," (number->string (cadr bvar_maj_min)) "));\n"
                       "MOV(R0,INDD(R0," (number->string (caddr bvar_maj_min)) "));\n"
                       )))
		
(define code-gen-set
        (lambda (pes env-counter param-counter) 
			(let ((bvar_maj_min (cdar pes)))
            (cond ((equal? (caar pes) 'pvar)
                        (string-append (code-gen (cadr pes) env-counter param-counter) 
									   "MOV(FPARG(2+" (number->string (caddar pes)) "), R0);\n"
                                       (code-gen-const (void))))
                  ((equal? (caar pes) 'bvar)
                        (string-append (code-gen (cadr pes) env-counter param-counter)
										 "MOV(R1,FPARG(0));\n" 
										 "MOV(R1,INDD(R1," (number->string (cadr bvar_maj_min)) "));\n"
										 "MOV(INDD(R1," (number->string (caddr bvar_maj_min)) "), R0);\n"
										 (code-gen-const (void))))
                  ((equal? (caar pes) 'fvar) 
                    (begin (insert-to-global-table (cadar pes))    (string-append 
                                    (code-gen (cadr pes) env-counter param-counter) 
                            "MOV(IND(" (number->string (lookup (cadar pes) global-table)) "), R0);\n"
                            (code-gen-const (void))
                            )))))))
                
(define code-gen-def
    (lambda (pes env-counter param-counter)
		(begin (insert-to-global-table (cadar pes))
			   (code-gen-set pes env-counter param-counter))))
			   
(define code-gen-box 
    (lambda (pes env-counter param-counter)
        (string-append 
         (code-gen pes env-counter param-counter)
                    "MOV(R1,R0);\n"
                    "PUSH(IMM(1));\n"
                    "CALL(MALLOC);\n"
                    "DROP(1);\n"
                    "MOV(IND(R0),R1);\n"
                    )))
        
(define code-gen-box-get
    (lambda (pes env-counter param-counter)
        (string-append (code-gen pes env-counter param-counter) "MOV(R0, IND(R0));\n")))
        
(define code-gen-box-set
    (lambda (pes env-counter param-counter)
;;         (string-append  (code-gen (cadr pes) env-counter param-counter)   ; [[e]]
;;                  "MOV(R15, R0);\n"
;;                  (code-gen (car pes) env-counter param-counter)    ; [[var]]
;;                  "MOV(IND(R0), R15);\n" (code-gen-const (void)))))
                 
        (let ((bvar_maj_min (cdar pes)))
            (cond ((equal? (caar pes) 'pvar)
                        (string-append (code-gen (cadr pes) env-counter param-counter) 
                                        "MOV(IND(FPARG(2+" (number->string (caddar pes)) ")), R0);\n"
                                       (code-gen-const (void))))
                  ((equal? (caar pes) 'bvar)
                        (string-append (code-gen (cadr pes) env-counter param-counter)
                                        "MOV(R1,FPARG(0));\n" 
                                        "MOV(R1,INDD(R1," (number->string (cadr bvar_maj_min)) "));\n"
                                        "MOV(R1, INDD(R1," (number->string (caddr bvar_maj_min)) "));\n"
                                        "MOV(IND(R1), R0);\n"
                                        (code-gen-const (void))))
                  ((equal? (caar pes) 'fvar) 
                    (begin (insert-to-global-table (cadar pes))    
                                (string-append 
                                    (code-gen (cadr pes) env-counter param-counter)
                                    "MOV(R1, IND(" (number->string (lookup (cadar pes) global-table)) "));\n"
                            "MOV(IND(R1), R0);\n"
                            (code-gen-const (void))
                            )))))))

(define code-gen-lambda-simple
    (lambda (pes env-counter param-counter)
        (let ((closure_label (make-label "L_clos_body_"))
              (closure_exit_label (make-label "L_clos_exit_"))
			  (loop1_start (make-label "L_first_loop_start_"))
			  (loop1_finish (make-label "L_first_loop_finish_"))
			  (loop2_start (make-label "L_second_loop_start_"))
			  (loop2_finish (make-label "L_second_loop_finish_"))
			  (major (+ env-counter 1))
			  (params (length (car pes))))
            (string-append "MOV(R1, FPARG(0));\n"
                "PUSH(IMM(" (number->string major) "));\n"
                "CALL(MALLOC);\n"
                "DROP(1);\n"
                "MOV(R2, R0);\n"
                "MOV(R4, IMM(0));\n" ;;;;   i
				"MOV(R5, IMM(1));\n" ;;;;   j
				loop1_start ":\n"
				"CMP(R4, IMM(" (number->string (- major 1)) "));\n"
				"JUMP_GE(" loop1_finish ");\n"
				"MOV(R6, INDD(R1, R4));\n"	;;;; R6 = R1[i]
				"MOV(INDD(R2, R5), R6);\n"  ;;;; R2[j] = R6
				"INCR(R4);\n"
				"INCR(R5);\n"
				"JUMP(" loop1_start ");\n"
				loop1_finish ":\n"
                "MOV(R3, FPARG(1));\n"
                "PUSH(R3);\n"
                "CALL(MALLOC);\n"
                "DROP(1);\n"
                "MOV(IND(R2), R0);\n"
				"MOV(R6, IND(R2));\n" ;;;;   R2 = R2[0]
				"MOV(R4, IMM(0));\n"  ;;;;   i
				"MOV(R5, IMM(2));\n"  ;;;;   j
				loop2_start ":\n"
				"CMP(R4, R3);\n"
				"JUMP_GE(" loop2_finish ");\n"
				"MOV(INDD(R6, R4), FPARG(R5));\n"
				"INCR(R4);\n"
				"INCR(R5);\n"
				"JUMP(" loop2_start ");\n"
				loop2_finish ":\n"
                "PUSH(IMM(3));\n"
                "CALL(MALLOC);\n"
                "DROP(1);\n"
                "MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "MOV(INDD(R0,1), R2);\n"
                "MOV(INDD(R0,2), LABEL(" closure_label "));\n"
                "JUMP(" closure_exit_label ");\n"
                closure_label ":\n"
                "PUSH(FP);\n"
                "MOV(FP, SP);\n"
                "CMP(FPARG(1), IMM(" (number->string params) "));\n"
                "JUMP_NE(L_error_lambda_args_count);\n"
                (code-gen (cadr pes) (+ env-counter 1) params)
                "POP(FP);\n"
                "RETURN;\n"
                closure_exit_label ":\n"))))
				
(define code-gen-lambda-var
	(lambda (pes env-counter param-counter)
		(let ((no_opt_start_loop (make-label "L_no_opt_start_"))
			  (no_opt_finish_loop (make-label "L_no_opt_finish_"))
			  (clean_stack_start (make-label "L_clean_stack_start_"))
			  (clean_stack_finish (make-label "L_clean_stack_finish_"))
			  (yes_opt_label (make-label "L_yes_opt_"))
			  (yes_opt_start_loop (make-label "L_yes_opt_start_"))
			  (yes_opt_finish_loop (make-label "L_yes_opt_finish_"))
			  (codeB_end (make-label "L_codeB_end_"))
			  (closure_label (make-label "L_clos_body_"))
              (closure_exit_label (make-label "L_clos_exit_"))
			  (loop1_start (make-label "L_first_loop_start_"))
			  (loop1_finish (make-label "L_first_loop_finish_"))
			  (loop2_start (make-label "L_second_loop_start_"))
			  (loop2_finish (make-label "L_second_loop_finish_"))
			  (major (+ env-counter 1)))
			(string-append 
				"MOV(R1, FPARG(0));\n"
                "PUSH(IMM(" (number->string major) "));\n"
                "CALL(MALLOC);\n"
                "DROP(1);\n"
                "MOV(R2, R0);\n"
                "MOV(R4, IMM(0));\n" ;;;;   i
				"MOV(R5, IMM(1));\n" ;;;;   j
				loop1_start ":\n"
				"CMP(R4, IMM(" (number->string (- major 1)) "));\n"
				"JUMP_GE(" loop1_finish ");\n"
				"MOV(R6, INDD(R1, R4));\n"	;;;; R6 = R1[i]
				"MOV(INDD(R2, R5), R6);\n"  ;;;; R2[j] = R6
				"INCR(R4);\n"
				"INCR(R5);\n"
				"JUMP(" loop1_start ");\n"
				loop1_finish ":\n"
                "MOV(R3, FPARG(1));\n"
                "PUSH(R3);\n"
                "CALL(MALLOC);\n"
                "DROP(1);\n"
                "MOV(IND(R2), R0);\n"
				"MOV(R6, IND(R2));\n" ;;;;   R2 = R2[0]
				"MOV(R4, IMM(0));\n"  ;;;;   i
				"MOV(R5, IMM(2));\n"  ;;;;   j
				loop2_start ":\n"
				"CMP(R4, R3);\n"
				"JUMP_GE(" loop2_finish ");\n"
				"MOV(INDD(R6, R4), FPARG(R5));\n"
				"INCR(R4);\n"
				"INCR(R5);\n"
				"JUMP(" loop2_start ");\n"
				loop2_finish ":\n"
                "PUSH(IMM(3));\n"
                "CALL(MALLOC);\n"
                "DROP(1);\n"
                "MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "MOV(INDD(R0,1), R2);\n"
                "MOV(INDD(R0,2), LABEL(" closure_label "));\n"
                "JUMP(" closure_exit_label ");\n"
                closure_label ":\n"
                "PUSH(FP);\n"
                "MOV(FP, SP);\n"
				
				;;; ^end of part A^ ;;;
				
				"MOV(R1, IMM(SOB_NIL));\n"
				"CMP(FPARG(1), IMM(0));\n"
				"JUMP_GT(" yes_opt_label ");\n"
				"PUSH(IMM(0));\n"
				"MOV(R2, IMM(-2));\n"
				"MOV(R3, IMM(-3));\n"
				no_opt_start_loop ":\n"
				"CMP(R2, IMM(2));\n"
				"JUMP_EQ(" no_opt_finish_loop ");\n"
				"MOV(FPARG(R3), FPARG(R2));\n"
				"MOV(FPARG(R2), R1);\n"
				"INCR(R2);\n"
				"INCR(R3);\n"
				"JUMP(" no_opt_start_loop ");\n"
				no_opt_finish_loop ":\n"
				"ADD(FP, IMM(1));\n"
				"JUMP(" codeB_end ");\n"
				
				yes_opt_label ":\n"
				"MOV(R3, IMM(1));\n"
				"MOV(R2, FPARG(1));\n"
				"INCR(R2);\n"
				yes_opt_start_loop ":\n"
				"CMP(R2, R3);\n"
				"JUMP_LE(" yes_opt_finish_loop ");\n"
				"PUSH(R1);\n"
				"PUSH(FPARG(R2));\n"
				"CALL(MAKE_SOB_PAIR);\n"
				"DROP(2);\n"
				"MOV(R1, R0);\n"
				"DECR(R2);\n"
				"JUMP(" yes_opt_start_loop ");\n"
				yes_opt_finish_loop ":\n"
				"MOV(FPARG(2), R1);\n"
				"MOV(R3, FPARG(1));\n"
				"CMP(R3, IMM(1));\n"
				"JUMP_EQ(" codeB_end ");\n"
				"MOV(R4, FPARG(1));\n"
				"INCR(R3);\n"										;;;; R3 = bottom of stack (102)
				"MOV(R2, IMM(2));\n"								;;;; R2 = index of element to move (decreases)
				clean_stack_start ":\n"
				"CMP(R2, IMM(-2));\n"
				"JUMP_LT(" clean_stack_finish ");\n"
				"MOV(FPARG(R3), FPARG(R2));\n"
				"DECR(R2);\n"
				"DECR(R3);\n"
				"JUMP(" clean_stack_start ");\n"
				clean_stack_finish ":\n"
				"DECR(R4);\n"
				"DROP(R4);\n"
				"SUB(FP, R4);\n"
				codeB_end ":\n"
				"MOV(FPARG(1), IMM(1));\n"
				(code-gen (cadr pes) (+ env-counter 1) 1)
                "POP(FP);\n"
                "RETURN;\n"
				closure_exit_label ":\n"
			))))
				
(define code-gen-lambda-opt
    (lambda (pes env-counter param-counter)
		(let ((params (length (car pes)))
			  (no_opt_start_loop (make-label "L_no_opt_start_"))
			  (no_opt_finish_loop (make-label "L_no_opt_finish_"))
			  (clean_stack_start (make-label "L_clean_stack_start_"))
			  (clean_stack_finish (make-label "L_clean_stack_finish_"))
			  (yes_opt_label (make-label "L_yes_opt_"))
			  (yes_opt_start_loop (make-label "L_yes_opt_start_"))
			  (yes_opt_finish_loop (make-label "L_yes_opt_finish_"))
			  (codeB_end (make-label "L_codeB_end_"))
			  (closure_label (make-label "L_clos_body_"))
              (closure_exit_label (make-label "L_clos_exit_"))
			  (loop1_start (make-label "L_first_loop_start_"))
			  (loop1_finish (make-label "L_first_loop_finish_"))
			  (loop2_start (make-label "L_second_loop_start_"))
			  (loop2_finish (make-label "L_second_loop_finish_"))
			  (major (+ env-counter 1)))
			(string-append 
				"MOV(R1, FPARG(0));\n"
                "PUSH(IMM(" (number->string major) "));\n"
                "CALL(MALLOC);\n"
                "DROP(1);\n"
                "MOV(R2, R0);\n"
                "MOV(R4, IMM(0));\n" ;;;;   i
				"MOV(R5, IMM(1));\n" ;;;;   j
				loop1_start ":\n"
				"CMP(R4, IMM(" (number->string (- major 1)) "));\n"
				"JUMP_GE(" loop1_finish ");\n"
				"MOV(R6, INDD(R1, R4));\n"	;;;; R6 = R1[i]
				"MOV(INDD(R2, R5), R6);\n"  ;;;; R2[j] = R6
				"INCR(R4);\n"
				"INCR(R5);\n"
				"JUMP(" loop1_start ");\n"
				loop1_finish ":\n"
                "MOV(R3, FPARG(1));\n"
                "PUSH(R3);\n"
                "CALL(MALLOC);\n"
                "DROP(1);\n"
                "MOV(IND(R2), R0);\n"
				"MOV(R6, IND(R2));\n" ;;;;   R2 = R2[0]
				"MOV(R4, IMM(0));\n"  ;;;;   i
				"MOV(R5, IMM(2));\n"  ;;;;   j
				loop2_start ":\n"
				"CMP(R4, R3);\n"
				"JUMP_GE(" loop2_finish ");\n"
				"MOV(INDD(R6, R4), FPARG(R5));\n"
				"INCR(R4);\n"
				"INCR(R5);\n"
				"JUMP(" loop2_start ");\n"
				loop2_finish ":\n"
                "PUSH(IMM(3));\n"
                "CALL(MALLOC);\n"
                "DROP(1);\n"
                "MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "MOV(INDD(R0,1), R2);\n"
                "MOV(INDD(R0,2), LABEL(" closure_label "));\n"
                "JUMP(" closure_exit_label ");\n"
                closure_label ":\n"
                "PUSH(FP);\n"
                "MOV(FP, SP);\n"
				
				;;; ^end of part A^ ;;;
				
				"MOV(R1, IMM(SOB_NIL));\n"
				"CMP(FPARG(1), IMM(" (number->string params) "));\n"
				"JUMP_GT(" yes_opt_label ");\n"
				"PUSH(IMM(0));\n"
				"MOV(R2, IMM(-2));\n"
				"MOV(R3, IMM(-3));\n"
				no_opt_start_loop ":\n"
				"CMP(R2, IMM(" (number->string (+ params 2)) "));\n"
				"JUMP_EQ(" no_opt_finish_loop ");\n"
				"MOV(FPARG(R3), FPARG(R2));\n"
				"MOV(FPARG(R2), R1);\n"
				"INCR(R2);\n"
				"INCR(R3);\n"
				"JUMP(" no_opt_start_loop ");\n"
				no_opt_finish_loop ":\n"
				"ADD(FP, IMM(1));\n"
				"JUMP(" codeB_end ");\n"
				
				yes_opt_label ":\n"
				"MOV(R3, IMM(" (number->string (+ params 1)) "));\n"
				"MOV(R2, FPARG(1));\n"
				"INCR(R2);\n"
				yes_opt_start_loop ":\n"
				"CMP(R2, R3);\n"
				"JUMP_LE(" yes_opt_finish_loop ");\n"
				"PUSH(R1);\n"
				"PUSH(FPARG(R2));\n"
				"CALL(MAKE_SOB_PAIR);\n"
				"DROP(2);\n"
				"MOV(R1, R0);\n"
				"DECR(R2);\n"
				"JUMP(" yes_opt_start_loop ");\n"
				yes_opt_finish_loop ":\n"
				"MOV(FPARG(" (number->string (+ params 2)) "), R1);\n"
				"MOV(R3, FPARG(1));\n"
				"CMP(R3, IMM(" (number->string (+ params 1)) "));\n"
				"JUMP_EQ(" codeB_end ");\n"
				"MOV(R4, FPARG(1));\n"
				"INCR(R3);\n"												;;;; R3 = bottom of stack (102)
				"MOV(R2, IMM(" (number->string (+ params 2)) "));\n"		;;;; R2 = index of element to move (decreases)
				clean_stack_start ":\n"
				"CMP(R2, IMM(-2));\n"
				"JUMP_LT(" clean_stack_finish ");\n"
				"MOV(FPARG(R3), FPARG(R2));\n"
				"DECR(R2);\n"
				"DECR(R3);\n"
				"JUMP(" clean_stack_start ");\n"
				clean_stack_finish ":\n"
				"SUB(R4, IMM(" (number->string (+ params 1)) "));\n"
				"DROP(R4);\n"
				"SUB(FP, R4);\n"
				codeB_end ":\n"
				"MOV(FPARG(1), IMM(" (number->string (+ params 1)) "));\n"
				(code-gen (caddr pes) (+ env-counter 1) (+ params 1))
                "POP(FP);\n"
                "RETURN;\n"
				closure_exit_label ":\n"
			))))
      
(define constant-table

            `((1000 ,(void) ("T_VOID"))

             (1001 () ("T_NIL"))

             (1002 #f ("T_BOOL" 0))

             (1004 #t ("T_BOOL" 1))
        ))
		
(define code-gen-const
            (lambda (const)
                (string-append "MOV(R0,IMM(" (number->string (lookup const constant-table)) "));\n")))

;;; scans the code for constants and builds the constant table. 
;;; this is done before the code generation. 
(define build-constant-table
	(lambda (pes)
		(cond ((is-x? pes 'const) (for-each insert-to-const-table (reverse (topo-sort (cadr pes)))))
			  ((pair? pes) (for-each build-constant-table pes)))))

(define insert-to-const-table
        (lambda (const)
            (begin (if (equal? (lookup const constant-table) #f)
            (cond
                ((integer? const) 
                (set! constant-table (append constant-table 
                `((,(+ (last-const-addr) (last-const-length)) ,const ("T_INTEGER" ,const))))))
                ((number? const) 
                 (set! constant-table (append constant-table 
                `((,(+ (last-const-addr) (last-const-length)) ,const ("T_FRACTION" ,(numerator const) ,(denominator const)))))))
                ((string? const)
                (set! constant-table (append constant-table 
                `((,(+ (last-const-addr) (last-const-length)) ,const ("T_STRING" ,(string-length const) ,@(get-string-repres const)))))))
                ((char? const) (set! constant-table (append constant-table
                `((,(+ (last-const-addr) (last-const-length)) ,const ("T_CHAR" ,(char->integer const)))))))
            ((pair? const)
                (set! constant-table (append constant-table 
                 `((,(+ (last-const-addr) (last-const-length)) ,const 
                 ("T_PAIR" ,(lookup (car const) constant-table) ,(lookup (cdr const) constant-table)))))))
            ((vector? const)
                 (set! constant-table (append constant-table 
                 `((,(+ (last-const-addr) (last-const-length)) ,const 
                 ("T_VECTOR" ,(vector-length const) ,@(map (lambda (vec-element) (lookup vec-element constant-table)) (vector->list const))))))))
            ((symbol? const)
            (begin (insert-to-const-table (symbol->string const))
                  (set! constant-table (append constant-table 
                 `((,(+ (last-const-addr) (last-const-length)) ,const 
                 ("T_SYMBOL" ,(lookup (symbol->string const) constant-table))))))))
                )) 
            )))
			
(define last-const-addr   
    (lambda ()
        (caar (reverse constant-table))))
        
(define last-const-length   
    (lambda ()
        (length (caddar (reverse constant-table)))))
		
(define get-string-repres 
    (lambda (str)
        (map char->integer (string->list str))))
        
(define void?
    (lambda (x) 
        (equal? x (void))))
        
(define topo-sort 
    (lambda (e) 
            (cond
                ((or (number? e) (string? e) (char? e) (void? e) (null? e) (boolean? e)) `(,e))
            ((pair? e)
                `(,e ,@(topo-sort (car e)) ,@(topo-sort (cdr e))))
            ((vector? e)
                `(,e ,@(apply append (map topo-sort (vector->list e)))))
            ((symbol? e)
                `(,e ,@(topo-sort (symbol->string e)))))))

(define global-table (list ))

(define add-built-in-funcs 
	(lambda () (begin
		(insert-to-global-table 'car)
		(insert-to-global-table 'cdr)
		(insert-to-global-table 'cons)
		(insert-to-global-table '+)
		(insert-to-global-table '/)
		(insert-to-global-table '-)
		(insert-to-global-table '*)
		(insert-to-global-table 'char?)
		(insert-to-global-table 'boolean?)
		(insert-to-global-table 'integer?)
		(insert-to-global-table 'null?)
		(insert-to-global-table 'pair?)
		(insert-to-global-table 'procedure?)
		(insert-to-global-table 'string?)
		(insert-to-global-table 'symbol?)
		(insert-to-global-table 'vector?)
		(insert-to-global-table 'number?)
		(insert-to-global-table 'rational?)
		(insert-to-global-table 'zero?)
		(insert-to-global-table 'denominator)
		(insert-to-global-table 'numerator)
		(insert-to-global-table 'string-length)
		(insert-to-global-table 'vector-length)
		(insert-to-global-table 'vector)
		(insert-to-global-table 'string-ref)
		(insert-to-global-table 'vector-ref)
		(insert-to-global-table 'string-set!)
		(insert-to-global-table 'vector-set!)
		(insert-to-global-table 'set-car!)
		(insert-to-global-table 'set-cdr!)
		(insert-to-global-table 'integer->char)
		(insert-to-global-table 'char->integer)
		(insert-to-global-table 'remainder)
		(insert-to-global-table 'make-string)
		(insert-to-global-table 'make-vector)
		(insert-to-global-table '>)
		(insert-to-global-table '=)
		(insert-to-global-table 'apply)
		(insert-to-global-table 'symbol->string)
		(insert-to-global-table 'string->symbol)
		(insert-to-global-table 'eq?)
		)))

(define code-gen-fvar 
    (lambda (fvar)
        (begin (insert-to-global-table fvar)
			(string-append "MOV(R0, IND(" (number->string (lookup fvar global-table)) "));\n"))))
			
(define insert-to-global-table
	(lambda (fvar)
		(if (equal? (lookup fvar global-table) #f) 
			(set! global-table (append global-table `((,(global-table-address) ,fvar)))))))		

;;; returns the string that builds the constant table in CISC code.
(define generate-constant-table
	(lambda () 
		(letrec ((generate-table
					(lambda (cells address)
						(if (null? cells) "" 
							(string-append "MOV(IND(" (number->string address)
									   "), IMM(" (if (number? (car cells)) (number->string (car cells)) (car cells)) "));\n"
									   (generate-table (cdr cells) (+ address 1)))))
				))
		(generate-table (apply append (map caddr constant-table)) 1000))))
		

;;; returns the address at which the next free variable will be. 
;;; the global table starts 1 cell after the end of the constant table.
(define global-table-address 
		(lambda () 
			(+ (last-const-addr) (last-const-length) (length global-table) 1)))
        
;;; works on both the constant table and the global table.
(define lookup 
    (lambda (element table)
        (cond ((null? table) #f)
              ((equal? element (cadar table)) (caar table))
              (else (lookup element (cdr table))))
    ))

(define prologue (lambda ()
        (string-append 
				"#include <stdio.h>\n" 
				"#include <stdlib.h>\n"  
				"#define DO_SHOW 1\n"
				"#include \"cisc.h\"\n"
				"int main(){\n" 
				"START_MACHINE;\n"
				"MOV(IND(999), IMM(0));\n"
				"ADD(IND(0),IMM(" (number->string (global-table-address)) "));\n"
				"JUMP(CONTINUE);\n"
				"#include \"char.lib\"\n" 
				"#include \"io.lib\"\n" 
				"#include \"math.lib\"\n" 
				"#include \"string.lib\"\n" 
				"#include \"system.lib\"\n" 
				"#include \"scheme.lib\"\n"
				(file->string "BuiltInFuncs.c")
				"CONTINUE:\n"
				"LmakeCarClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LcarBody));\n"
				"MOV(IND(" (number->string(lookup 'car global-table)) "),R0);\n"
				"LmakeCdrClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LcdrBody));\n"
				"MOV(IND(" (number->string(lookup 'cdr global-table))"),R0);\n"
				"LmakeConsClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LconsBody));\n"
				"MOV(IND(" (number->string(lookup 'cons global-table))"),R0);\n"
				"LmakePlusClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LplusBody));\n"
				"MOV(IND(" (number->string(lookup '+ global-table))"),R0);\n"
				"LmakeDivClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LdivBody));\n"
				"MOV(IND(" (number->string(lookup '/ global-table))"),R0);\n"
				"LmakeMinusClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LminusBody));\n"
				"MOV(IND(" (number->string(lookup '- global-table))"),R0);\n"
				"LmakeMulClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LmulBody));\n"
				"MOV(IND(" (number->string(lookup '* global-table))"),R0);\n"
				"LmakecharClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LcharBody));\n"
				"MOV(IND(" (number->string(lookup 'char? global-table))"),R0);\n"
				"LmakebooleanClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LbooleanBody));\n"
				"MOV(IND(" (number->string(lookup 'boolean? global-table))"),R0);\n"
				"LmakeintegerClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LintegerBody));\n"
				"MOV(IND(" (number->string(lookup 'integer? global-table))"),R0);\n"
				"LmakenullClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LnullBody));\n"
				"MOV(IND(" (number->string(lookup 'null? global-table))"),R0);\n"
				"LmakepairClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LpairBody));\n"
				"MOV(IND(" (number->string(lookup 'pair? global-table))"),R0);\n"
				"LmakeprocedureClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LprocedureBody));\n"
				"MOV(IND(" (number->string(lookup 'procedure? global-table))"),R0);\n"
				"LmakestringClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LstringBody));\n"
				"MOV(IND(" (number->string(lookup 'string? global-table))"),R0);\n"
				"LmakesymbolClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LsymbolBody));\n"
				"MOV(IND(" (number->string(lookup 'symbol? global-table))"),R0);\n"
				"LmakeisvectorClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LisvectorBody));\n"
				"MOV(IND(" (number->string(lookup 'vector? global-table))"),R0);\n"
				"LmakenumberClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LnumberBody));\n"
				"MOV(IND(" (number->string(lookup 'number? global-table))"),R0);\n"
				"LmakerationalClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LrationalBody));\n"
				"MOV(IND(" (number->string(lookup 'rational? global-table))"),R0);\n"
				"LmakezeroClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LzeroBody));\n"
				"MOV(IND(" (number->string(lookup 'zero? global-table))"),R0);\n"
				"LmakedenominatorClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LdenominatorBody));\n"
				"MOV(IND(" (number->string(lookup 'denominator global-table))"),R0);\n"
				"LmakenumeratorClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LnumeratorBody));\n"
				"MOV(IND(" (number->string(lookup 'numerator global-table))"),R0);\n"
				"LmakestringlengthClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LstringlengthBody));\n"
				"MOV(IND(" (number->string(lookup 'string-length global-table))"),R0);\n"
				"LmakevectorlengthClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LvectorlengthBody));\n"
				"MOV(IND(" (number->string(lookup 'vector-length global-table))"),R0);\n"
				"LmakevectorClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LvectorBody));\n"
				"MOV(IND(" (number->string(lookup 'vector global-table))"),R0);\n"
				"LmakestringrefClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LstringrefBody));\n"
				"MOV(IND(" (number->string(lookup 'string-ref global-table))"),R0);\n"
				"LmakevectorrefClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LvectorrefBody));\n"
				"MOV(IND(" (number->string(lookup 'vector-ref global-table))"),R0);\n"
				"LmakestringsetClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LstringsetBody));\n"
				"MOV(IND(" (number->string(lookup 'string-set! global-table))"),R0);\n"
				"LmakevectorsetClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LvectorsetBody));\n"
				"MOV(IND(" (number->string(lookup 'vector-set! global-table))"),R0);\n"
				"LmakesetcarClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LsetcarBody));\n"
				"MOV(IND(" (number->string(lookup 'set-car! global-table))"),R0);\n"
				"LmakesetcdrClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LsetcdrBody));\n"
				"MOV(IND(" (number->string(lookup 'set-cdr! global-table))"),R0);\n"
				"LmakeintegertocharClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LintegertocharBody));\n"
				"MOV(IND(" (number->string(lookup 'integer->char global-table))"),R0);\n"
				"LmakechartointegerClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LchartointegerBody));\n"
				"MOV(IND(" (number->string(lookup 'char->integer global-table))"),R0);\n"
				"LmakeremainderClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LremainderBody));\n"
				"MOV(IND(" (number->string(lookup 'remainder global-table))"),R0);\n"
				"LmakemakestringClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LmakestringBody));\n"
				"MOV(IND(" (number->string(lookup 'make-string global-table))"),R0);\n"
				"LmakemakevectorClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LmakevectorBody));\n"
				"MOV(IND(" (number->string(lookup 'make-vector global-table))"),R0);\n"
				"LmakebiggerClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LbiggerBody));\n"
				"MOV(IND(" (number->string(lookup '> global-table))"),R0);\n"
				"LmakeequalClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LequalBody));\n"
				"MOV(IND(" (number->string(lookup '= global-table))"),R0);\n"
				"LmakeapplyClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LapplyBody));\n"
				"MOV(IND(" (number->string(lookup 'apply global-table))"),R0);\n"
				"LmakesymbolTostringClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LsymbolTostringBody));\n"
				"MOV(IND(" (number->string(lookup 'symbol->string global-table))"),R0);\n"
				"LmakestringTosymbolClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LstringTosymbolBody));\n"
				"MOV(IND(" (number->string(lookup 'string->symbol global-table))"),R0);\n"
				"LmakeeqClos:\n"
				"PUSH(IMM(3));\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(INDD(R0, 0), IMM(T_CLOSURE));\n"
				"MOV(INDD(R0, 1), IMM(123));\n"
				"MOV(INDD(R0, 2), LABEL(LeqBody));\n"
				"MOV(IND(" (number->string(lookup 'eq? global-table))"),R0);\n"
		)))

(define epilogue
		(string-append
				"END:\n"
				"L_error_lambda_args_count:\n"
				"L_error_cannot_apply_non_clos:\n"
				"STOP_MACHINE;\n"
				"return 0;\n"
				"}\n"
		))
		
(define code-gen
    (lambda (pes env-counter param-counter) 
        (cond ((is-x? pes 'if3) (code-gen-if3 (cdr pes) env-counter param-counter))
              ((is-x? pes 'seq) (code-gen-seq (cadr pes) env-counter param-counter))
              ((is-x? pes 'or) (code-gen-or (cadr pes) env-counter param-counter))
              
              ((is-x? pes 'applic) (code-gen-applic (cdr pes) env-counter param-counter))
              ((is-x? pes 'tc-applic) (code-gen-tc-applic (cdr pes) env-counter param-counter))
              
              ((is-x? pes 'pvar) (code-gen-pvar (cdr pes)))
              ((is-x? pes 'fvar) (code-gen-fvar (cadr pes)))
              ((is-x? pes 'set) (code-gen-set (cdr pes) env-counter param-counter))
              ((is-x? pes 'box-get) (code-gen-box-get (cadr pes) env-counter param-counter))
              ((is-x? pes 'bvar) (code-gen-bvar (cdr pes)))
              ((is-x? pes 'def) (code-gen-def (cdr pes) env-counter param-counter))
              ((is-x? pes 'const) (code-gen-const (cadr pes)))
              ((is-x? pes 'box) (code-gen-box (cadr pes) env-counter param-counter))
              ((is-x? pes 'box-set) (code-gen-box-set (cdr pes) env-counter param-counter))
              
              ((is-x? pes 'lambda-simple) (code-gen-lambda-simple (cdr pes) env-counter param-counter))
              ((is-x? pes 'lambda-opt) (code-gen-lambda-opt (cdr pes) env-counter param-counter))
              ((is-x? pes 'lambda-var) (code-gen-lambda-var (cdr pes) env-counter param-counter))
              (else (error 'code-gen "the expression isn't valid!" pes))
        )))

;;; adds print commands to the end of the code generation.
;;; no need to call this function anywhere other than in "compile-scheme-file".
(define code-gen-with-print
	(lambda (x)
		(let ((cont_label (make-label "CONTINUE_")))
		(string-append	(code-gen x 0 0)
						"CMP(R0, IMM(SOB_VOID));\n"
						"JUMP_EQ(" cont_label ");\n"
						"PUSH(R0);\n"
						"CALL(WRITE_SOB);\n"
						"DROP(1);\n"
						"CALL(NEWLINE);\n"
						cont_label ":\n"))))
          
;;;;;;; should we reset the global variables each time? ;;;;;;;;;			
(define compile-scheme-file
     (lambda (source target) 
         (let* ((scheme-code 
                 (append
                 (map (lambda (x) (full x)) (convert-string <sexpr>  (file->string "scheme-funcs.scm")))
                 (map (lambda (x) (full x)) (convert-string <sexpr> (file->string source)))
 	))
 		
 		(cisc-code (begin
 							(build-constant-table scheme-code) 
 							(add-built-in-funcs)
 							(apply string-append 
 								(map code-gen-with-print scheme-code)))))
                    (begin (string->file (string-append (prologue) (generate-constant-table) cisc-code epilogue) target)
                       (set! constant-table `((1000 ,(void) ("T_VOID"))

             (1001 () ("T_NIL"))

             (1002 #f ("T_BOOL" 0))

             (1004 #t ("T_BOOL" 1)))) (set! global-table '()))                                                                                        )))