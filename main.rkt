#lang racket/base

(require (for-syntax syntax/parse racket/base)
         syntax/parse/define
         racket/dict
         racket/string
         )

(provide #%top
         #%top-interaction ; stfu
         #%app
         true
         false
         defun
         record
         @
         function
         ++
         (rename-out [rec-mb #%module-begin]
                     [rec-datum #%datum]
                     [rec-if if]
                     [rec-let let]
                     [rec-zero? zero?]
                     [rec-plus +]
                     [rec-empty? empty?]
                     )
         )

(struct fn (name body)
  #:property prop:procedure (struct-field-index body)
  #:property prop:custom-write (lambda (v p m)
                                 (write (list 'function (fn-name v)) p)))

(struct ttrue  () #:property prop:custom-write (lambda (v p m) (display "true" p)))
(struct tfalse () #:property prop:custom-write (lambda (v p m) (display "false" p)))

(define true (ttrue))
(define false (tfalse))

(module+ test
  (require rackunit))

(module reader syntax/module-reader
  RecImpl)

(define-syntax (rec-mb stx)
  (syntax-parse stx
    [(_ (defun stuff ...) ... e:expr)
     #'(#%module-begin
        (defun stuff ...) ...
        e)]))

(define-syntax (rec-datum stx)
  (syntax-parse stx
    [(_ . n:number) #'(#%datum . n)]
    [(_ . s:str) #'(#%datum . s)]))

(define-syntax (defun stx)
  (syntax-parse stx
    [(_ (id:id x:id) body:expr)
     #'(define id (fn 'id (lambda (x) body)))]))

(define-syntax (rec-if stx)
  (syntax-parse stx
    [(_ c:expr t:expr f:expr)
     #'(let ([c* c])
         (cond
           [(eq? c* true) t]
           [(eq? c* false) f]
           [else (error 'if "condition used without boolean, got ~v" c*)]))]))

(define-syntax (rec-let stx)
  (syntax-parse stx
    [(_ ((x:id e:expr)) body:expr)
     #'(let ((x e)) body)]))

(define-syntax (record stx)
  (syntax-parse stx
    [(_ (s:str e:expr) ...)
     #'(list 'record (cons s e) ...)]))

(define-syntax (@ stx)
  (syntax-parse stx
    [(_ r:expr e:str)
     #'(let ([r* r])
         (if (list? r*)
             (dict-ref (cdr r*) e (lambda () (error '@ "~a not found in ~a" e r*)))
             (error '@ "not a record, got ~a" r*)))]))

(define-simple-macro (rec-zero? e:expr)
  (let ([e* e])
    (check-number 'zero? e*)
    (zero? e*)))

(define-simple-macro (rec-plus a:expr b:expr)
  (let ([a* a]
        [b* b])
    (check-number '+ a*)
    (check-number '+ b*)
    (+ a* b*)))

(define-simple-macro (function id:id)
  (if (procedure? id)
      id
      (error 'function "~v is not a function" id)))

(define-simple-macro (rec-empty? e:expr)
  (let ([e* e])
    (check-string 'empty? e*)
    (not (non-empty-string? e*))))

(define-simple-macro (++ a:expr b:expr)
  (let ([a* a]
        [b* b])
    (check-string '+ a*)
    (check-string '+ b*)
    (string-append a* b*)))

(define (check-number fn v)
  (unless (number? v) (error fn "~v is not a number" v)))

(define (check-string fn v)
  (unless (string? v) (error fn "~v is not a string" v)))
