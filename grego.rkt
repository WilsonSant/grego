#lang racket
(require "parser.rkt")
(require syntax/strip-context)


(define (filter-empty-string list)
  (filter non-empty-string? list))

; (: grego-read (-> Input-Port  Sexp))
 (define (grego-read input)
     (syntax->datum (grego-read-syntax #f input)))

;(: parse-lines (-> (Lista String) Handle-String))
(define (parse-lines lines)
  (cond [(null? lines) '()]
        [else (cons (parser (string-trim (car lines))) (parse-lines (cdr lines)))]))

;(: grego-read-syntax (-> Boolean Input-Port Sexp))
 (define (grego-read-syntax path port)
         (define src-lines (port->lines port))
         (define result (filter-empty-string (parse-lines (filter-empty-string src-lines))))
         (datum->syntax #f `(module grego racket ,@result)))

 (provide (rename-out [grego-read read]
                      [grego-read-syntax read-syntax]))