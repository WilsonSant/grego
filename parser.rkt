#lang typed/racket

(define-type (Lista A) (U (Listof A) '()))

(define-type (Handle-String) (U String Void))

(define-struct letra
  ([maiusculo : Char] [minusculo : Char] [nome : String]))

(: string->letra (-> String String String letra))
(define (string->letra maiusculo minusculo nome)
  (make-letra (string-ref maiusculo 0) (string-ref minusculo 0) nome))

(define alfabeto
  (list (string->letra "Α" "α" "άλφα")
        (string->letra "Β" "β" "βήτασ")
        (string->letra "Γ" "γ" "γάμμα")
        (string->letra "Δ" "δ" "δέλτα")
        (string->letra "Ε" "ε" "έψιλο")
        (string->letra "Ζ" "ζ" "ζήτα")
        (string->letra "Η" "η" "και")
        (string->letra "Θ" "θ" "θήτα")
        (string->letra "Ι" "ι" "ιώτα")
        (string->letra "Κ" "κ" "κάπα")
        (string->letra "Λ" "λ" "λάμδα")
        (string->letra "Μ" "μ" "μι")
        (string->letra "Ν" "ν" "αρ")
        (string->letra "Ξ" "ξ" "ξι")
        (string->letra "Ο" "ο" "όμικρο")
        (string->letra "Π" "π" "πι")
        (string->letra "Ρ" "ρ" "ρο")
        (string->letra "Σ" "σ,ς" "σίγμα")
        (string->letra "Τ" "τ" "ταυ")
        (string->letra "Υ" "υ" "ύψιλο")
        (string->letra "Φ" "φ" "φι")
        (string->letra "Χ" "χ" "χι")
        (string->letra "Ψ" "ψ" "ψι")
        (string->letra "Ω" "ω" "ωμέγα")))

(: find-letra (->  (Lista letra) Char (U String Void)))
(define (find-letra lista char)
  (cond
    [(null? lista) (error "Erro! Aceito apenas letras gregas!")]
    [(or (eq?  (letra-maiusculo (car lista)) char)
         (eq? (letra-minusculo (car lista)) char)) (letra-nome (car lista))]
    [else (find-letra (cdr lista) char)]))

(: parser (-> String Handle-String))
(define (parser input)
   (find-letra alfabeto (string-ref input 0)))

(provide parser Lista Handle-String)