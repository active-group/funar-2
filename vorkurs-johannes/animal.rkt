;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname animal) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
;;#lang deinprogramm/sdp/beginner

;; Tiere

;; Beschreiben die Domäne natürlichsprachlich:

;; Datendefinition (Aufzählung)

;; Haustier _ist eins der Folgenden_:
;; - Katze -ODER-
;; - Hund -ODER-
;; - Schlange

(define pet
  (signature (enum "cat" "dog" "snake")))

;; Ist ein Haustier niedlich?
(: cute? ;; cute-p (predicate)
   (pet -> boolean))

(check-expect (cute? "dog")
              #t)
(check-expect (cute? "cat")
              #t)
(check-expect (cute? "snake")
              #f)

;; Gerüst
#;(define cute?   ;; <- #; heißt: folgender Ausdruck ist auskommentiert
    (lambda (pet)
      ...))

;; Schablone
(define cute?
  (lambda (pet)
    (cond
      ;; besteht aus 3 Ausdrücken
      ;; einer für jedes Tier
      ;; -> Paare aus Bedingung und Wahrheitswert
      ((equals pet "dog") #t) ;; string-equals-p
      ((string=? pet "cat") #t)
      ((string=? pet "snake") #f)))) ;; conditional









