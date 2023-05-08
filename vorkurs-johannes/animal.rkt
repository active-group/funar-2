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
      ((string=? pet "dog") #t) ;; string-equals-p
      ((string=? pet "cat") #t)
      ((string=? pet "snake") #f)))) ;; conditional

(define hour (signature (integer-from-to 0 23)))
(define minute (signature (integer-from-to 0 59)))

;; Uhrzeit _besteht aus_/_hat folgende Eigenschaften_:
;; -> zusammengesetzte Daten
;; - Stunde -UND-
;; - Minute
(define-record time  ;; Record definieren
  make-time ;; Konstruktor (Name frei wählbar)
  ;; Bestandteile:
  (time-hour hour) ;; Selektor/Accessor
  (time-minute minute))

;; Signaturen
(: make-time (hour minute -> time))
(: time-hour (time -> hour))
(: time-minute (time -> minute))

(define time1 (make-time 11 23))
(define time2 (make-time 14 11))

;; Minuten seit Mitternacht berechnen
(: msm (time -> natural))

(check-expect (msm time1)
              683)
(check-expect (msm time2)
              851)

;; Gerüst
#;(define msm
    (lambda (time)
      ...))

;; Schablone
#;(define msm
    (lambda (time)
      ... (time-hour time) ...
      ... (time-minute time) ...))

(define msm
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))

;; Aus Minuten seit Mitternacht die Uhrzeit berechnen
(: msm->time (natural -> time))

(check-expect (msm->time 683)
              time1)
(check-expect (msm->time 851)
              time2)

(define msm->time
  (lambda (msm)
    ;; Integerdivision mit Rest
    (make-time (quotient msm 60)
               (remainder msm 60))))


;;;;; Tiere auf dem texanischen Highway

;; Ein Tier ist eins der Folgenden:
;; - Gürteltier -ODER-
;; - Papagei
(define animal
  (signature (mixed dillo parrot)))

;; Ein Gürteltier hat folgende Eigenschaft:
;; - lebendig oder tot? -UND-
;; - Gewicht
(define-record dillo
  make-dillo
  dillo? ;; nachträglich hinzugefügt
  (dillo-alive? boolean)
  (dillo-weight number))

(: dillo? (any -> boolean))

;; lebendiger Dillo
(define dillo1 (make-dillo #t 10))
;; toter Dillo
(define dillo2 (make-dillo #f 8))

;; Gürteltiere überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 10))
(check-expect (run-over-dillo dillo2)
              dillo2)

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f
                (dillo-weight dillo))))

;; Gürteltiere füttern
(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 5)
              (make-dillo #t 15))
(check-expect (feed-dillo dillo2 5)
              dillo2)

(define feed-dillo
  (lambda (dillo amount)
    (define alive? (dillo-alive? dillo))
    (define weight (dillo-weight dillo))
    (make-dillo alive?
                (if alive?
                    (+ amount weight)
                    weight))))

;; Ein Papagei hat folgende Eigenschaften:
;; - ein Satz -UND-
;; - ein Gewicht
(define-record parrot
  make-parrot
  parrot?
  (parrot-sentence string)
  (parrot-weight number))

;; Begrüßungspapagei
(define parrot1 (make-parrot "Hallo" 1))
;; Verabschiedungspapagei
(define parrot2 (make-parrot "Ciao" 2))

;; Papageien überfahren
(: run-over-parrot (parrot -> parrot))

(check-expect (run-over-parrot parrot1)
              (make-parrot "" 1))
(check-expect (run-over-parrot parrot2)
              (make-parrot "" 2))

(define run-over-parrot
  (lambda (parrot)
    (make-parrot "" (parrot-weight parrot))))

;; Tier überfahren
(: run-over-animal (animal -> animal))

(check-expect (run-over-animal dillo1)
              (run-over-dillo dillo1))
(check-expect (run-over-animal dillo2)
              (run-over-dillo dillo2))
(check-expect (run-over-animal parrot1)
              (run-over-parrot parrot1))
(check-expect (run-over-animal parrot2)
              (run-over-parrot parrot2))

(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) (run-over-dillo animal))
      ((parrot? animal) (run-over-parrot animal))
      ... ... )))

(define pet-animal (...

#|
interface Animal { Animal runOver();  Animal feed(int amount);  pet(); }

class Dillo implements Animal { @Override Animal runOver(); }

class Parrot implements Animal { ... }

class Snake implements Animal { ... runOver(); }

OOP: neue Fälle einfach, neue Operationen "schwer"
FP: neue Fälle schwer, neue Operationen einfach

|#
