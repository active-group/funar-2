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
#;(define animal
    (signature (mixed dillo parrot)))

;; Ein Gürteltier hat folgende Eigenschaft:
;; - lebendig oder tot? -UND-
;; - Gewicht
(define-record dillo
  make-dillo
  (dillo-alive? boolean)
  (dillo-weight number))

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
    (make-dillo (dillo-alive? dillo)
                (+ amount (dillo-weight dillo)))))









