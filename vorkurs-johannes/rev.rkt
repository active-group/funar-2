;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname rev) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
;;#lang deinprogramm/sdp

;; Listen erstellen:
;(define list4 (list 5 3 6 4))

;; first und rest funktionieren genauso

; Liste umdrehen
(: rev ((list-of %a) -> (list-of %a)))

(check-expect (rev (list 1 2 3 4))
              (list 4 3 2 1))

(define rev
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (append-element
        (rev (rest list)) ; 4 3 2
        (first list))))))  ; 1 --> fehlt: hinten an Liste anhängen

;; Laufzeit:
;; 1 + 2 + 3 + ... + n = n * (n+1) / 2 in O(n^2)

; Element hinten an Liste anhängen
(: append-element ((list-of %a) %a -> (list-of %a)))

(check-expect (append-element (list 1 2 3) 4)
              (list 1 2 3 4))

(define append-element
  (lambda (ls a)
    (cond
      ((empty? ls) (list a))
      ((cons? ls)
       (cons
        (first ls)  ; 1
        (append-element (rest ls) a))))))

; Liste umdrehen, mit Zwischenergebnis
(: rev* ((list-of %a) (list-of %a) -> (list-of %a)))

(check-expect (rev* (list 1 2 3) empty)
              (list 3 2 1))

(define rev*
  ; Schleifeninvariante
  ; acc ist Liste aller bereits gesehenen Elemente in umg. Reihenfolge
  (lambda (ls acc) ;; Akkumulator
    (cond
      ((empty? ls) acc) ; 4 3 2 1
      ((cons? ls)       ; ls ist (list 4), acc ist (list 3 2 1)
       ; v    rev* steht in sog. tail-call-Position
       (rev* (rest ls)
             (cons (first ls) acc)))))) ; benötigen keinen zus. Speicher


; gänginge Repräsentation für Kontext zur Laufzeit:
; Stack
; gängige Runtimes: Stack feste Größe, klein im Vergleich zum Hauptspeicher

; JVM: auch Tail-Calls verbrauchen Speicher
; => auf JVM-Sprachen gibt es spezielle Konstrukte für Endrekursion
; Kotlin: tailrec
; Scala: @tailrec
; Clojure: loop

; Übung: bel. Funktion endrekursiv schreiben!

