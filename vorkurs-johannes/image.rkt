;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname image) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
;;#lang deinprogramm/sdp/beginner

;; REPL - Read Eval Print Loop

;; Booleans: #t #f

(require deinprogramm/sdp/image)

;; https://docs.racket-lang.org/

;; https://github.com/active-group/funar-2

;; Dingen Namen geben
(define x 3)

(define circle1 (circle 50 "solid" "green"))
(define square1 (square 100 "outline" "red"))
(define star1 (star 50 "solid" "yellow"))

;; Badezimmer mit Kacheln auslegen

;; Bilder überlagern
;(define overlay1 (overlay star1 circle1))

;; übereinander
;(above star1 square1)

;; nebeneinander
;(beside circle1 circle1)

;; Kachel ist 2x2-Muster aus Bildern
(above
 (beside square1 star1)
 (beside star1 square1))

(above
 (beside square1 circle1)
 (beside circle1 square1))

;; Abstraktion:
;; - Unterschieden Namen geben
;; Funktionen dienen der Abstraktion

;; Deutsche Beamtenmethode
;; Konstruktionsanleitung!

;; 1. Kurzbeschreibung

;; Quadratisches Kachelmuster aus zwei Bildern bilden

;; 2. Signatur "Klammer + Doppelpunkt"

(: tile (image image -> image))





