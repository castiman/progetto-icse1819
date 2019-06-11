; Definizione variabili globali
(defglobal ?*visualizza-analisi* = NIL) 
(defglobal ?*file-cura* = cura.txt)

; Definizione template
(deftemplate dettagli-sintomi
    (slot nome-sintomo)
    (slot nome-pianta)
    (slot malattia-associata)
    (slot presenza-sintomo (default no))
    (slot peso (default 0)))

(deftemplate dettagli-malattia
    (slot nome-malattia )
    (slot nome-pianta )
    (slot peso (default 0)))

; Definizione funzioni
(deffunction poni-domanda (?domanda $?valori-permessi)
   (printout t ?domanda)
   (bind ?risposta (read))
   (if (lexemep ?risposta) 
       then (bind ?risposta (lowcase ?risposta)))
   (while (not (member ?risposta ?valori-permessi)) do
      (printout t ?domanda)
      (bind ?risposta (read))
      (if (lexemep ?risposta) 
          then (bind ?risposta (lowcase ?risposta))))
   ?risposta)

(deftemplate miglior-diagnosi
    (slot testo)
    (slot nome-pianta)
    (slot malattia-associata))
	

(deffunction confronta-peso (?fatto1 ?fatto2)
   (> (fact-slot-value ?fatto1 peso) (fact-slot-value ?fatto2 peso)))
		
 (deffunction trova-massimo (?template ?predicato)
   (bind ?massimo FALSE)
   (do-for-all-facts ((?f ?template)) TRUE
      (if (or (not ?massimo) (funcall ?predicato ?f ?massimo))
         then
         (bind ?massimo ?f)))
   (return ?massimo))
   
(defrule trova-massimo
   (declare (salience 10))
   (assicurati)
   (not (found))
   (miglior-diagnosi (testo ?testo1)
                    (nome-pianta ?nome-pianta1)
                    (malattia-associata ?malattia-associata1))
   =>
   (bind ?fatto (trova-massimo dettagli-malattia confronta-peso))
   (if ?fatto
      then
	  (assert (found))
	  (assert (diagnosi ?testo1 ?nome-pianta1 ?malattia-associata1))
      ;(printout t "Il fatto " (fact-slot-value ?fatto nome-malattia) " e' il massimo" crlf)
	  ))