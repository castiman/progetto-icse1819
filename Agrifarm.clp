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

