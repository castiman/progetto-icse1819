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

(deffunction controllo-si-no (?domanda)
   (bind ?risposta (poni-domanda ?domanda si no s n yes y ))
   (if (or (eq ?risposta si) (eq ?risposta s) (eq ?risposta y) (eq ?risposta yes))
        then si 
    else no))

(deffunction schema-analisi (?risposta)
   (if (or (eq ?risposta si) (eq ?risposta s))
        then (bind ?*visualizza-analisi* TRUE) 
    else (bind ?*visualizza-analisi* FALSE) ))

(deffunction scelta-pianta (?domanda)
    (bind ?risposta (poni-domanda ?domanda 1 2 3 4 5 6))
    (if (eq ?risposta 1)
        then carote
    else (if (eq ?risposta 2)
        then cavoli
    else (if (eq ?risposta 3)
        then ciliegie
    else (if (eq ?risposta 4)
        then banane
    else (if (eq ?risposta 5)
        then pomodori
    else (if (eq ?risposta 6)
        then patate 
    else nil))))))
    )

; Calcola il livello di confidenza e determina la diagnosi basata sulla soglia stabilita
(deffunction diagnosi-pianta (?nome-pianta ?malattia-associata ?soglia)
    (bind ?peso 1)
    (if ?*visualizza-analisi* 
        then
        (printout t "" crlf 
                    "Calcolo il livello di confidenza totale per "
                    ?malattia-associata "..." crlf))
    (do-for-all-facts ((?g dettagli-sintomi)) 
        (and 
            (eq ?g:presenza-sintomo si)
            (eq ?g:nome-pianta ?nome-pianta)
            (eq ?g:malattia-associata ?malattia-associata))
        (if ?*visualizza-analisi*
            then
            (printout t ?g:nome-sintomo " livello confidenza: " ?g:peso crlf)
            (printout t "Calcolo: " ?peso " * (1 - "  ?g:peso ")" crlf))
        (bind ?peso (* ?peso (- 1 ?g:peso)))
        (if ?*visualizza-analisi*
            then
            (printout t "= " ?peso crlf "" crlf)))
    (if ?*visualizza-analisi*
        then
        (printout t "--------------------------------------------------" crlf
        "Livello di confidenza totale per " ?malattia-associata " (1 - " ?peso ")"crlf))
    (bind ?peso (- 1 ?peso))
    (if ?*visualizza-analisi*
        then
        (printout t "= " ?peso crlf 
        "E soglia" crlf "= "?soglia crlf
        "--------------------------------------------------" crlf "" crlf))
    (assert (dettagli-malattia (nome-malattia ?malattia-associata)
                            (nome-pianta ?nome-pianta)
                            (peso ?peso)))
    (if (> ?peso ?soglia)
        then TRUE))

