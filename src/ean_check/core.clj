(ns ean-check.core
  (:gen-class))

(def ean2 "9788850322442")
(def ean  "9781932394283")


(defn str-to-long [x]
  (Long/parseLong (apply str (filter #(Character/isDigit %) x))))

(defn valoriInPosizione [s func]
    (map #(str-to-long (str %)) (keep-indexed #(if (func %1) %2) (reverse s))))

(defn valoriInPosizionePari [s]
  (valoriInPosizione s even?))

(defn valoriInPosizioneDispari [s]
  (valoriInPosizione s odd?))

(defn moltiplica-per [seq n]
  (map #(* n %) seq ))

(defn sommaIndiciPari [s]
  (reduce + (moltiplica-per (valoriInPosizionePari s) 3)  ))

(defn sommaIndiciDispari [s]
  (reduce + (valoriInPosizioneDispari s)  ))

(defn decina-superiore [aLong]
  (long (* 10 (Math/ceil (/ aLong 10)))))

(defn cifraParita [ean]
  (let [somma (+ (sommaIndiciPari ean) (sommaIndiciDispari ean))
        decinaSuperiore (decina-superiore somma)]
        (- decinaSuperiore somma) ))

(defn togliUltimaCifra [ean]
  (subs ean 0 12))

(defn valido? [ean]
  "torna true se il codice ean è valido"
  (let [ean2 (togliUltimaCifra ean)
        parita (cifraParita ean2)]
      (= (str-to-long (str (get ean 12))) parita)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println ean " è valido? " (valido? ean)) )
