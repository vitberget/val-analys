(ns se.vitberget.val.val
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]))

(def url "https://resultat.val.se/assets/valdata/RD_00_P_mandat.json")
(def filen "/home/k/src/val-analys/val.json")

(defn dirty-string->number [string]
  (->> string
       (filter (fn [c] (Character/isDigit c)))
       (apply str)
       (read-string)))

(defn procent
  [namnare taljare]
  (->> (/ (* 100 namnare) taljare)
       (float)
       (format "%.2f%%")))

(defn valet []
  (let [v (json/read (io/reader filen)
                     :key-fn keyword)
        antalRostberattigade (->> v
                                  (:antalRostberattigade)
                                  (dirty-string->number))
        rosterPaverkaMandat (->> v
                                 (:rosterPaverkaMandat)
                                 (:antalRoster)
                                 (dirty-string->number))

        rosterEjPaverkaMandat (->> v
                                   (:rosterEjPaverkaMandat)
                                   (:antalRoster)
                                   (dirty-string->number))

        soffliggare (- antalRostberattigade rosterPaverkaMandat rosterEjPaverkaMandat)



        partier (->> v
                     (:rosterPaverkaMandat)
                     (:partiroster))

        ogiltiga (->> v
                      (:rosterEjPaverkaMandat)
                      (:partiroster))

        items (-> (concat partier ogiltiga)
                  (conj {:antalRoster      (str soffliggare)
                         :partibeteckning  "Soffliggare"
                         :partiforkortning "X"}))
        items (->> items
                   (map (fn [b] (-> b
                                    (select-keys [:antalRoster :partibeteckning :partiforkortning])
                                    (update :antalRoster dirty-string->number))))
                   (map (fn [b] (assoc b :procent (procent (:antalRoster b) antalRostberattigade))))
                   (sort (fn [a b] (> (:antalRoster a) (:antalRoster b)))))

        ]
    (for [p items]
      (println (:procent p) (:partibeteckning p)))
    ))

(valet)