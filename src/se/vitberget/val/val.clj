(ns se.vitberget.val.val
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]))

(def url "https://resultat.val.se/assets/valdata/RD_00_P_mandat.json")
(def filen "/home/k/src/val-analys/val.json")

(defn dirty-string->number [input]
  (if (number? input)
    input
    (->> input
         (filter (fn [c] (Character/isDigit c)))
         (apply str)
         (read-string))))

(defn procent
  [namnare taljare]
  (->> (/ (* 100 namnare) taljare)
       (float)
       (format "%.2f%%")))

(defn valet []
  (let [val-data (json/read (io/reader url) :key-fn keyword)
        antalRostberattigade (->> val-data
                                  (:antalRostberattigadeIRaknadeValdistrikt)
                                  (dirty-string->number))
        soffliggare (- antalRostberattigade
                       (->> val-data
                            (:totaltAntalRoster)
                            (dirty-string->number)))
        items (as-> (concat (->> val-data
                                 (:rosterPaverkaMandat)
                                 (:partiroster))
                            (->> val-data
                                 (:rosterEjPaverkaMandat)
                                 (:partiroster))) $
                    (conj $ {:antalRoster      soffliggare
                             :partibeteckning  "Soffliggare"
                             :partiforkortning "X"})
                    (map (fn [b] (-> b
                                     (select-keys [:antalRoster
                                                   :partibeteckning
                                                   :partiforkortning])
                                     (update :antalRoster dirty-string->number)))
                         $)
                    (map (fn [b] (assoc b :procent
                                          (procent (:antalRoster b)
                                                   antalRostberattigade)))
                         $)
                    (sort (fn [a b] (> (:antalRoster a)
                                       (:antalRoster b)))
                          $))]
    (for [p items]
      (println (:procent p) (:partibeteckning p)))))

(valet)