(ns fingersmudge.core
  (:require [clojure.java.shell :as shell]
            [fingersmudge.musicbrainz :as musicbrainz])
  (:gen-class))

;;https://acoustid.org/chromaprint
(def fingerprinter "fpcalc")
(def tracks ["resources/136_think.wav"])

(defn fingerprinter-data [data]
  (let [lines (clojure.string/split data #"\n")
        keyvalues (map #(re-matches #"^(.+)=(.+)$" %1) lines)
        fingerprint-data (reduce (fn [acc [_ k v]]
                                   (assoc acc (keyword (clojure.string/lower-case k)) v)) {} keyvalues)]
    fingerprint-data))

(defn generate-fingerprint [file]
  (let [r (shell/sh fingerprinter file)
        exit-code (:exit r)
        std-out (:out r)]
    (if (= exit-code 0)
      (let [data (fingerprinter-data std-out)]
        data)
      {})))

(defn fitness [duration candidate-fingerprint]
  (let [matches (musicbrainz/lookup duration candidate-fingerprint)]
    (:score matches)))

(defn generate-candidate [] "resources/136_think.wav")

(defn build-tracks [duration target-fingerprint]
  (println "seconds" duration)
  (loop [candidate (generate-candidate)]
    (let [candidate-fingerprint (generate-fingerprint candidate)
          score (fitness duration (:fingerprint candidate-fingerprint))]
      (println "Candidate match: " score)

      (if (and
           candidate
           (< score 0.8))
        (recur (generate-candidate))
        (println "Match found: " score)))))

(defn -main [& args]
  (let [data (generate-fingerprint (first tracks))]
    (println "Seed:" (:file data))
    (build-tracks (data :duration) (data :fingerprint))))

(-main)
