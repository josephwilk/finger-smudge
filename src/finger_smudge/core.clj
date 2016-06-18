(ns finger-smudge.core
  (:require [clojure.java.shell :as shell]
            [finger-smudge.musicbrainz :as musicbrainz]
            [finger-smudge.music-maker :as music-maker])
  (:gen-class))

;;https://acoustid.org/chromaprint
(def fingerprinter "fpcalc")
(def api-rate 1000)
(def tracks ["resources/136_think_mono.wav"
             "resources/136_think_8bits.wav"
             "resources/chipzel-only-human.wav"
             "resources/136_think.wav"])

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
    matches))

(defn generate-candidate [dur]
  (let [dur (Integer/parseInt dur)]
    (music-maker/and-we-are-the-dreamer-of-the-dreams dur)))

(defn build-tracks [duration target-fingerprint]
  (println "seconds" duration)
  (loop [candidate (generate-candidate duration)]
    (let [candidate-fingerprint (generate-fingerprint candidate)
          score-data (fitness duration (:fingerprint candidate-fingerprint))
          score (:score score-data)]
      (Thread/sleep api-rate)
      (println "Candidate match: " score)
      (if (and
           candidate
           score
           (< score 0.8))
        (recur (generate-candidate duration))
        (println score-data)))))

(defn -main [& args]
  (let [data (generate-fingerprint (first tracks))]
    (println "Seed:" (:file data))
    (build-tracks (data :duration) (data :fingerprint)))
  (System/exit 0))
