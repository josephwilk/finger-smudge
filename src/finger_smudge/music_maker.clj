(ns finger-smudge.music-maker
  (:require [dynne.sound :as sound]
            [clojure.java.io :as io]
            [primitive-math :as p])
  (:use [overtone.live])
  (:import [javax.sound.sampled
            AudioFileFormat$Type
            AudioFormat
            AudioFormat$Encoding
            AudioInputStream
            AudioSystem
            Clip]
           [dynne.sound.impl ISound MutableDouble BufferPosition]))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(defn noise-fn []
  (let [silence (take (rand-int 1024) (cycle [0.0]))
        sound-data (concat [(rand -1) (rand 1)] silence)]
    (fn ^double [^double x]
      (rand-nth sound-data))))

(defn and-we-are-the-dreamer-of-the-dreams [duration]
  (let [channels        2
        bits-per-sample 8
        frames          44100
        bytes-per-frame (* frames bits-per-sample channels)
        candidate (str "resources/generations/" (uuid) ".wav")]
    (-> (sound/sound duration (noise-fn))
        (sound/save  candidate 44100))
    candidate))


(defsynth siney [note 0 amp 0 out-bus 0]
  (let [freq (midicps note)
        src (sin-osc freq)
        src (* src amp)]
    (out out-bus src)))

(defonce synth-notes {:a  (siney (note :A3))
                      :a# (siney (note :A#3))
                      :b  (siney (note :B3))
                      :c  (siney (note :C4))
                      :c# (siney (note :C#4))
                      :d  (siney (note :D4))
                      :d# (siney (note :D#4))
                      :e  (siney (note :E4))
                      :f  (siney (note :F4))
                      :f# (siney (note :F#4))
                      :g  (siney (note :G4))
                      :g# (siney (note :G#4))})

(ctl (:a synth-notes) :amp 0.0)

(comment
  (and-we-are-the-dreamer-of-the-dreams 120)
  )
