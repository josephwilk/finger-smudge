(ns finger-smudge.music-maker
  (:require [dynne.sound :as sound]
            [clojure.java.io :as io]
            [primitive-math :as p])
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

(comment
  (and-we-are-the-dreamer-of-the-dreams 120)
  )
