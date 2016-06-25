(ns finger-smudge.music-maker
  (:require [dynne.sound :as sound]
            [clojure.java.io :as io]
            [primitive-math :as p]
            [finger-smudge.fft :as fft]
            [mikera.image.core :as img]
            [mud.timing :as time]
            [clojure.core.matrix :as mat]
            [mikera.vectorz.matrix-api]
            [clojure.core.matrix :as mat]
            )
  (:use [overtone.live])
  (:import [javax.sound.sampled
            AudioFileFormat$Type
            AudioFormat
            AudioFormat$Encoding
            AudioInputStream
            AudioSystem
            Clip
            ]
           [ java.awt.image BufferedImage]
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


(defsynth siney [note 0 amp-buf 0 out-bus 0  beat-bus (:count time/main-beat) beat-trg-bus (:beat time/main-beat)]
  (let [cnt      (in:kr beat-bus)
        beat-trg (in:kr beat-trg-bus)
        amp (buf-rd:kr 1 amp-buf cnt)

        freq (midicps note)
        src (sin-osc freq)
        src (* src amp)]
    (out out-bus src)))

(defonce amp-buffers {:a  (buffer 963)
                      :a# (buffer 963)
                      :b  (buffer 963)
                      :c  (buffer 963)
                      :c# (buffer 963)
                      :d  (buffer 963)
                      :d# (buffer 963)
                      :e  (buffer 963)
                      :f  (buffer 963)
                      :f# (buffer 963)
                      :g  (buffer 963)})

(defonce synth-notes {:a  (siney :note (note :A3)  :amp-buf (:a amp-buffers))
                      :a# (siney :note (note :A#3) :amp-buf (:a# amp-buffers))
                      :b  (siney :note (note :B3)  :amp-buf (:b amp-buffers))
                      :c  (siney :note (note :C4)  :amp-buf (:c amp-buffers))
                      :c# (siney :note (note :C#4) :amp-buf (:c# amp-buffers))
                      :d  (siney :note (note :D4)  :amp-buf (:d amp-buffers))
                      :d# (siney :note (note :D#4) :amp-buf (:d# amp-buffers))
                      :e  (siney :note (note :E4)  :amp-buf (:e amp-buffers))
                      :f  (siney :note (note :F4)  :amp-buf (:f amp-buffers))
                      :f# (siney :note (note :F#4) :amp-buf (:f# amp-buffers))
                      :g  (siney :note (note :G4)  :amp-buf (:g amp-buffers))
                      :g# (siney :note (note :G#4) :amp-buf (:g# amp-buffers))})


(defn image->matrix [^BufferedImage bi rgb-to-val-fn]
  (let [w (.getHeight bi) ;;Notes is not rotated. lazy switch
        h (.getWidth bi)
        M (mat/new-matrix h w)]
    (println (type M))
    (dotimes [y h]
      (dotimes [x w]
        (let [v (rgb-to-val-fn (.getRGB bi x y))]
          (println v)

          (mat/mset! M y x v)
;;          (mat/mset! M y x v)
          )))
    M))

(def a (mat/new-matrix 2 2))
(type a)
(mat/mset a 0 0 3)

(defonce fft-image    (img/load-image "image.png"))
(defonce image-matrix (image->matrix fft-image identity))

(comment
  (and-we-are-the-dreamer-of-the-dreams 120)

  (doseq [row (map vector #([:a :a# :b :c :c# :d :d# :e :f :f# :g]) (mat/rows row))]
    (doseq [cell row]
      (buffer-write! (amp-buffers cell))))
  )
