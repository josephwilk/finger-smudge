(ns finger-smudge.music-maker
  (:require [dynne.sound :as sound]
            [clojure.java.io :as io]
            [primitive-math :as p]
            [finger-smudge.fft :as fft]
            [mikera.image.core :as img]
            [mud.timing :as time]
            ;;[clojure.core.matrix :as mat]

            [clojure.core.matrix :as mat]
;;            [mikera.vectorz.matrix-api]
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
           [mikera.matrixx Matrix AMatrix]
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
                      :g  (buffer 963)
                      :g#  (buffer 963)
                      })

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
  (let [h (.getHeight bi) ;;Notes is not rotated. lazy switch
        w (.getWidth bi)
        M (mat/new-matrix w h)]
    ;;(println (type M))
    (dotimes [y w]
      (dotimes [x h]
        (let [v (rgb-to-val-fn (.getRGB bi y x))]
          ;;(println y x)
          (mat/mset! M y x v))))
    M))

(mat/set-current-implementation :vectorz)

(defonce fft-image    (img/load-image "image.png"))
(def image-matrix (image->matrix fft-image fft/rgb-to-ngrey))

(comment
  (and-we-are-the-dreamer-of-the-dreams 120)

  (doseq [[note idx-y row] (map vector [:a :a# :b :c :c# :d :d# :e :f :f# :g] (range) (mat/rows image-matrix))]
    (doseq [[idx-x cell] (map vector (range) row)]
      (let [cell (/ cell 2)]
        (println cell)
        (buffer-write! (get amp-buffers note) (* idx-y idx-x) [cell])))))
