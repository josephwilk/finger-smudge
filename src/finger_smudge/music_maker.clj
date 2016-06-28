(ns finger-smudge.music-maker
  (:require [dynne.sound :as sound]
            [clojure.java.io :as io]
            [primitive-math :as p]
            [finger-smudge.fft :as fft]
            [mikera.image.core :as img]
            [mud.timing :as time]
            [mud.core :as mud]
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
        src (mix [(sin-osc freq)
                  (lf-tri freq)
                  (lpf (* 0.25 (lf-saw freq)) (/ freq 2.0))])
        src (g-verb src 300)
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

(def synth-notes {:a  (siney :note (note :A2)  :amp-buf (:a amp-buffers))
                  :a# (siney :note (note :A#2) :amp-buf (:a# amp-buffers))
                  :b  (siney :note (note :B2)  :amp-buf (:b amp-buffers))
                  :c  (siney :note (note :C3)  :amp-buf (:c amp-buffers))
                  :c# (siney :note (note :C#3) :amp-buf (:c# amp-buffers))
                  :d  (siney :note (note :D3)  :amp-buf (:d amp-buffers))
                  :d# (siney :note (note :D#3) :amp-buf (:d# amp-buffers))
                  :e  (siney :note (note :E3)  :amp-buf (:e amp-buffers))
                  :f  (siney :note (note :F3)  :amp-buf (:f amp-buffers))
                  :f# (siney :note (note :F#3) :amp-buf (:f# amp-buffers))
                  :g  (siney :note (note :G3)  :amp-buf (:g amp-buffers))
                  :g# (siney :note (note :G#3) :amp-buf (:g# amp-buffers))})
(kill siney)

(defn image->matrix [^BufferedImage bi rgb-to-val-fn]
  (let [h (.getHeight bi) ;;Notes is not rotated. lazy switch
        w (.getWidth bi)
        M (mat/new-matrix h w)]
    (dotimes [row-idx h]
      (dotimes [col-idx w]
        (let [v (rgb-to-val-fn (.getRGB bi col-idx row-idx))]
          ;;(println y x)
          (mat/mset! M row-idx col-idx v))))
    M))

(mat/set-current-implementation :vectorz)

(def fft-image    (img/load-image "resources/chroma_fft_image.png"))
(def image-matrix  (mat/transpose (fft/file->image "image.data")))

(def notes [:a :a# :b :c :c# :d :d# :e :f :f# :g :g#])

(defn drop-nth
  [n coll]
  (->> coll
       (map vector (iterate inc 1))
       (remove #(zero? (mod (first %) n)))
              (map second)))

(do
  (defn write-max-energy-note-only! [scale]
    (let [scale-notes (map (fn [x] (keyword (clojure.string/lower-case (name (case x
                                                                               :Bb :a# ;;Joe hack
                                                                               :Eb :d# ;;For joes brain.
                                                                               x))))) (map find-pitch-class-name scale))]

      (doseq [[col-idx col] (map vector (range) (mat/columns image-matrix))]
        (let [as  (into [] col)
              max-idx (first (apply max-key second (map-indexed vector as)))
              max-energy (nth as max-idx)
              winning-note (nth notes max-idx)
              lossing-notes (drop-nth max-idx notes)]
          (doseq [[row-idx note] (map vector (range) notes)]
            (let [e (if (and
                         (some #{winning-note} scale-notes)
                         (= row-idx max-idx)) max-energy 0.0)]
              (buffer-write! (get amp-buffers note) col-idx [e])))))))

  (write-max-energy-note-only! (scale :Bb3 :major)))

(defn write-energy! []
  (doseq [[note row] (map vector [:a :a# :b :c :c# :d :d# :e :f :f# :g :g#] (mat/rows image-matrix))]
    (doseq [[idx-x cell] (map vector (range) row)]
      (buffer-write! (get amp-buffers note) idx-x [cell]))))

(comment
  (and-we-are-the-dreamer-of-the-dreams 120)
  (mud/ctl-global-clock (* 4 32.0))
  )
