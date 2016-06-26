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

(defn write-max-energy-note-only! []
  (doseq [[col-idx col] (map vector (range) (mat/columns image-matrix))]
    (let [as  (into [] col)
          max-idx (first (apply max-key second (map-indexed vector as)))
          max-energy (nth as max-idx)
          winning-note (nth notes max-idx)
          lossing-notes (drop-nth max-idx notes)]

      (doseq [[row-idx note] (map vector (range) notes)]
        (if (= row-idx max-idx)
          (do (println note max-energy) (buffer-write! (get amp-buffers note) col-idx [max-energy]))
          (buffer-write! (get amp-buffers note) col-idx [0.0]))))))

(defn write-energy! []
  (doseq [[col-idx col] (map vector (range) (mat/columns image-matrix))]
    (let [as  (into [] col)
          max-idx (first (apply max-key second (map-indexed vector as)))
          max-energy (nth as max-idx)
          winning-note (nth notes max-idx)
          lossing-notes (drop-nth max-idx notes)]

      (doseq [[row-idx note] (map vector (range) notes)]
        (if (= row-idx max-idx)
          (do (println note max-energy) (buffer-write! (get amp-buffers note) col-idx [max-energy]))
          (buffer-write! (get amp-buffers note) col-idx [0.0]))))))


(comment
  (and-we-are-the-dreamer-of-the-dreams 120)
  (mud/ctl-global-clock 6.0)
  (write-max-energy-note-only!)
  )

(disj notes [:a])

(max-key [0.2 0.3])
