(ns finger-smudge.fft
  "Adapted from https://clojurefun.wordpress.com/2013/12/22/spectrograms-with-overtone/"
  (:use [overtone core])
  (:require [mikera.vectorz.matrix-api]
            [clojure.core.matrix :as mat]
            [mikera.image.core :as img]
            [mikera.image.colours :as col]
            [mikera.image.spectrum :as spec])
  (:import [mikera.matrixx Matrix AMatrix]
           [java.awt.image BufferedImage]))

(defonce boot (boot-external-server))
(defonce buf (buffer 11025)) ;; create a buffer to store the audio

(defonce samp-buf (sample "/Users/josephwilk/Workspace/josephwilk/clojure/finger-smudge/resources/136_think_mono11025.wav"))

(defonce sample-double-data (into-array Double/TYPE (buffer-read samp-buf)))

(defn magnitude
  (^double [^double a ^double b]
           (Math/sqrt (+ (* a a) (* b b)))))

(defn fft-matrix [^doubles sample-array]
  (let [n (count sample-array)
        frame-size 4096
        half-length (quot frame-size 2)
        frequency-range 700

        fft (mikera.matrixx.algo.FFT. (int frame-size)) ;; 4069
        window (double-array (* 2 frame-size))

        overlap   (- frame-size (quot frame-size 3))
        increment (- frame-size overlap)

        window-count  (quot (- n frame-size) overlap)
        spectrum-data (double-array (* frequency-range window-count))]

    (dotimes [i window-count]
      ;; src  srcPos  dest destpos length
      (let [src-pos (* i overlap)]
        (System/arraycopy sample-array src-pos
                          window 0
                          frame-size))

      (.realForward fft window)

      (dotimes [j frequency-range]
        (aset spectrum-data (+ i (* j window-count))
              (magnitude (aget window (* j 2))
                         (aget window (inc (* j 2)))))))

    (Matrix/wrap frequency-range window-count spectrum-data)))

(defn matrix->sample-data [m sample-array]
  (let [n (count sample-array)
        frame-size 4096
        half-length (quot frame-size 2)
        frequency-range 700

        fft (mikera.matrixx.algo.FFT. (int frame-size)) ;; 4069
        window (double-array (* 2 frame-size))

        overlap   (- frame-size (quot frame-size 3))
        increment (- frame-size overlap)

        window-count  (quot (- n frame-size) overlap)
        spectrum-data (double-array n)]

    (dotimes [i window-count]
      ;; src  srcPos  dest destpos length
      (let [src-pos (* i overlap)]
        (System/arraycopy sample-array src-pos
                          window 0
                          frame-size))

      (.realInverse fft window true)

      (dotimes [j frequency-range]
        (aset spectrum-data (+ i (* j window-count))
              (magnitude (aget window (* j 2))
                         (aget window (inc (* j 2)))))))
    spectrum-data))

(defn colour ^long [^double val]
  (let [lval (* (inc (Math/log val)) 0.9)]
    (println val)
    (cond
     (<= lval 0.0) 0xFF00000
     (<= lval 1.0) (let [v (- lval 0.0)] (col/rgb 0.0 0.0 v))
     (<= lval 2.0) (let [v (- lval 1.0)] (col/rgb v 0.0 (- 1.0 v)))
     (<= lval 3.0) (let [v (- lval 2.0)] (col/rgb 1.0 v 0.0))
     (<= lval 4.0) (let [v (- lval 3.0)] (col/rgb 1.0 1.0 v))
     :else 0xFFFFFFFFF)))

(defn render
  "Renders a spectrogram matrix into a bufferedimage"
  ([M]
     (render M (img/new-image (mat/column-count M) (mat/row-count M))))
  ([^AMatrix M ^BufferedImage bi]
     (let [w (.getWidth bi)
           h (.getHeight bi)]
       (dotimes [x w]
         (dotimes [y h]
           (.setRGB bi
                    (int x) (- (dec h) (int y))
                    (unchecked-int (spec/heatmap (* 0.009 (.get M (int y) (int x)))))))))
     bi))

(defn consume
  "Consumes bufferedimage and generates a FFT matrix"
  ([^AMatrix M  img-file]
     (let [bi ^BufferedImage (img/load-image img-file)
           w (.getWidth bi)
           h (.getHeight bi)]
       (dotimes [x w]
         (dotimes [y h]
           (let [rgb (.getRGB bi (int x) (- (dec h) (int y)))
                 v (/ rgb 0.009) ;;Much more required here
                 ]
             (.set M (int y) (int x) v))
           ))
      M)))


(def M (fft-matrix sample-double-data))

(defn demo-code []
  (let [i (render (fft-matrix  sample-double-data))]
    (img/show i)
    (img/write i "image.png" "png" :quality 1.0)))

(demo-code)

(def fft-from-image (consume M "image.png"))
(def sample-data (matrix->sample-data fft-from-image sample-double-data))

(def buf (buffer (count sample-data))) ;; create a buffer to store the audio
(doseq [[idx d] (map vector (range) sample-data)]
  (buffer-set! buf idx d))

(buffer-save buf "play.wav")
