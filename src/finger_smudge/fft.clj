(ns finger-smudge.fft
  "Adapted from https://clojurefun.wordpress.com/2013/12/22/spectrograms-with-overtone/"
  (:use [overtone core])
  (:require [mikera.vectorz.matrix-api]
            [clojure.core.matrix :as mat]
            [mikera.image.core :as img]
            [mikera.image.colours :as col]
            [mikera.image.spectrum :as spec])
  (:import [mikera.matrixx Matrix AMatrix]
           [java.awt.image BufferedImage]
           [org.jtransforms.fft DoubleFFT_2D RealFFTUtils_2D]))

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

    (dotimes [y window-count]
      ;; src  srcPos  dest destpos length
      (let [src-pos (* y overlap)]
        (System/arraycopy sample-array src-pos
                          window 0
                          frame-size))

      (.realInverse fft window true)

      (dotimes [x frequency-range]
        (aset spectrum-data (+ y (* x window-count))
              (magnitude (aget window (* x 2))
                         (aget window (inc (* y 2)))))))
    spectrum-data))

(defn rgb-to-grey [val]
  (let [val (int  val)]
    (+ (* 0.2126 (bit-and 0xFF (bit-shift-right val 16)))
       (* 0.7152 (bit-and 0xFF (bit-shift-right val 8)))
       (* 0.0722 (bit-and 0xFF val)))))

(defn rgb-to-ngrey [val]
  (let [val (int val)]
    (/ (+ (* 0.2126 (bit-and 0xFF (bit-shift-right val 16)))
          (* 0.7152 (bit-and 0xFF (bit-shift-right val 8)))
          (* 0.0722 (bit-and 0xFF val)))
       255.0)))

(defn ngrey-to-rgb [^double val]
  (let [ival (int (Math/round (* 2 val)))]
    (bit-or 0xFF000000
            (bit-shift-left ival 16)
            (bit-shift-left ival 8)
            ival)))

(defn render
  "Renders a spectrogram matrix into a bufferedimage"
  ([M]
     (render M (img/new-image (mat/column-count M) (mat/row-count M))))
  ([^AMatrix M ^BufferedImage bi]
     (let [w (.getWidth bi)
           h (.getHeight bi)]
       (dotimes [x w]
         (dotimes [y h]
           (let [v (.get M (int y) (int x))
                 val-fn #(spec/heatmap (* 0.009 %1))
                 val-fn ngrey-to-rgb
                 ]
             (.setRGB bi
                      (int x) (- (dec h) (int y))
                      (unchecked-int (val-fn v)))))))
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
                 v rgb ;;Much more required here
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

(comment
  (def fft-from-image (consume M "image.png"))
  (def sample-data (matrix->sample-data fft-from-image sample-double-data))
  (def buf (buffer (count sample-data))) ;; create a buffer to store the audio
  (doseq [[idx d] (map vector (range) sample-data)]
    (buffer-set! buf idx d))

  (buffer-save buf "play.wav"))
