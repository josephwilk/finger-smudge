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

(defn magnitude ^double [^double r ^double i] (Math/sqrt (+ (* r r) (* i i))))
(defn phase ^double [^double r ^double i] (Math/atan2 i r))
(defn real ^double [^double r ^double i] r)
(defn imaginary ^double [^double r ^double i] i)

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
       1.0)))

(defn ngrey-to-rgb [^double val]
  (let [ival (int (Math/round (* 1 val)))]
    (bit-or 0xFF000000
            (bit-shift-left ival 16)
            (bit-shift-left ival 8)
            ival)))

(defn dft-fn [ri-fn ^doubles sample-data]
  (let [n (count sample-data)
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
        (System/arraycopy sample-data src-pos
                          window 0
                          frame-size))

      (.realForward fft window)

      (dotimes [j frequency-range]
        (aset spectrum-data (+ i (* j window-count))
              (ri-fn (aget window (* j 2))
                     (aget window (inc (* j 2)))))))
    (Matrix/wrap frequency-range window-count spectrum-data)))

(def dft-magnitudes (partial dft-fn magnitude))
(def dft-phases     (partial dft-fn phase))
(def dft-reals      (partial dft-fn real))
(def dft-imaginarys (partial dft-fn imaginary))

(defn idft-fn [ri-fn R I]
  (let [w (mat/column-count R)
        h (mat/row-count R)

        frame-size 4096
        half-length (quot frame-size 2)
        frequency-range 700

        fft (mikera.matrixx.algo.FFT. (int frame-size)) ;; 4069
        window (make-array Double/TYPE (* 2 w h))

        overlap   (- frame-size (quot frame-size 3))
        increment (- frame-size overlap)

        nM (mat/new-matrix h w)]

    (dotimes [y h]
      (dotimes [x h]
        (aset window (+ (* y 2 h) (* x 2)) ^double (mat/mget R y x))
        (aset window (+ (* y 2 h) (inc (* x 2))) ^double (mat/mget I y x))))

    (.realInverse fft window true)

    (dotimes [y h]
      (dotimes [x w]
        (let [r (aget window (+ (* y 2 h) (* x 2)))
              i (aget window (+ (* y 2 h) (inc (* x 2))))]
          (mat/mset! nM y x (ri-fn r i)))))
    nM))

(defn normalize! [M]
  (let [^double mn (mat/emin M)
        ^double mx (mat/emax M)
        r (double (- mx mn))]
    (mat/emap! (fn [^double d] (/ (- d mn) r)) M)))

(def idft-ri-reals (partial idft-ri-fn real))

(defn image->matrix [^BufferedImage bi rgb-to-val-fn]
  (let [h (.getHeight bi)
        w (.getWidth bi)
        M (mat/new-matrix h w)]
    (dotimes [y h]
      (dotimes [x w]
        (mat/mset! M y x (rgb-to-val-fn (.getRGB bi x y)))))
        M))

(defn matrix->image [M val-to-rgb-fn]
  (let [w (mat/column-count M)
        h (mat/row-count M)
        bi (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)]
    (dotimes [y h]
      (dotimes [x w]
        (let [v (mat/mget M y x)]
          (.setRGB bi x y (unchecked-int (val-to-rgb-fn v))))))
    bi))

(def M (fft-matrix sample-double-data))

(defn demo-code []
  (let [i (render (fft-matrix  sample-double-data))]
    (img/show i)
    (img/write i "image.png" "png" :quality 1.0)))

(demo-code)
(img/write (matrix-to-image M ngrey-to-rgb) "image.png" "png" :quality 1.0)

(comment
  (def fft-from-image (image-matrix M "image.png"))
  (def sample-data (matrix->sample-data fft-from-image sample-double-data))
  (def buf (buffer (count sample-data))) ;; create a buffer to store the audio
  (doseq [[idx d] (map vector (range) sample-data)]
    (buffer-set! buf idx d))

  (buffer-save buf "play2.wav")
  )
