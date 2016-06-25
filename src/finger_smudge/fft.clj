(ns finger-smudge.fft
  "Adapted from https://clojurefun.wordpress.com/2013/12/22/spectrograms-with-overtone/"
  (:use [overtone core]

        )
  (:require [mikera.vectorz.matrix-api]
            [clojure.core.matrix :as mat]
            [mikera.image.core :as img]
            [mikera.image.colours :as col]
            [mikera.image.spectrum :as spec])
  (:import [mikera.matrixx Matrix AMatrix]
           [java.awt.image BufferedImage]
           [org.jtransforms.fft DoubleFFT_1D DoubleFFT_2D RealFFTUtils_2D]))

;;(defonce boot (boot-external-server))

(defonce samp-buf (sample "/Users/josephwilk/Workspace/josephwilk/clojure/finger-smudge/resources/136_think_mono11025.wav"))
(defonce sample-double-data (into-array Double/TYPE (buffer-read samp-buf)))

(defn magnitude ^double [^double r ^double i] (Math/sqrt (+ (* r r) (* i i))))
(defn phase ^double [^double r ^double i] (Math/atan2 i r))
(defn real ^double [^double r ^double i] r)
(defn imaginary ^double [^double r ^double i] i)

(defn chromaprint->grey [val]
  (spec/heatmap val)
)

(img/show (matrix->image (file->image "image.data") chromaprint->grey) "data.png" "png")

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
        frequency-range 70

        ;;fft (mikera.matrixx.algo.FFT. (int frame-size)) ;;While faster for sanity, go with JTransform. Less abstraction for my brain.
        fft (DoubleFFT_1D. (int frame-size))

        fft-data (double-array (* 2 frame-size)) ;;Real + Imaginary part

        overlap   (- frame-size (quot frame-size 3))
        increment (- frame-size overlap)

        window-count  (quot (- n frame-size) overlap)
        spectrum-data (double-array (* frequency-range window-count))]

    (dotimes [i window-count]
      (let [src-pos (* i overlap)]
        ;; src  srcPos  dest destpos length
        (System/arraycopy sample-data src-pos
                          fft-data 0
                          frame-size))

      ;;8192
      (println :size (count fft-data))

      (.realForward fft fft-data)

      (dotimes [j frequency-range]
        (aset spectrum-data (+ i (* j window-count))
              (ri-fn (aget fft-data (* j 2))       ;; Real
                     (aget fft-data (inc (* j 2))) ;; Imaginary
                     ))))

    (Matrix/wrap frequency-range window-count spectrum-data)))

(def dft-magnitudes (partial dft-fn magnitude))
(def dft-phases     (partial dft-fn phase))
(def dft-reals      (partial dft-fn real))
(def dft-imaginarys (partial dft-fn imaginary))

(defn idft-fn [ri-fn M sample-data]
  (let [w (mat/column-count M)
        h (mat/row-count M)
        frame-size 4096
        half-length (quot frame-size 2)
        frequency-range 700
        ;;fft (DoubleFFT_1D. (int frame-size))
        fft (mikera.matrixx.algo.FFT. (int frame-size))
        fft-data (make-array Double/TYPE (* 2 frame-size))


       ;; window-count  (quot (- n frame-size) overlap)
        overlap   (- frame-size (quot frame-size 3))
        increment (- frame-size overlap)
        nM (mat/new-matrix h w)
        result (double-array (count sample-data))]
    ;;558 70
    (println w h)

    (dotimes [y h]
      (let [d (.toDoubleArray (mat/get-row M y))]

        ;;TODO: d -> Magnitude values.
        ;;Reverse (Math/sqrt (+ (* r r) (* i i)))

        (println :row-as-double d (count d))

        ))

    (println :inverse)
    (println :data (count fft-data))

    (.realInverse fft fft-data true)
    (println :postinverse)


    (println :data (count fft-data))

    (aset result w fft-data)

;;    (dotimes [r (count spectrum-data)])

;;    (aset result row (aget spectrum-data 0))
;;    (.getRows nM)
    result))

(defn normalize! [M]
  (let [^double mn (mat/emin M)
        ^double mx (mat/emax M)
        r (double (- mx mn))]
    (mat/emap! (fn [^double d] (/ (- d mn) r)) M)))

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

(def M (dft-magnitudes sample-double-data)) ;;70x558

;;(def W (idft-fn dft-reals M sample-double-data))

(println :size-sample      (count sample-double-data))
(println :fft-matrix-rows  (mat/row-count M))
(println :fft-matrix-cols  (mat/column-count M))

(defn demo-code []
  (let [i (matrix->image (dft-magnitudes sample-double-data) ngrey-to-rgb)]
    (img/show i)
    (img/write i "image.png" "png" :quality 1.0)))

(defn file->image [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (let [lines (line-seq rdr)
          data    (doall
                   (map (fn [r]
                          (let [data
                                (doall
                                 (map #(Double/parseDouble %1)
                                      (clojure.string/split r #" ")))]
                            data))
                        lines))
          rows (count data)
          cols (count (nth data 0))
          data (into-array Double/TYPE (flatten data))]
      (Matrix/wrap rows cols data))))

(comment
  (img/show (matrix->image (file->image "image.data") chromaprint->grey) "data.png" "png" :quality 1.0)
  (demo-code)
  (img/write (matrix-to-image M ngrey-to-rgb) "image.png" "png" :quality 1.0)

  (def data (io/resource "image.data"))

  (def d  (idft-fn real M sample-double-data))

  (let [f (DoubleFFT_1D. 5)]
    (def xt (double-array [1 2 4 6 8]))
    (.realForward f xt)
    (println (seq xt))
    (.realInverse f xt true)
    (println (seq xt)))

  (def fft-from-image (image-matrix M "image.png"))
  (def sample-data (matrix->sample-data fft-from-image sample-double-data))
  (def buf (buffer (count W))) ;; create a buffer to store the audio
  (doseq [[idx d] (map vector (range) W)]
    (buffer-set! buf idx d))

  (buffer-save buf "play2.wav"))
