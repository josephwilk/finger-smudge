(ns finger-smudge.generative-shazam
  (:use [overtone.live])
  (:require [mud.timing :as time]
            [mud.core :as mud]
            [clojure.java.io :as io])

  (:import [java.awt Rectangle Dimension Robot Toolkit]
           [java.awt.image BufferedImage]
           [java.io File IOException]
           [javax.imageio ImageIO]))

(do
  (defonce coef-b (buffer 128))
  (defonce notes (buffer 256))
  (defonce dur-b (buffer 256))
  (definst plucked-string [amp 0.8 decay 30 coef 0.3 gate 1
                           release 0.2
                           attack 0.03
                           damp 0.2
                           coef-buf coef-b
                           beat-bus (:count time/beat-1th) beat-trg-bus (:beat time/beat-1th)
                           notes-buf 0 dur-buf 0
                           mix-rate 0.5
                           wave 0]
    (let [cnt (in:kr beat-bus)
          trg (in:kr beat-trg-bus)
          note (buf-rd:kr 1 notes-buf cnt)
           dur (buf-rd:kr 1 dur-buf cnt)
          coef (buf-rd:kr 1 coef-buf cnt)

          freq   (midicps note)
          noize  (* (lf-tri freq (sin-osc:kr 0.5)))
          dly    (/ 1.0 freq)
          plk    (pluck noize trg dly dly decay coef)
          dist   (distort plk)
          filt   (rlpf dist (* 12 freq) 0.6)
          clp    (clip2 filt 0.8)
          wave (select:ar [(sin-osc freq (* 2 Math/PI))
                           (lf-saw freq (* 2 Math/PI))
                           (lf-tri freq (* 2 Math/PI))]
                          wave)
          clp (mix [clp
                    (* 1.01 (sin-osc freq (* 2 Math/PI)))
                    (rlpf (saw freq) 1200)])

          clp (comb-n clp 0.9)
          reverb clp
          ;;reverb (g-verb clp 400 2.0 0.0 0.1 0.3 0.2 0.5 0.1 400)
          reverb (g-verb clp 250 20 0)]
            (pan2 (* amp (env-gen (perc attack release) :gate trg :time-scale dur) reverb)))))

(defsynth space-kick3
  "More randomness + a little pitch shift for a floater mellow kick"
  [amp 1 amp-buf 0 mod-freq 5 mod-index 5 sustain 0.4 noise 0.025 attack 0.005
   beat-bus (:count time/main-beat) beat-trg-bus (:beat time/main-beat) note-buf 0 seq-buf 0 beat-num 0 num-steps 8 out-bus 0]
  (let [cnt      (in:kr beat-bus)
        beat-trg (in:kr beat-trg-bus)
        note     (buf-rd:kr 1 note-buf cnt)
        amp      (* amp (buf-rd:kr 1 amp-buf cnt))
        bar-trg  (and (buf-rd:kr 1 seq-buf cnt)
                      ;;                     (= beat-num cnt)
                      beat-trg)
        freq (midicps note)

        pitch-contour (line:kr (* 2 freq) freq 0.02)
        drum (lpf (sin-osc pitch-contour (sin-osc mod-freq (/ mod-index 1.3))) 1000)
        drum-env (env-gen (perc attack sustain) :gate bar-trg)
        hit (hpf (* noise (white-noise)) 500)
        hit (lpf hit (line 6000 500 0.03))
        hit-env (env-gen (perc) :gate bar-trg)
        src (* amp (+ (* drum drum-env) (* hit hit-env)))
        src (pitch-shift src 0.4 1 0 (t-rand:ar 0.01 0.0 bar-trg))
        src (free-verb src)]
                (out out-bus (pan2 src))))

(defsynth whitenoise-hat2 [out-bus 0
                           amp-buf 0
                           seq-buf 0 beat-bus (:count time/main-beat) beat-trg-bus (:beat time/main-beat) num-steps 0 beat-num 0
                           verb-buf 0
                           amp 1
                           release 1 attack 0]
  (let [cnt      (in:kr beat-bus)
        amp      (* amp (buf-rd:kr 1 amp-buf cnt))
        beat-trg (in:kr beat-trg-bus)
        verb    (buf-rd:kr 1 verb-buf cnt)
        bar-trg (and (buf-rd:kr 1 seq-buf cnt)
                     ;;(= beat-num cntsteps)
                     beat-trg)
        w (* 0.01 (pitch-shift (white-noise:ar)))
        attack-fuzz (* attack 0.4)
        release-fuzz (* release 0.2)
        attack (+ (t-rand:kr (- attack attack-fuzz) (+ attack attack-fuzz) bar-trg) attack)
        release (+ (t-rand:kr (- release release-fuzz) (+ release release-fuzz) bar-trg) release)
        e (env-gen (perc :attack attack :release release) :gate bar-trg)
        src (* e w)
        [s1 s2] [src src]
        ;;        echo-src  (+ src (comb-n src 1.0 0.25 1))
        ;;        ctl-wave-prob (coin-gate:kr verb (impulse:kr (/ 1 0.5)))
        ;;        s1 (select:ar ctl-wave-prob [echo-src s1])
        ;;        s2 (select:ar ctl-wave-prob [echo-src s2])
        src (free-verb src 0.3 (t-rand:kr 0.5 1.0 bar-trg) (t-rand:kr 0.0 1.0 bar-trg))
        ;;        src (* slice-amp [s1 s2])
        ]
        (out out-bus (pan2 (* amp src)))))

(do (defonce kick-seq-buf (buffer 256)) (defonce bass-notes-buf (buffer 256)) (defonce drums-g (group "main drums")) (defonce kick-amp (buffer 256))
    (def kicker (space-kick3 [:head drums-g] :note-buf bass-notes-buf :seq-buf kick-seq-buf :num-steps 16 :beat-num 0 :noise 0.05 :amp 0.0 :mod-index 0.1 :mod-freq 4.0 :mode-freq 0.2))(ctl kicker :amp-buf kick-amp)(mud/pattern! kick-amp  [1.5 1 1 1 1 1 1 1   1.1 1 1 1 1 1 1 1] (repeat 2 [1.2 1 1 1 1 1 1 1   1.1 1 1 1 1 1 1 1]) [1.2 1 1 1 1 1 1 1   1.2 1 1 1 1.2 1 1.3 1])(mud/pattern! bass-notes-buf (repeat 8 (mud/degrees [1] :minor :F1))(repeat 2 (repeat 8 (mud/degrees [1] :minor :F1)))(repeat 2 (repeat 8 (mud/degrees [3] :minor :F1)))(repeat 2 (repeat 8 (mud/degrees [3] :minor :F1)))[(mud/degrees [1 1 1 1  5 4 3 1] :minor :F1)]))


(ctl kicker :attack 0.0 :sustain 0.2 :amp 4.0)

(defonce kick-seq-buf (buffer 32))
(defonce bass-notes-buf (buffer 16))
(defonce power-kick-seq-buf (buffer 16))

(mud/pattern! dur-b (repeatedly 256 #(choose [1])))
(mud/pattern! coef-b (repeatedly 128 #(choose [4])))
(mud/pattern! notes (repeatedly 256 #(choose (scale :C3 :major))))

(mud/one-time-beat-trigger
 15 16
 (fn []
   (do
     (defonce hats-buf (buffer 256)) (defonce hats-amp (buffer 256)) (defonce hat-verb (buffer 256))
     (mud/pattern! hats-amp [4 4 4 4])
     (mud/pattern! hat-verb[1.0])
     ;;(kill whitenoise-hat2)
     (def white (whitenoise-hat2 [:head drums-g] :amp-buf hats-amp :seq-buf hats-buf :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th) :num-steps 16 :release 0.1 :attack 0.0 :beat-num 0))(ctl white :amp-buf hats-amp)
     (ctl white :attack 0.002 :release 0.04 :amp 0.0)
     (when kicker (ctl kicker :amp 0.0)))))

;;(kill plucked-string)

(def root "/Users/josephwilk/Workspace/josephwilk/clojure/finger-smudge")

(defn stop-it []
  (println @counter)
  (reset! counter 0)
  (recording-stop)
  (kill plucked-string)
  (mud/remove-beat-trigger trigger-g17519)
  (mud/remove-all-beat-triggers))

(defn take-screenshot [start-ts counter]
  (let [screen (.getScreenSize (Toolkit/getDefaultToolkit))
        rt (new Robot)
        img (.createScreenCapture rt (new Rectangle (int (.getWidth screen)) (int (.getHeight screen))))
        t (System/currentTimeMillis)
        test-pixel (.getRGB img 2230 72)]
    (when (= -15570434 test-pixel)
      (swap! counter inc)
      (println (str "Match found: [" t "] Track position: " (/ (/ (- t start-ts) 1000) 60)))
      (ImageIO/write img "jpg" (new File (str "resources/generations/" start-ts "/screenshots/" t "-" test-pixel "-" ".jpg"))))))

(defn shake-music-params! [p-synth change-iterations]
  (swap! change-iterations inc)
  (println (str "Change: " @change-iterations))
  (let [s (choose ["A" "A#" "B" "C" "C#" "D" "D#" "E" "F" "F#" "G"])
        n (flatten (concat (scale (str s "2") :minor-pentatonic)
                           (scale (str s "3") :minor-pentatonic)
                           (scale (str s "4") :minor-pentatonic)))]
    (ctl p-synth :wave (choose [0 1 2 3]))
    (mud/ctl-global-clock      (choose [4.0 3.0 5.0 6.0 7.0 8.0]))
    (mud/pattern! notes        (repeatedly 256 #(choose n)))
    (mud/pattern! coef-b       (repeatedly 128 #(choose [2])))
    (mud/pattern! hats-buf     (repeatedly 32 #(choose [0 0 0 0])))
    (mud/pattern! kick-seq-buf (repeatedly 32 #(choose [0 0 0 0])))
    (mud/pattern! hats-amp     (repeatedly 32 #(choose [0 4.0 4.0])))
    (mud/pattern! dur-b        (repeatedly 256 #(choose [4 4 5 5 6 6])))))

(defn go []
  (def counter (atom 0))
  (def change-iterations (atom 0))
  (let [t (System/currentTimeMillis)
        generation-dir (str root "/resources/generations/" t "/screenshots")
        ]
    (io/make-parents (str generation-dir "/blah"))


    (println (str generation-dir "/generative.wav"))
    (recording-start (str generation-dir "/generative.wav"))
    (let [p-synth (plucked-string :notes-buf notes :dur-buf dur-b)]

      (def trigger-g17519 (mud/on-beat-trigger 1  (fn [] (take-screenshot t counter))))
      (def trigger-g17518 (mud/on-beat-trigger 128 (fn [] (shake-music-params! p-synth change-iterations))))))
  )

(comment
  (go)
  (stop-it)
  )
