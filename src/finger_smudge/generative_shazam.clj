(ns finger-smudge.generative-shazam
  (:use [overtone.live])
  (:require [mud.timing :as time]
            [mud.core :as mud]))

(do
  (defonce coef-b (buffer 128))
  (defonce notes (buffer 256))
  (definst plucked-string [amp 0.8 decay 30 coef 0.3 gate 1
                           release 0.2
                           attack 0.03
                           damp 0.2
                           coef-buf coef-b
                           beat-bus (:count time/beat-1th) beat-trg-bus (:beat time/beat-1th)
                           notes-buf 0 dur-buf 0
                           mix-rate 0.5]
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
          clp (mix [clp
                    (* 1.01 (sin-osc freq (* 2 Math/PI)))
                    (rlpf (saw freq) 1200)])

          clp (comb-n clp 0.9)
          reverb clp
          ;;reverb (g-verb clp 400 2.0 0.0 0.1 0.3 0.2 0.5 0.1 400)
          reverb (g-verb clp 250 20 0)]
            (pan2 (* amp (env-gen (perc attack release) :gate trg :time-scale dur) reverb)))))


(plucked-string :notes-buf notes)
(mud/pattern! coef-b (repeatedly 128 #(choose [4])))
(mud/pattern! notes (repeatedly 256 #(choose (scale :C3 :major))))

(mud/ctl-global-clock 0.0)

(recording-start "~/Desktop/generative_01.wav")
(recording-stop)
