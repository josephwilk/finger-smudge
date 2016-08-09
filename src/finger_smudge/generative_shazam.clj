(ns finger-smudge.generative-shazam
  (:use [overtone.live])
  (:require [mud.timing :as time]
            [mud.core :as mud]
            [clojure.java.io :as io]
            [taoensso.timbre :as timbre :refer (debug info warn error)]
            [clojure.edn :as edn]
            [image-resizer.core :as resizer]
            [clojure.java.shell :as shell]

            [finger-smudge.logging])

  (:import [java.awt Rectangle Dimension Robot Toolkit]
           [java.awt.image BufferedImage]
           [java.io File IOException]
           [javax.imageio ImageIO]))

(def max-buffer-size 512)

(do
  (defonce coef-b (buffer max-buffer-size))
  (defonce notes  (buffer max-buffer-size))
  (defonce dur-b  (buffer max-buffer-size))
  (definst plucked-string [amp 0.8 decay 30 coef 0.3 gate 1
                           release 0.2
                           attack 0.03
                           damp 0.2
                           coef-buf coef-b
                           beat-bus (:count time/beat-1th) beat-trg-bus (:beat time/beat-1th)
                           notes-buf 0 dur-buf 0
                           mix-rate 0.5
                           wave 0
                           fud 0
                           dist 0]
    (let [cnt (in:kr beat-bus)
          trg (in:kr beat-trg-bus)
          note (buf-rd:kr 1 notes-buf cnt)
           dur (buf-rd:kr 1 dur-buf cnt)
          coef (buf-rd:kr 1 coef-buf cnt)

          freq   (midicps note)
          noize  (* (lf-tri freq (sin-osc:kr 0.5)))
          dly    (/ 1.0 freq)
          plk    (select:ar fud [(pluck noize trg dly dly decay coef)
                                 (mix [(lf-saw (/ freq 2.0)) (pluck noize trg dly dly decay coef)])
                                 (sin-osc freq)
                                 (pitch-shift:ar (lf-saw freq) 0.1 0.1 0.1 0.1)
                                 (lf-saw freq)
                                 (lf-tri freq)
                                 (lf-pulse:kr freq)])
          dist   (select:ar dist [(distort plk) plk])
          filt   (rlpf dist (* 12 freq) 0.6)
          clp    (clip2 filt 0.8)
          wave (select:ar wave [(sin-osc freq (* 2 Math/PI))
                                (lf-saw freq (* 2 Math/PI))
                                (lf-tri freq (* 2 Math/PI))])
          clp (mix [clp
                    (* 1.01 wave)
                    ;;(rlpf (saw freq) 1200)
                    ])

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

(comment
  (do (defonce kick-seq-buf (buffer 256)) (defonce bass-notes-buf (buffer 256)) (defonce drums-g (group "main drums")) (defonce kick-amp (buffer 256))
      (def kicker (space-kick3 [:head drums-g] :note-buf bass-notes-buf :seq-buf kick-seq-buf :num-steps 16 :beat-num 0 :noise 0.05 :amp 0.0 :mod-index 0.1 :mod-freq 4.0 :mode-freq 0.2))(ctl kicker :amp-buf kick-amp)(mud/pattern! kick-amp  [1.5 1 1 1 1 1 1 1   1.1 1 1 1 1 1 1 1] (repeat 2 [1.2 1 1 1 1 1 1 1   1.1 1 1 1 1 1 1 1]) [1.2 1 1 1 1 1 1 1   1.2 1 1 1 1.2 1 1.3 1])(mud/pattern! bass-notes-buf (repeat 8 (mud/degrees [1] :minor :F1))(repeat 2 (repeat 8 (mud/degrees [1] :minor :F1)))(repeat 2 (repeat 8 (mud/degrees [3] :minor :F1)))(repeat 2 (repeat 8 (mud/degrees [3] :minor :F1)))[(mud/degrees [1 1 1 1  5 4 3 1] :minor :F1)])))

;;(ctl kicker :attack 0.0 :sustain 1.2 :amp 10.0 :mod-freq 1)

(defonce kick-seq-buf (buffer 32))
(defonce bass-notes-buf (buffer 16))
(defonce power-kick-seq-buf (buffer 16))

(comment
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
       (when kicker (ctl kicker :amp 0.0))))))

;;(kill plucked-string)

(def root "/Users/josephwilk/Workspace/josephwilk/clojure/finger-smudge")

(defn stop-it [counter change-iterations settings
               trigger-1 trigger-2
               generation-dir]
  (let [state {:counter @counter :change-iterations @change-iterations :settings @settings :generation generation-dir}
        score @counter]
    (info (pr-str state))
    (spit (str generation-dir "state.edn") (str state) :append true)
    (reset! counter 0)
    (reset! change-iterations 0)
    (reset! settings [])
    (Thread/sleep 1000)
    (recording-stop)
    (kill plucked-string)
    (mud/remove-beat-trigger trigger-1)
    (mud/remove-beat-trigger trigger-2)
    (mud/remove-all-beat-triggers)
    score))


(def text-matches (atom []))

(defn img->text [img]
  (let [r  (shell/sh "tesseract" img "stdout")
        noisey-text (clojure.string/split (:out r) #"\n")
        text (remove clojure.string/blank? noisey-text)]
    (when (not (clojure.string/blank? (:err r)))  (str (error (:err r))))
    (clojure.string/join "\n" (flatten text))))

(defn take-screenshot [generation-dir start-ts counter hit]
  (let [screen (.getScreenSize (Toolkit/getDefaultToolkit))
        width (.getWidth screen)
        rt (new Robot)
        img (.createScreenCapture rt (new Rectangle (- width 350) 35 350 75))
        t (System/currentTimeMillis)
        test-pixel (.getRGB img 30 40)]
    (when (or (= -3350273 test-pixel))
      (when (>= (- t @hit) (* 6 1000))
        (reset! hit t)
        (swap! counter inc)
        (let [f (str generation-dir t ".png")]
          (ImageIO/write img "png" (new File f))
          (let [text (str (img->text f))]
            (swap! text-matches concat [(clojure.string/join text)])
            (info (str "Match found: [" t "] Text: [" text  "] Track position: " (float (/ (- t start-ts) 1000))))))))))

(defn map-every-nth [f coll n]
  (map-indexed #(if (zero? (mod (inc %1) n)) (f %2) %2) coll))

(defn new-music-state []
  (let [current-buffer-size max-buffer-size
        root (choose ["A" "A#" "B" "C" "C#" "D" "D#" "E" "F" "F#" "G" "G#"])
        new-scale (choose [:minor-pentatonic :major-pentatonic :minor :major])

        octave-no (int (ranged-rand 2 16))
        octaves (repeatedly octave-no #(choose [1 1 2 2 3 3 4 4]))
        note-choices (mapcat (fn [octave]
                               (println (str root octave))
                               (scale (str root octave) new-scale))
                             octaves)
        note-choices (shuffle (concat note-choices (take (choose [1 2 4 8 16]) (cycle [0]))))

        wave      (choose [0 1 2 3])
        wave-base (choose [0 1 2 3 4 5 6 7 8])
        clock     (choose [4.0 3.0 5.0 6.0 7.0 8.0 9.0 10.0 11.0 12.0 13.0 14.0])
        score    (repeatedly current-buffer-size #(choose note-choices))
        coefs    (repeatedly current-buffer-size #(choose [3 2]))
        duration (repeatedly current-buffer-size #(choose [2 2 4 4 6 6]))
        distortion (choose [0 1])

        state {:wave wave :wave-base wave-base :clock clock :score score :coefs coefs :duration duration
               :root root :scale new-scale :octaves octaves :distortion distortion}]
    state))

;;(new-music-state)
(defn generate-score [state]
  (let [pick (rand-int 4)

        new-root (choose ["A" "A#" "B" "C" "C#" "D" "D#" "E" "F" "F#" "G" "G#"])
        new-scale (choose [:minor-pentatonic :major-pentatonic :minor :major])
        new-octaves (repeatedly (int (ranged-rand 2 32)) #(choose [1 1 2 2 3 3 4 4]))

        new-state (case pick
                    0 (assoc state :root new-root)
                    1 (assoc state :octaves new-octaves)
                    2 (assoc state :scale new-scale)
                    3 state)

        new-notes (mapcat (fn [octave]
                            (scale (str (:root new-state) octave) (:scale new-state)))
                          (:octaves new-state))
        new-score (repeatedly max-buffer-size #(choose new-notes))
        new-score (shuffle (concat new-score (take (choose [1 2 4 8 16]) (cycle [0]))))]
    (assoc new-state :score new-score)))

;;(generate-score {:root "A" :scale :minor :octaves [2]})
;;(new-music-state)

(defn progress-state [settings]
  (let [old-settings (last @settings)
        new-settings (new-music-state)

        options 7
        pick-one-thing (rand-int options)

        thingys [:wave :clock :score :coefs :duration :distortion :wave-base]]
    (let [new-state
          (case pick-one-thing
            0 (assoc old-settings :wave       (:wave new-settings))
            1 (assoc old-settings :clock      (:clock new-settings))
            2 (generate-score old-settings)
            3 (assoc old-settings :coefs      (:coefs new-settings))
            4 (assoc old-settings :duration   (:duration new-settings))
            5 (assoc old-settings :distortion (:distortion new-settings))
            6 (assoc old-settings :wave-base  (:wave-base new-settings)))]

      (info (str {:change-no (count @settings) :mutated (nth thingys pick-one-thing)}))
      new-state)))

(defn ping-synths! [settings p-synth]
  ;;(mud/pattern! kick-seq-buf (mud/spread (choose [1 4 8]) (choose [4 8 16])))

  (ctl p-synth :wave       (:wave settings))
  (ctl p-synth :fud        (:wave-base settings))
  (ctl p-synth :distortion (:distortion settings))

  (mud/ctl-global-clock (:clock settings))
  (mud/pattern! notes   (:score settings))
  (mud/pattern! coef-b  (:coefs settings))
  (mud/pattern! dur-b   (:duration settings)))

(def global-scores (atom {}))

(comment
  (def s (plucked-string :notes-buf notes :dur-buf dur-b :fud 4 :dist 1))
  (kill plucked-string)
  (mud/pattern! kick-seq-buf [0])
  (ctl s :dist 0 :fud 0)
  (mud/pattern! notes (take 32 (cycle (mud/degrees-seq [:f2 41243]))))
  (mud/pattern! dur-b (take 32 (cycle [2.0])))
  (mud/ctl-global-clock 4.0))

(defn go
  ([] (go 128))
  ([change-rate]
      (let [counter (atom 0)
            change-iterations (atom 0)
            settings (atom [(new-music-state)])]
        (info (str "Initial state: " (last @settings)))
        (let [t (System/currentTimeMillis)
              generation-dir (str root "/resources/generations/" t "/")]
          (io/make-parents (str generation-dir "/blah")) ;; Lazy create dirs
          (recording-start (str generation-dir "/generative.wav"))

          (let [shazam-hit (atom 0)
                p-synth (plucked-string :notes-buf notes :dur-buf dur-b)
                t1 (mud/on-beat-trigger 1  (fn [] (take-screenshot generation-dir t counter shazam-hit)))
                t2 (mud/on-beat-trigger
                    change-rate
                    (fn []
                      (let [new-state (progress-state settings)]
                        (swap! settings concat [new-state])
                        (ping-synths! new-state p-synth))))]
            (ping-synths! (last @settings) p-synth)
            (fn [scores]
              (let [score
                    (stop-it counter change-iterations settings
                             t1 t2
                             generation-dir)]
                (swap! scores assoc t score)
                scores)))))))

(def sleep-time (* 2 60 1000))
(def run-flag (atom true))

(defn delete-recursively [fname]
  (let [func (fn [func f]
               (when (.isDirectory f)
                 (doseq [f2 (.listFiles f)]
                   (func func f2)))
               (clojure.java.io/delete-file f))]
    (func func (clojure.java.io/file fname))))

(defn purge []
  (let [data (str "["
                  (clojure.string/join ","
                                       (clojure.string/split (slurp "global-scores.edn") #"\n"))

                  "]")
        results (into {} (edn/read-string data))
        misses (filter (fn [[k v]] (= v 0)) results)]
    (doseq [[k v] misses]
      (try
        (delete-recursively (str root "/resources/generations/" k))
        (catch Exception e (println e))))))

;;(purge)

(defn event-loop []
  (loop []
    (try
      (let [stop-fn (go (choose [128 64 32 16 8]))]
        (Thread/sleep sleep-time)
        (stop-fn global-scores)
        (info (str @global-scores))
        (spit "global-scores.edn" (str @global-scores "\n") :append true)
        (spit "matches.edn" (map str @text-matches) :append true))
      (Thread/sleep 1000) ;;Time for shutdown
      (catch Exception e
        (recording-stop)
        (error e)))
    (when @run-flag (recur))))

(set! *print-length* false)

(comment
  (event-loop)
  (recording-stop)
  (reset! run-flag false)
  )
;;(kill plucked-string)y
;;(kill space-kick3)
(comment
  (shell/sh "tesseract" "/Users/josephwilk/Workspace/josephwilk/clojure/finger-smudge/test.jpg" "stdout")

  (let [screen (.getScreenSize (Toolkit/getDefaultToolkit))
        width (.getWidth screen)
        rt (new Robot)
        img (.createScreenCapture rt (new Rectangle (- width 350) 35 350 75))
        test-pixel (.getRGB img 50 40)
        ;;   img (resizer/resize-to-height img 1000)
        ]
    (println width)
    (println test-pixel)
    (when (= test-pixel -3420723)
      (ImageIO/write img "png" (new File (str "test.png")))
      (println (img->text "/Users/josephwilk/Workspace/josephwilk/clojure/finger-smudge/test.png")))
    ))
