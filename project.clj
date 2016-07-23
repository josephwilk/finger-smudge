(defproject finger-smudge "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main finger-smudge.core

  :global-vars {*print-length* false}

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [http-kit "2.1.18"]
                 [cheshire "5.6.1"]
                 [org.craigandera/dynne "0.4.1"]
                 [org.clojure/math.combinatorics "0.1.3"]

                 [org.imgscalr/imgscalr-lib "4.2"]
                 [com.github.wendykierp/JTransforms "3.1"]

                 [overtone "0.9.1"]
                 [net.mikera/vectorz-clj "0.29.0"]
                 [net.mikera/imagez "0.10.0"]

                 [mud "0.1.0-SNAPSHOT"]]
  :jvm-opts ["-Xmx2048m"])
