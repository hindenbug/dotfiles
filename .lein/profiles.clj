{:user {:dependencies [[org.clojure/clojure "1.6.0"]
                       [clj-stacktrace "0.2.7"]
                       [spyscope "0.1.3"]
                       [redl "0.2.4"]
                       [spyscope "0.1.5"]
                       [io.aviso/pretty "0.1.13"]
                       [lein-midje "3.1.3"]]
        :plugins [[lein-exec "0.3.4"]
                  [lein-kibit "0.0.8"]
                  [im.chit/vinyasa "0.2.2"]
                  [venantius/ultra "0.1.9"]]
        :ultra {:color-scheme :solarized_dark}
        :injections [(require 'spyscope.core)
                     (require '[redl core complete])
                     (require 'io.aviso.repl
                              'clojure.repl
                              'clojure.main)

                     (alter-var-root #'clojure.main/repl-caught
                                     (constantly @#'io.aviso.repl/pretty-pst))
                     (alter-var-root #'clojure.repl/pst
                                     (constantly @#'io.aviso.repl/pretty-pst))

                     (let [orig (ns-resolve (doto 'clojure.stacktrace require)
                                            'print-cause-trace)
                           new (ns-resolve (doto 'clj-stacktrace.repl require)
                                           'pst)]
                       (alter-var-root orig (constantly (deref new))))

                     ;;(require 'vinyasa.inject)
                     ]}}