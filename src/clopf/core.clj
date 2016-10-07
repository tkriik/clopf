(ns clopf.core)

(defn too-long?
  [message]
  (< 5 (count message)))

(defn drop-if
  [condition]
  (fn [message]
    (if-not (condition message)
             message)))

(defn packet-handler
  "Make a function that takes a packet and runs it through the provided handlers"
  [& handlers]
  (fn [packet]
    ((apply comp handlers) packet)))

(defmacro defpacket
  "Define how a packet should be handled"
  [name & handlers]
  `(def ~name (packet-handler ~@handlers)))
