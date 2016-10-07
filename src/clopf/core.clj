(ns clopf.core)

(defn destruct-packet
  "Validate the structure of a given byte array"
  [structure bindings packet]
  (let [[identifier length kind] (first structure)]
    (if (and (not-empty structure) (>= (count packet) length))
      (let [new-binding
              (case kind
                :string {identifier (apply str (map char (subvec packet 0 length)))})]
        (destruct-packet
          (rest structure)
          (conj bindings new-binding)
          (subvec packet length)))
      bindings)))

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
