(ns clopf.core)

(defn destruct-packet
  "Validate the structure of a given byte array"
  ([structure bindings packet]
    (defn byte-array->str [a] (apply str (map char a)))
    (let [[id length kind] (first structure)
          packet-is-done (or (nil? length) (< (count packet) length))]
      (cond
        (and packet-is-done (not-empty structure)) nil
        (not-empty structure)
          (let [new-binding
                 (case kind
                   :string {id (byte-array->str (subvec packet 0 length))})]
            (destruct-packet
              (rest structure)
              (conj bindings new-binding)
              (subvec packet length)))
        :else bindings)))
  ([structure bindings]
    (partial destruct-packet structure bindings)))

(defn drop-if
  ([condition destructured-packet]
    (if-not (condition destructured-packet)
             destructured-packet))
  ([condition]
    (partial drop-if condition)))

(defn packet-handler
  "Make a function that takes a packet and runs it through the provided handlers"
  [& handlers]
  (fn [packet]
    ((apply comp (reverse handlers)) packet)))

(defmacro defpacket
  "Define how a packet should be handled"
  [name & handlers]
  `(def ~name (packet-handler ~@handlers)))
