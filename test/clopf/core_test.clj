(ns clopf.core-test
  (:require [clopf.core :as c])
  (:use clojure.test))

(deftest test-destruct-packet
  (testing "packet of strings"
    (let [structure [[:id-a 3 :string] [:id-b 2 :string]]
          bindings {}
          packet [(byte 0x61) (byte 0x62) (byte 0x63) (byte 0x64) (byte 0x65)]]
      (is (= (c/destruct-packet structure bindings packet)
             {:id-a "abc" :id-b "de"}))))
  (testing "too short a packet"
    (let [structure [[:id-a 3 :string] [:id-b 2 :string]]
          bindings {}
          packet [(byte 0x61) (byte 0x62) (byte 0x63)]]
      (is (nil? (c/destruct-packet structure bindings packet))))))
