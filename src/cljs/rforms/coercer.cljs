(ns rforms.coercer
  (:require
    [clojure.string :as str]
    [rforms.validator :as validator])
  (:import
    [goog.i18n NumberFormat DateTimeFormat DateTimeParse]
    [goog.math]
    [clojure.string :as str]))

(defprotocol Coercer
  (to-str [this obj-value])
  (from-str [this str-value]))

(extend-protocol Coercer
  nil
  (to-str [this obj-value]
    (str obj-value))
  (from-str [this str-value]
    str-value))

(defn parse-date
  [datetime-parse d]
  (let [date (js/Date. 0)]
    (if-not (= 0 (.strictParse datetime-parse d date))
      (if (> (.getYear date) -900)
        date
        nil)
      nil)))

(deftype NumberCoercer [number-format finite? message]
  Coercer
  (to-str [_ obj-value]
    (if  (or (not finite?) (goog.math/isFiniteNumber obj-value))
      (.format number-format obj-value)
      ""))
  (from-str [this str-value]
    (.parse number-format str-value))

  validator/Validator
  (validate [this str-value]
    (let [obj-value (from-str this str-value)]
      (if-not (or (not finite?) (goog.math/isFiniteNumber obj-value))
        [message]
        []))))

(defn number-format [number-format message & {:keys [finite]
                                       :or {finite true}}]
  (->NumberCoercer number-format finite message))

