(ns rforms.coercer
  (:import
    [goog.i18n NumberFormat DateTimeFormat DateTimeParse]))

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