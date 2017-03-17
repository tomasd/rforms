(ns rforms.coercer
  (:refer-clojure :exclude [keyword])
  (:require
    [rforms.validator :as validator]
    [clojure.string :as str]
    [goog.math])
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

(deftype NumberCoercer [number-format finite? error-message]
  Coercer
  (to-str [_ obj-value]
    (if (or (not finite?) (goog.math/isFiniteNumber obj-value))
      (.format number-format obj-value)
      ""))
  (from-str [this str-value]
    (.parse number-format str-value))

  validator/Validator
  (validate [this str-value]
    (let [obj-value (from-str this str-value)]
      (if-not (or (not finite?) (goog.math/isFiniteNumber obj-value))
        [error-message]
        []))))



(defn- make-number-format [format]
  (cond
    (keyword? format)
    (-> {:decimal    NumberFormat.Format.DECIMAL
         :scientific NumberFormat.Format.SCIENTIFIC
         :percent    NumberFormat.Format.PERCENT
         :currency   NumberFormat.Format.CURRENCY}
        (get format)
        (NumberFormat.))

    :else (NumberFormat. format)))

(defn number [number-format error-message & {:keys [finite]
                                             :or   {finite true}}]
  (->NumberCoercer (make-number-format number-format) finite error-message))


(deftype DateCoercer [formatter parser error-message tz]
  Coercer
  (to-str [_ obj] (if (nil? obj) "" (.format formatter obj tz)))
  (from-str [_ s] (if (str/blank? s) nil (parse-date parser s)))

  validator/Validator
  (validate [this str-value]
    (let [obj-value (from-str this str-value)]
      (if-not (boolean (or (str/blank? str-value)
                           (not (nil? (from-str this str-value)))))
        [error-message]
        []))))

(defn- make-datetime-format [format]
  (let [format (if (keyword? format)
                 (get {
                       :full-date       DateTimeFormat.Format.FULL_DATE
                       :long-date       DateTimeFormat.Format.LONG_DATE
                       :medium-date     DateTimeFormat.Format.MEDIUM_DATE
                       :short-date      DateTimeFormat.Format.SHORT_DATE
                       :full-time       DateTimeFormat.Format.FULL_TIME
                       :long-time       DateTimeFormat.Format.LONG_TIME
                       :medium-time     DateTimeFormat.Format.MEDIUM_TIME
                       :short-time      DateTimeFormat.Format.SHORT_TIME
                       :full-datetime   DateTimeFormat.Format.FULL_DATETIME
                       :long-datetime   DateTimeFormat.Format.LONG_DATETIME
                       :medium-datetime DateTimeFormat.Format.MEDIUM_DATETIME
                       :short-datetime  DateTimeFormat.Format.SHORT_DATETIME}
                      format))]
    (DateTimeFormat. format)))

(defn- make-datetime-parse [format]
  (let [format (if (keyword? format)
                 (get {
                       :full-date       DateTimeFormat.Format.FULL_DATE
                       :long-date       DateTimeFormat.Format.LONG_DATE
                       :medium-date     DateTimeFormat.Format.MEDIUM_DATE
                       :short-date      DateTimeFormat.Format.SHORT_DATE
                       :full-time       DateTimeFormat.Format.FULL_TIME
                       :long-time       DateTimeFormat.Format.LONG_TIME
                       :medium-time     DateTimeFormat.Format.MEDIUM_TIME
                       :short-time      DateTimeFormat.Format.SHORT_TIME
                       :full-datetime   DateTimeFormat.Format.FULL_DATETIME
                       :long-datetime   DateTimeFormat.Format.LONG_DATETIME
                       :medium-datetime DateTimeFormat.Format.MEDIUM_DATETIME
                       :short-datetime  DateTimeFormat.Format.SHORT_DATETIME}
                      format))]
    (DateTimeParse. format)))

(defn date
  ([format error-message]
    (date format error-message nil))
  ([format error-message tz]
   (->DateCoercer
     (make-datetime-format format)
     (make-datetime-parse format)
     error-message
     tz)))

(deftype KeywordCoercer [error-message]
  Coercer
  (to-str [_ obj] (if (nil? obj) "" (name obj)))
  (from-str [_ s] (if (str/blank? s) nil (cljs.core/keyword s)))

  validator/Validator
  (validate [this str-value]
    (let [obj-value (from-str this str-value)]
      (if-not (str/blank? str-value)
        [error-message]
        []))))

(defn keyword [error-message]
  (->KeywordCoercer error-message))