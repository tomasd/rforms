(ns rforms.validator)

(defprotocol Validator
  (validate [this form]))

(extend-protocol Validator
  nil
  (validate [this form] {}))

(defprotocol ValidationResult
  (field-errors [this path]))

(extend-protocol ValidationResult
  cljs.core/PersistentHashMap
  (field-errors [this path]
    (get-in this path [])))

(defn valid? [validator value]
  (empty? (validate validator value)))