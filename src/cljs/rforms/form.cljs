(ns rforms.form
  (:require
    [rforms.validator :as validator]
    [rforms.coercer :as coercer]))

(defprotocol FormProcessor
  (process-form [this form]))

(extend-protocol FormProcessor
  nil
  (process-form [this form] form))

(defn- field-path [path type]
  (if (#{:value :original :validator-error} type)
    (cons type path)
    [type path]))

(defrecord Form
  [value original form-touched field-touched
   str-value coercion-error validator-error validator])

; FORM
(defn form-value [form]
  (:value form))

(defn form-original-value [form]
  (:original form))

(defn reset-form-value [form]
  (assoc form :value (form-original-value form)))

(defn form-touched? [form]
  (:form-touched form))

(defn touch-form [form]
  (assoc form :form-touched true))

(defn untouch-form [form]
  (assoc form :form-touched false))

(defn form-valid? [form]
  (and (empty? (:coercion-error form))
       (empty? (:validator-error form))))

(defn forced-validation? [form]
  (:forced-validation form))

(defn force-validation [form]
  (assoc form :forced-validation true))

(defn clear-validation [form]
  (assoc form :forced-validation false))

; FIELD value
(defn field-value [form path]
  (get-in form (field-path path :value)))

(defn assoc-field-value [form path value]
  (assoc-in form (field-path path :value) value))

(defn update-field-value [form path f & args]
  (apply update-in form (field-path path :value) f args))

; FIELD original value
(defn field-original-value [form path]
  (get-in form (field-path path :original)))

(defn reset-field-value [form path]
  (assoc-in form
            (field-path path :value)
            (field-original-value form path)))

; FIELD touched
(defn field-touched? [form path]
  (or (get-in form (field-path path :field-touched) false)
      (forced-validation? form)))

(defn untouch-fields [form]
  (assoc form :field-touched {}))

(defn touch-field [form path]
  (assoc-in form (field-path path :field-touched) true))

(defn untouch-field [form path]
  (assoc-in form (field-path path :field-touched) false))

; FIELD errors
(defn field-errors [form path]
  (concat
    (get-in form (field-path path :coercion-error))
    (get-in form (field-path path :validator-error))))

(defn field-valid? [form path]
  (-> (field-errors form path)
      (empty?)))

; FIELD str value

(defn field-str-value [form path coercer]
  (or (get-in form (field-path path :str-value))
      (coercer/to-str coercer (field-value form path))))

(defn assoc-field-str-value [form path str-value]
  (assoc-in form (field-path path :str-value) str-value))

(defn update-field-str-value [form path f & args]
  (apply update-in form (field-path path :str-value) f args))

(defn clear-str-values [form]
  (assoc form :str-value {}))
(defn clear-str-value [form field]
  (update form :str-value dissoc (:path field)))

(defn clear-coerced-str-value [form path]
  (if (field-value form path)
    (dissoc form (field-path path :str-value))
    form))

; Validation
(defn validate-form [form]
  (let [{:keys [validator processor]} form
        errors (validator/validate validator (form-value form))]
    (-> form
        (assoc :validator-error errors)
        (as-> form (process-form processor form)))))

; Coercion
(defn coerce-field [form path coercer]
  (let [{:keys [coercer]} path
        str-value (field-str-value form path coercer)
        errors    (validator/validate coercer str-value)]
    (if (empty? errors)
      (-> form
          (update :coercion-error dissoc path)
          (assoc-field-value path (coercer/from-str coercer str-value)))
      (-> form
          (assoc-in (field-path path :coercion-error) errors)
          (assoc-field-value path nil)))))

; Factory

(defn make-form [value {:keys [validator processor]
                        :as   options}]
  (map->Form (-> (merge options
                        {:value             value
                         :original          value
                         :form-touched      false
                         :forced-validation false
                         :field-touched     {}
                         :str-value         {}
                         :coercion-error    {}
                         :validator-error   {}
                         :validator         validator
                         :processor         processor})
                 (validate-form))))