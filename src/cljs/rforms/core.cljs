(ns rforms.core
  (:require-macros
    [reagent.ratom :refer [reaction]])
  (:require
    [rforms.coercer]
    [rforms.form :as form]
    [reagent.ratom :refer [atom cursor]]
    [rforms.coercer :as coercer]
    [rforms.validator :as validator]))

(defn make-form [value & {:keys [validator processor]
                          :as   options}]
  (if processor
    (doto (atom nil)
      (as-> a (reset! a (form/make-form value (assoc options :processor (processor a))))))
    (atom (form/make-form value options))))

(defrecord Field [form path coercer])
(defn make-field [form path coercer]
  (->Field form path coercer))


(defn field-value [field]
  (reaction
    (form/field-value @(:form field) (:path field))))

(defn field-str-value [field]
  (reaction
    (form/field-str-value @(:form field) (:path field) (:coercer field))))

(defn field-valid? [field]
  (reaction
    (form/field-valid? @(:form field) (:path field))))

(defn field-touched? [field]
  (reaction
    (form/field-touched? @(:form field) (:path field))))

(defn field-errors [field]
  (reaction
    (form/field-errors @(:form field) (:path field))))

(defn form-valid? [form]
  (reaction
    (form/form-valid? @form)))

(defn form-value [form]
  (reaction
    (form/form-value @form)))

(defn form-touched? [form]
  (reaction
    (form/form-touched? @form)))

(defn reset-form! [form]
  (swap! form
         (fn [form]
           (-> form
               (form/reset-form-value)
               (form/clear-str-values)
               (form/untouch-fields)
               (form/untouch-form)
               (form/validate-form)))))

(defn set-field-value! [field value]
  (swap! (:form field)
         (fn [form]
           (-> form
               (form/assoc-field-value (:path field) value)
               (form/touch-field (:path field))
               (form/validate-form)
               (form/touch-form)))))

(defn reset-field!
  [field]
  (swap! (:form field)
         (fn [form]
           (-> form
               (form/reset-field-value (:path field))
               (form/clear-str-value (:path field))
               (form/untouch-field (:path field))
               (form/validate-form)))))

(defn- coerce-field [form field]
  (form/coerce-field form (:path field) (:coercer field)))

(defn set-field-str-value!
  [field str-value & {:keys [coerce touch validate retain]
                      :or   {coerce   true
                             touch    true
                             validate true
                             retain   true}}]
  (swap! (:form field)
         (fn [form]
           (cond-> form
             true (form/assoc-field-str-value (:path field) str-value)
             coerce (coerce-field field)
             touch (form/touch-field (:path field))
             (and coerce (not retain)) (form/clear-coerced-str-value (:path field))
             (and coerce validate) (form/validate-form)
             true (form/touch-form)))))

(defn coerce-field! [field & {:keys [retain]
                              :or   {retain true}}]
  (swap! (:form field)
         (fn [form]
           (cond-> form
             true (coerce-field field)
             true (form/touch-field (:path field))
             (not retain) (form/clear-coerced-str-value (:path field))
             true (form/validate-form)
             true (form/touch-form)))))

(defn force-validation! [form]
  (swap! form
         (fn [form]
           (-> form
               (form/force-validation)
               (form/validate-form)))))

(defn handle-str-value
  ([field & {:keys [coerce touch validate retain]
             :or   {coerce   true
                    touch    true
                    validate true
                    retain   true}
             :as   options}]
   #(apply set-field-str-value! field (-> % .-target .-value) options)))

(defn handle-checked-value [field]
  #(set-field-value! field (-> % .-target .-checked)))

(defn handle-valid-form [form callback]
  (fn [e]
    (force-validation! form)
    (when @(form-valid? form)
      (callback (form/form-value @form)))
    (.preventDefault e)))

(defn input
  ([field attrs]
   (let [value     (field-value field)
         str-value (field-str-value field)]
     (fn [field attrs]
       (case (:type attrs)
         "checkbox"
         [:input (merge attrs
                        {:type      "checkbox"
                         :checked   @value
                         :on-change (handle-checked-value field)})]

         "radio"
         (do
           (assert (:value attrs) "value attribute is required for radio")
           [:input (-> attrs
                       (dissoc :value)
                       (merge
                         {:type      "radio"
                          :value     (coercer/to-str (:coercer field) (:value attrs))
                          :on-change (handle-str-value field)
                          :checked   (= @value (:value attrs))}))])

         "select"
         (let [options (cond-> (:options attrs)
                         (satisfies? IDeref (:options attrs)) deref)]
           (assert (:options attrs) "options attribute is required for select")
           (into [:select
                  (-> attrs
                      (dissoc :options)
                      (merge
                        {:value     @str-value
                         :on-change (handle-str-value field)}))]
                 options))

         (let [{:keys [touch-on-change retain-str-value validate-on-change]
                :or   {touch-on-change    true
                       validate-on-change true
                       retain-str-value   true
                       }} attrs]
           [:input (merge {:type "text"}
                          (dissoc attrs
                                  :touch-on-change
                                  :retain-str-value
                                  :validate-on-change)
                          {:value     @str-value
                           :on-change (handle-str-value field
                                                        :touch touch-on-change
                                                        :validate validate-on-change
                                                        :retain retain-str-value)
                           :on-blur   #(coerce-field! field :retain retain-str-value)})]))))))

(defn kv-options [& options]
  (->> (partition 2 options)
       (mapv (fn [[value label]]
               ^{:key value}
               [:option {:value value} label]))))

