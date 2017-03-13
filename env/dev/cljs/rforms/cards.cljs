(ns rforms.cards
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.session :as session]
            [rforms.core :as form]
            [rforms.form :as f]
            [devcards.core :as dc]
            [rforms.validator :as validator])
  (:require-macros
    [reagent.ratom :refer [reaction]]
    [devcards.core
     :as dc
     :refer [defcard defcard-doc defcard-rg deftest]]))

(set! goog.LOCALE "sk")

(defcard-rg first-card
  [:div>h1 "This is your first devcard!"])

#_(defcard-rg home-page-card
    [core/home-page])

(defn input [field label attrs]
  (let [valid?   (form/field-valid? field)
        touched? (form/field-touched? field)
        errors   (form/field-errors field)]
    (fn [field label attrs]
      [:div
       [:label
        (str label ": ")
        [form/input field attrs]
        (if @valid? "Valid" "Invalid")
        " "
        (if @touched? "Touched" "")
        ]
       (when (seq @errors)
         [:ul
          (for [[i error] (map-indexed vector @errors)]
            ^{:key i} [:li error])])])))

(defcard-rg form
  (fn [state]
    (let [form       (form/make-form @state
                                     :pending false
                                     :validator (reify validator/Validator
                                                  (validate [_ value]
                                                    (if-not (empty? (:value value))
                                                      {}
                                                      {:value ["Field is required"]})))
                                     :processor (fn [form]
                                                  (reify f/FormProcessor
                                                    (process-form [this -form]
                                                      (if (and (f/field-valid? -form [:value]))
                                                        (do
                                                          (js/setTimeout #(swap! form assoc :pending false) 1000)
                                                          (assoc -form :pending true))
                                                        -form)))))
          can-submit (reaction (let [form @form]
                                 (and (f/form-valid? form)
                                      (not (:pending form)))))]
      (fn [state]
        [:form
         {:on-submit (fn [e]
                       (form/force-validation! form)
                       (let [form @form]
                         (when (and (f/form-valid? form)
                                    (not (:pending form)))
                           (js/console.log "Submit: " (form/form-value @form))))
                       (.preventDefault e))}
         [input (form/make-field form [:value] nil) "Value" {:type "text"}]
         [:input {:type     "submit"
                  :disabled (not @can-submit)
                  :value    (str "Submit - " @can-submit)}]
         ])))
  {:value nil})

(reagent/render [:div] (.getElementById js/document "app"))

;; remember to run 'lein figwheel devcards' and then browse to
;; http://localhost:3449/cards
