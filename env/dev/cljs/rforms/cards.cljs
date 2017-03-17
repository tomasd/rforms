(ns rforms.cards
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.session :as session]
            [rforms.core :as form]
            [rforms.form :as f]
            [devcards.core :as dc]
            [rforms.validator :as validator]
            [rforms.coercer :as coercer])
  (:require-macros
    [reagent.ratom :refer [reaction]]
    [cljs.test :refer [testing is async]]
    [devcards.core
     :as dc
     :refer [defcard defcard-doc defcard-rg deftest]])
  (:import
    [goog.i18n TimeZone]))

(defcard-rg first-card
  [:div>h1 "This is your first devcard!"])

#_(defcard-rg home-page-card
    [core/home-page])

(deftest coercer
  (testing "Number"
    (testing "Decimal format"
      (let [c (coercer/number :decimal "Invalid integer")]
        (is (= 1 (coercer/from-str c "1")))
        (is (= "1" (coercer/to-str c 1)))
        (is (= 1.1 (coercer/from-str c "1,1")))
        (is (= "1,1" (coercer/to-str c 1.1)))))
    (testing "Currency format"
      (let [c (coercer/number :currency "Invalid integer")]
        (is (= 1 (coercer/from-str c "1 €")))
        (is (= "1,00 €" (coercer/to-str c 1)))
        (is (= 1.1 (coercer/from-str c "1,1 €")))
        (is (= "1,10 €" (coercer/to-str c 1.1)))))
    (testing "Percent format"
      (let [c (coercer/number :percent "Invalid percent")]
        (is (= 0.5 (coercer/from-str c "50 %")))
        (is (= "50 %" (coercer/to-str c 0.5)))

        (is (= "26 %" (coercer/to-str c 0.256)))
        (is (= 0.26 (coercer/from-str c "26 %")))))
    (testing "Decimal percent format #.##%"
      (let [c (coercer/number "#.##%" "Invalid percent")]
        (is (= 0.5 (coercer/from-str c "50%")))
        (is (= "50%" (coercer/to-str c 0.5)))

        (is (= "25,6%" (coercer/to-str c 0.256)))
        (is (= 0.256 (coercer/from-str c "25,6%"))))))
(testing "Date"
  (testing "medium date format"
    (let [c (coercer/date :medium-date "Invalid date")]
      (is (= "1. 2. 2010" (coercer/to-str c #inst "2010-02-01T14:34:55.054")))
      (is (= #inst "2010-02-01" (coercer/from-str c "1. 2. 2010")))))
  (testing "medium date time format"
    (let [c (coercer/date :medium-datetime "Invalid date")]
      (is (= "1. 2. 2010, 15:34:55" (coercer/to-str c #inst "2010-02-01T14:34:55.054")))
      (is (= #inst "2010-02-01T14:34:55.000" (coercer/from-str c "1. 2. 2010, 15:34:55")))))
  (testing "medium date time format UTC tz"
    (let [c (coercer/date :medium-datetime "Invalid date" (TimeZone.createTimeZone 0))]
      (is (= "1. 2. 2010, 14:34:55" (coercer/to-str c #inst "2010-02-01T14:34:55.054")))
      (is (= #inst "2010-02-01T14:34:55.000" (coercer/from-str c "1. 2. 2010, 15:34:55")))))
  )
  (testing "Keyword"
    (let [c (coercer/keyword "Invalid keyword")]
      (is (= :keyword (coercer/from-str c "keyword")))
      (is (= "keyword" (coercer/to-str c  :keyword)))))
  #_(testing "Date coercer"
      (is (= "" (coercer/to-str (coercer/date "dd.MM.yyyy"))))))

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
