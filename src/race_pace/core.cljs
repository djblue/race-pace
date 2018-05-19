(ns race-pace.core
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [race-pace.time :as t]
            [race-pace.distance :as d]))

(defn parse-number [s t]
  (let [types {:float js/parseFloat :int js/parseInt}
        parse (get types t js/parseInt)]
    (parse s)))

(defn round [n precision]
  (let [factor (js/Math.pow 10 precision)]
    (/ (js/Math.round (* n factor)) factor)))

(defn div [style & args]
  (into [:div {:style style}] args))

(defn flex [& args]
  (into
   [div {:display :flex
         :flex 1
         :align-items :center
         :justify-content
         (if (= (count args) 1) :space-around :space-between)}]
   args))

(defn number-input []
  (let [in (r/atom nil) tmp (r/atom nil)]
    (fn [props]
      (let [{:keys [value on-change]} props
            t (get props :type :int)]
        (when (and (some? @in)
                   (not (= @tmp value)))
          (reset! in nil))
        [:input {:value (or @in (round value 3))
                 :style {:display :block
                         :width "100%"
                         :background :none
                         :text-align :center
                         :border :none
                         :outline :none
                         :font-size "1.4em"
                         :color "#eceff4"}
                 :on-change
                 (fn [e]
                   (let [txt (.-value (.-target e))
                         n (count (re-seq #"\." txt))
                         v (parse-number txt t)]
                     (when (and (re-matches #"[0-9.]*" txt)
                                (case t :int (= n 0) :float (< n 2)))
                       (if (or (str/ends-with? txt ".") (js/isNaN v))
                         (do (reset! in txt)
                             (reset! tmp value))
                         (when-not
                          (or (and (contains? props :min) (< v (:min props)))
                              (and (contains? props :max) (> v (:max props)))
                              (not (= (round v 3) v)))
                           (reset! in nil)
                           (reset! tmp nil)
                           (on-change v))))))}]))))

(defn time-unit-input [props]
  (let [{:keys [value unit on-change]} props
        t (unit (t/parse (or value 0)))
        u (unit t/units)
        r (unit {:hours {:min 0}
                 :minutes {:min 0 :max 59}
                 :seconds {:min 0 :max 59}})
        label (name (unit {:hours :h :minutes :m :seconds :s}))]
    [flex
     [number-input
      (merge r
             {:value t
              :on-change #(on-change (+ (- value (* t u)) (* % u)))})]
     (name label)]))

(defn time-input [props]
  (let [{:keys [value on-change]} props]
    (->> [:hours :minutes :seconds]
         (map #(-> [time-unit-input
                    {:unit %
                     :value value
                     :on-change on-change}]))
         (interpose [flex ":"])
         (into [div {:display :flex :max-width 260}]))))

(defn unit-label
  ([a] (unit-label a nil))
  ([a b]
   [:div
    {:style {:font-weight 300 :text-align :center}}
    (str (name a) (if-not (nil? b) (str " / " (name b))))]))

(defn section [label & args]
  (->> args
       (map #(-> [flex %]))
       (into
        [:div
         [div {:padding "10px 0"
               :font-weight :bold
               :font-family :Lato}
          [div {:font-size "1.8em"}
           [flex
            label
            [div {:flex 1
                  :margin-left 20
                  :border-radius 100
                  :border-bottom "2px solid #eceff4"}]]]]])))

(defn option [props]
  (let [{:keys [label value option-value on-select]} props
        selected? (= value option-value)]
    [:button
     {:on-click #(on-select option-value)
      :style
      {:background (if selected? "#8fbcbb" "#eceff4")
       :border 0
       :border-radius 5
       :color "#2e3440"
       :flex 1
       :font-size "1.2em"
       :height 48
       :margin 5
       :min-width 140}}
     label]))

(defn options [& opts]
  (into
   [div {:display :flex
         :flex-wrap :wrap
         :align-items :center
         :justify-content :space-around}]
   opts))

(defn slash []
  [div {:height "100px"
        :transform "rotate(25deg)"
        :margin-left 10
        :margin-right 10
        :border-radius 100
        :border-right "2px solid #eceff4"}])

(defn distance-options [props]
  (let [{:keys [value on-select]} props]
    [:div
     {:style {:margin-bottom 20}}
     (->> [[:marathon (d/miles 26.2)]
           [:half-marathon (d/miles 13.1)]
           [:10k (d/kilometers 10)]
           [:5k (d/kilometers 5)]]
          (map (fn [[label option-value]]
                 [option
                  {:label label
                   :option-value option-value
                   :value value
                   :on-select on-select}]))
          (into [options]))]))

(defn distance-unit-input [props]
  (let [{:keys [value unit on-change]} props
        scale (unit {:miles d/miles
                     :meters d/meters
                     :kilometers d/kilometers})]
    [:div
     [number-input {:value (/ value (scale 1))
                    :min 0
                    :type :float
                    :on-change #(on-change (scale %))}]
     [unit-label unit]]))

(defn distance-input [props]
  (let [{:keys [value on-change]} props]
    (->> [:miles :kilometers :meters]
         (map #(-> [distance-unit-input
                    {:value value :unit % :on-change on-change}]))
         (interpose [slash])
         (into [flex])
         (conj [:div [distance-options
                      {:value value :on-select on-change}]]))))

(defn pace-input [props]
  (let [{:keys [time distance on-change]} props
        kms (d/->kilometers distance)
        miles (d/->miles distance)
        hrs (t/->hours time)]
    (->> [[:time :km    (/ time kms)    #(* % kms)]
          [:time :mile  (/ time miles)  #(* % miles)]
          [:miles :hour (/ miles hrs)   #(/ miles (t/->hours %)) :float]]
         (map (fn [[a b value fn t]]
                [div {:padding "20px 0"}
                 [(case t :float number-input time-input)
                  {:value value
                   :type t
                   :on-change #(-> % fn on-change)}]
                 [unit-label a b]]))
         (into [options]))))

(defn page [& body]
  (->> body
       (into
        [div {:font-family :Roboto
              :font-weight 300
              :max-width 600
              :margin "0 auto"}])
       (conj
        [div {:padding 10
              :box-sizing :border-box
              :background "#2e3440"
              :color "#eceff4"
              :min-height "100vh"}])))

(defonce state (r/atom {:time  (t/minutes 30)
                        :distance (d/kilometers 5)}))

(defn app []
  (let [{:keys [time distance]} @state
        on-update #(swap! state assoc %1 %2)]
    [page
     [section "time"
      [time-input {:value time
                   :on-change #(on-update :time %)}]]
     [section "distance"
      [distance-input {:value distance
                       :on-change #(on-update :distance %)}]]
     [section "pace"
      [pace-input {:time time
                   :distance distance
                   :on-change #(on-update :time %)}]]]))

(defn ^:export render []
  (r/render
   [app]
   (. js/document (getElementById "app"))))

(render)
