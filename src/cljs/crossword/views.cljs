(ns crossword.views
  (:require [clojure.string :as string]
            [reagent.core :as reagent]
            [re-frame.core :as re-frame]
            [crossword.subs :as subs]
            [crossword.utils :as utils]))

(defn settings-panel []
  (let [date (reagent/atom "")]
    (fn []
      [:div.row>div.column
       [:h1 "Word."]
       [:label
        {:for "date"}
        "Enter YYYY-MM-DD or 'current' or 'random' without quotes. e.g. 2018-01-15 or current or random"]
       [:input
        {:value       @date
         :id          "date"
         :placeholder "YYYY-MM-DD"
         :on-change   #(reset! date (utils/target-value %))}]
       [:div
        [:button
         {:on-click #(re-frame/dispatch [:core/get-puzzle @date])}
         "Load"]
        [:button
         {:on-click #(re-frame/dispatch [:core/check-answers])}
         "Check Answers (Ctrl+J)"]
        [:p "Hit Ctrl+K to reveal a cell's letter/answer."]]])))

(defn clue-panel []
  (let [active-clue (re-frame/subscribe [:core/active-clue])]
    (fn []
      [:div.active-clue-panel (or @active-clue "<no clue selected>")])))

(defn board-cell [r-idx c-idx _]
  (let [current     (re-frame/subscribe [:core/get-answer r-idx c-idx])
        active-cell (re-frame/subscribe [:core/active-cell])
        active-clue (re-frame/subscribe [:core/active-clue])]
    (fn [r-idx c-idx {:keys [black? num circle? across-clue down-clue correct?]}]
      [:td.cell
       {:class (str (when circle? "circle ")
                    (if black? "black " "cell ")
                    (when (and (some? correct?) (not correct?)) "incorrect-answer ")
                    (cond
                      (= @active-cell [r-idx c-idx]) "active-cell"
                      (contains? #{across-clue down-clue} @active-clue) "active-clue"))
        :title (str "Across: " across-clue
                    ", Down: " down-clue)}
       (when black? [:span])
       (when-not black?
         [:input.cell-input
          {:value       @current
           :tab-index   -1
           :on-click    #(re-frame/dispatch [:core/set-active-cell [r-idx c-idx]])
           :on-key-down (fn [e]
                          (let [ctrl-key? (.-ctrlKey e)
                                key-code  (.-keyCode e)]
                            (when (or (and ctrl-key? (= key-code 74))
                                      (and ctrl-key? (= key-code 75)))
                              (.preventDefault e))
                            (re-frame/dispatch [:core/handle-key-down r-idx c-idx key-code ctrl-key?])))
           :data-x      r-idx
           :data-y      c-idx}])
       (when-not black?
         [:span.number num])])))

(defn board-row [r-idx row]
  [:tr.row
   (for [[c-idx cell] (map-indexed list row)]
     ^{:key (str "board-cell-" c-idx)}
     [board-cell r-idx c-idx cell])])

(defn board-panel []
  (let [board-data (re-frame/subscribe [:core/puzzle-for-display])]
    (fn []
      [:table>tbody
       (for [[r-idx row] (map-indexed list @board-data)]
         ^{:key (str "board-row-" r-idx)}
         [board-row r-idx row])])))

(defn board-and-clue-panel []
  [:div.column
   [clue-panel]
   [board-panel]])

(defn clue-entry []
  (let [active-clue   (re-frame/subscribe [:core/active-clue])
        opposite-clue (re-frame/subscribe [:core/opposite-clue])]
    (fn [full-clue grid-num clue-text]
      [:li.clue
       {:class     (str "clue "
                        (when (= @active-clue full-clue) "clue-selected")
                        (when (= @opposite-clue full-clue) "clue-highlighted"))
        :data-clue full-clue}
       [:span.clue-num grid-num]
       [:span.clue-text clue-text]])))

(defn clue-list [label items id]
  [:div.column.clue-container
   {:style {:padding-bottom "25px"}}
   [:h4 label]
   [:ol.clue-list
    {:id id}
    (for [[idx entry] (map-indexed list items)
          :let [grid-num  (first (re-find utils/clue-num-regex entry))
                clue-text (second (re-find utils/clue-text-regex entry))]]
      ^{:key (str "clue-" label "-" idx)}
      [clue-entry entry grid-num clue-text])]])

(defn clues-panel [{:keys [clues]}]
  (let [{:keys [across down]} clues]
    [:div.column
     [:div.row
      [clue-list "Across" across "across-clue-list"]
      [clue-list "Down" down "down-clue-list"]]]))

(defn game-panel [{:keys [title dow publisher date copyright author editor notepad]
                   :as   puzzle}]
  [:div
   [:div.row>div.column
    [:h2 (if (string/blank? title)
           (str dow " " publisher " - " date)
           title)]
    (when notepad [:div
                   [:em {:dangerouslySetInnerHTML {:__html notepad}}]
                   [:hr]])]
   [:div.row
    [board-and-clue-panel]
    [clues-panel puzzle]]
   [:div.row
    [:div.column>small "Copyright " copyright]
    [:div.column>small "Author " [:strong author] ", Editor " [:strong editor]]]])

(defn main-panel []
  (let [puzzle (re-frame/subscribe [:core/puzzle])]
    [:div.container
     [settings-panel]
     (when (not-empty @puzzle)
       [game-panel @puzzle])]))