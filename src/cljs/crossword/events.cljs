(ns crossword.events
  (:require [cljs.tools.reader :as reader]
            [re-frame.core :as re-frame]
            [reframe-utils.core :as rf-utils]
            [crossword.db :as db]
            [clojure.string :as string]
            [crossword.utils :as utils]
            [vimsical.re-frame.cofx.inject :as inject])
  (:import goog.Uri
           goog.net.Jsonp))

(defn do-jsonp
  [uri callback]
  (let [req (Jsonp. (Uri. uri))]
    (.send req nil callback)))

(re-frame/reg-fx
  ::http
  (fn [{:keys [uri on-success]}]
    (do-jsonp uri #(re-frame/dispatch (conj on-success %)))))

(re-frame/reg-fx
  :core/focus-on
  (fn [{:keys [x y]}]
    (when-let [el (.querySelector js/document (str "input[data-x='" x "'][data-y='" y "']"))]
      (.focus el))))

(defn clue-child [list clue]
  (utils/filter-first #(= (.getAttribute % "data-clue") clue) (.-children list)))

(defn scroll-to [list clue]
  (when-let [focal-clue (clue-child list clue)]
    (set! (.-scrollTop list)
          (- (.-offsetTop focal-clue)
             (.-offsetTop list)))))

(re-frame/reg-fx
  :core/scroll-to
  (fn [{:keys [across-clue down-clue]}]
    (let [across-list (.getElementById js/document "across-clue-list")
          down-list   (.getElementById js/document "down-clue-list")]
      (scroll-to across-list across-clue)
      (scroll-to down-list down-clue))))

(re-frame/reg-event-fx
  ::initialize-db
  [(re-frame/inject-cofx :storage/get {:name :cows-crossword/db})]
  (fn [{stored-db :storage/get} _]
    {:db (or (reader/read-string stored-db) db/default-db)}))

(rf-utils/multi-generation
  rf-utils/reg-set-event
  :core/orientation)

(re-frame/reg-event-db
  :core/add-puzzle
  (fn [db [_ puzzle]]
    (let [{:keys [size] :as puzzle} (js->clj puzzle :keywordize-keys true)
          blanks (mapv #(mapv (fn [_] "") (range (:cols size))) (range (:rows size)))]
      (assoc db :core/puzzle puzzle
                :core/answers blanks
                :core/checks blanks))))

(re-frame/reg-event-fx
  :core/get-puzzle
  (fn [{:keys [db]} [_ date]]
    {:db    (assoc db :core/orientation :row)
     ::http {:uri        (str "https://www.xwordinfo.com/JSON/Data.aspx?date=" (or date "current"))
             :on-success [:core/add-puzzle]}}))

(re-frame/reg-event-fx
  :core/set-answer
  (fn [{:keys [db]} [_ row col ans]]
    (let [new-state (assoc-in db
                              [:core/answers row col]
                              (if (re-find utils/alpha-regex ans)
                                (-> ans
                                    str
                                    string/upper-case)
                                ""))]
      {:db          new-state
       :dispatch    [:core/reset-check row col]
       :storage/set {:session? false
                     :name     :cows-crossword/db
                     :value    new-state}})))

(re-frame/reg-event-fx
  :core/set-active-cell
  [(re-frame/inject-cofx ::inject/sub [:core/puzzle-for-display])]
  (fn [{:keys [db core/puzzle-for-display]} [_ [x y]]]
    {:db             (assoc db :core/active-cell [x y])
     :core/focus-on  {:x x :y y}
     :core/scroll-to (get-in puzzle-for-display [x y])}))

(defn flip-orientation [orientation]
  (if (= :row orientation) :col :row))

(defn move [{:keys [rows cols] :as size} puzzle row col orientation change multiplier rollover?]
  (let [total-change (* multiplier change)
        new-x        (if (= orientation :row) row (+ row total-change))
        new-y        (if (= orientation :col) col (+ col total-change))]
    (cond
      (< new-x 0) (move size puzzle (dec rows) (if rollover? col (dec col)) orientation change 0 true)
      (>= new-x rows) (move size puzzle 0 (if rollover? 0 (inc col)) orientation change 0 true)
      (< new-y 0) (move size puzzle (if rollover? row (dec row)) (dec cols) orientation change 0 true)
      (>= new-y cols) (move size puzzle (if rollover? 0 (inc row)) 0 orientation change 0 true)
      :else (if-let [{:keys [black?]} (get-in puzzle [new-x new-y])]
              (if black?
                (move size puzzle row col orientation change (inc multiplier) rollover?)
                [new-x new-y])
              [row col]))))

(defn delete-code? [key-code]
  (contains? #{46 8} key-code))

(defn alpha-code? [key-code]
  (and (>= key-code 65)
       (<= key-code 90)))

(defn numeric-code? [key-code]
  (or
    ;; non-keypad
    (and (>= key-code 48)
         (<= key-code 57))
    ;; keypad
    (and (>= key-code 96)
         (<= key-code 105))))

(defn answer-code? [key-code]
  (or (alpha-code? key-code)
      (numeric-code? key-code)
      (delete-code? key-code)))

(re-frame/reg-event-db
  :core/reset-check
  (fn [db [_ row col]]
    (assoc-in db [:core/checks row col] nil)))

(re-frame/reg-event-fx
  :core/handle-key-down
  [(re-frame/inject-cofx ::inject/sub [:core/puzzle-for-display])]
  (fn [{:keys [db core/puzzle-for-display]} [_ row col key-code ctrl-key? current letters]]
    (let [orientation   (:core/orientation db)
          size          (get-in db [:core/puzzle :size])
          current-count (count current)]
      {:db         (case key-code
                     32 (update db :core/orientation flip-orientation)
                     37 (assoc db :core/orientation :row)
                     39 (assoc db :core/orientation :row)
                     38 (assoc db :core/orientation :col)
                     40 (assoc db :core/orientation :col)
                     db)
       :dispatch-n [(cond (and ctrl-key? (= key-code 74))
                          [:core/check-answers]

                          (and ctrl-key? (= key-code 75))
                          [:core/set-answer row col
                           (get-in puzzle-for-display [row col :answer])]

                          (answer-code? key-code)
                          [:core/set-answer row col (cond
                                                      (and (delete-code? key-code)
                                                           (> current-count 1))
                                                      (subs current 0 (dec current-count))

                                                      (delete-code? key-code)
                                                      ""

                                                      (and (> letters 1)
                                                           (< current-count letters))
                                                      (str current (char key-code))

                                                      (and (> letters 1)
                                                           (= current-count letters))
                                                      (str (subs current 0 (dec current-count))
                                                           (char key-code))

                                                      :else
                                                      (char key-code))])
                    [:core/set-active-cell (cond
                                             (= 37 key-code) (move size puzzle-for-display row col :row -1 1 false)
                                             (= 38 key-code) (move size puzzle-for-display row col :col -1 1 false)
                                             (= 39 key-code) (move size puzzle-for-display row col :row 1 1 false)
                                             (= 40 key-code) (move size puzzle-for-display row col :col 1 1 false)

                                             (and (or (alpha-code? key-code)
                                                      (numeric-code? key-code))
                                                  (not (and ctrl-key?
                                                            (= key-code 74)))
                                                  (or (= (inc current-count)
                                                         letters)
                                                      (and (= 1 current-count)
                                                           (= 1 letters))))
                                             (move size puzzle-for-display row col orientation 1 1 false)

                                             (and (= 8 key-code)
                                                  (<= current-count 1))
                                             (move size puzzle-for-display row col orientation -1 1 false)

                                             :else [row col])]]})))

(re-frame/reg-event-fx
  :core/check-answers
  [(re-frame/inject-cofx ::inject/sub [:core/puzzle-for-display])]
  (fn [{:keys [db core/puzzle-for-display]} _]
    (let [answers (:core/answers db)]
      {:db (assoc db :core/checks
                     (vec (map-indexed
                            (fn [r row]
                              (vec (map-indexed
                                     (fn [c cell]
                                       (let [ans (get-in puzzle-for-display [r c :answer])]
                                         (or (= ans cell)
                                             (= "." ans))))
                                     row)))
                            answers)))})))