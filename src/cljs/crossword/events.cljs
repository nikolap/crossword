(ns crossword.events
  (:require [re-frame.core :as re-frame]
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

(re-frame/reg-event-db
  ::initialize-db
  (fn [_ _]
    db/default-db))

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
    {:db    (assoc db :core/orientation :row
                      :core/set-active-cell [0 0])
     ::http {:uri        (str "https://www.xwordinfo.com/JSON/Data.aspx?date=" (or date "current"))
             :on-success [:core/add-puzzle]}}))

(re-frame/reg-event-db
  :core/set-answer
  (fn [db [_ row col ans]]
    (assoc-in db
              [:core/answers row col]
              (if (re-find utils/alpha-regex ans)
                (-> ans
                    last
                    str
                    string/upper-case)
                ""))))

(re-frame/reg-event-fx
  :core/set-active-cell
  [(re-frame/inject-cofx ::inject/sub [:core/puzzle-for-display])]
  (fn [{:keys [db core/puzzle-for-display]} [_ [x y]]]
    {:db             (assoc db :core/active-cell [x y])
     :core/focus-on  {:x x :y y}
     :core/scroll-to (get-in puzzle-for-display [x y])}))

(defn flip-orientation [orientation]
  (if (= :row orientation) :col :row))

;; TODO: wrap around
(defn move [{:keys [rows cols] :as size} puzzle row col orientation change]
  (let [new-x (if (= orientation :row) row (+ row change))
        new-y (if (= orientation :col) col (+ col change))]
    (if-let [{:keys [black?]} (get-in puzzle [new-x new-y])]
      (if black?
        (move size puzzle row col orientation (+ change change))
        [new-x new-y])
      [row col])))

(defn delete-code? [key-code]
  (contains? #{46 8} key-code))

(defn alpha-code? [key-code]
  (and (>= key-code 65)
       (<= key-code 90)))

(defn answer-code? [key-code]
  (or (alpha-code? key-code)
      (delete-code? key-code)))

(re-frame/reg-event-fx
  :core/handle-key-down
  [(re-frame/inject-cofx ::inject/sub [:core/puzzle-for-display])]
  (fn [{:keys [db core/puzzle-for-display]} [_ row col key-code]]
    (let [orientation (:core/orientation db)
          size (get-in db [:core/puzzle :size])]
      {:db (case key-code
             32 (update db :core/orientation flip-orientation)
             37 (assoc db :core/orientation :row)
             39 (assoc db :core/orientation :row)
             38 (assoc db :core/orientation :col)
             40 (assoc db :core/orientation :col)
             db)
       :dispatch-n [(when (answer-code? key-code)
                      [:core/set-answer row col (if (delete-code? key-code)
                                                  ""
                                                  (char key-code))])
                    [:core/set-active-cell (cond
                                             (= 37 key-code) (move size puzzle-for-display row col :row -1)
                                             (= 38 key-code) (move size puzzle-for-display row col :col -1)
                                             (= 39 key-code) (move size puzzle-for-display row col :row 1)
                                             (= 40 key-code) (move size puzzle-for-display row col :col 1)
                                             (alpha-code? key-code) (move size puzzle-for-display row col orientation 1)
                                             (= 8 key-code) (move size puzzle-for-display row col orientation -1)
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
                                       (= (get-in puzzle-for-display [r c :answer]) cell))
                                     row)))
                            answers)))})))