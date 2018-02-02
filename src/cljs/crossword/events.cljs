(ns crossword.events
  (:require [re-frame.core :as re-frame]
            [reframe-utils.core :as rf-utils]
            [crossword.db :as db]
            [clojure.string :as string]
            [crossword.utils :as utils]
            [vimsical.re-frame.cofx.inject :as inject])
  (:import goog.Uri
           goog.net.Jsonp))

(def key-codes {32 :space
                37 :left
                38 :up
                39 :right
                40 :down})

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

(defn move-horizontal [{:keys [core/puzzle]} row col change]
  (let [rows    (get-in puzzle [:size :rows])
        new-val (+ row change)]
    [(if (and (>= new-val 0)
              (< new-val rows))
       new-val
       row) col]))

(defn move-vertical [{:keys [core/puzzle]} row col change]
  (let [rows    (get-in puzzle [:size :rows])
        new-val (+ col change)]
    [row (if (and (>= new-val 0)
                  (< new-val rows))
           new-val
           col)]))

(defn handle-key-press [db row col pressed-key]
  (case pressed-key
    :left (move-vertical db row col -1)
    :right (move-vertical db row col 1)
    :up (move-horizontal db row col -1)
    :down (move-horizontal db row col 1)
    [row col]))

;; move forward
;; move back
;; no move

(re-frame/reg-event-fx
  :core/handle-key-down
  (fn [{:keys [db]} [_ row col key-code]]
    (if-let [pressed-key (get key-codes key-code)]
      (let [horizontal? (contains? #{:left :right} pressed-key)
            vertical?   (contains? #{:up :down} pressed-key)]
        {:db       (cond
                     (= :space pressed-key) (update db :core/orientation flip-orientation)
                     horizontal? (assoc db :core/orientation :row)
                     vertical? (assoc db :core/orientation :col)
                     :else db)
         :dispatch [:core/set-active-cell (handle-key-press db row col pressed-key)]})
      (merge
        {:db         db
         :dispatch-n [[:core/set-answer row col (char key-code)]
                      (when (re-find utils/alpha-regex (char key-code))
                        [:core/set-active-cell (if (= (:core/orientation db) :row)
                                                 (move-vertical db row col 1)
                                                 (move-horizontal db row col 1))])]}))))

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