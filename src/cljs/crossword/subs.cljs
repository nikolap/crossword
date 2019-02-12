(ns crossword.subs
  (:require [re-frame.core :as re-frame]
            [reframe-utils.core :as rf-utils]
            [crossword.utils :as utils]))

(re-frame/reg-sub
  ::name
  (fn [db]
    (:name db)))

(rf-utils/multi-generation
  rf-utils/reg-basic-sub
  :core/puzzle
  :core/answers
  :core/active-cell
  :core/orientation
  :core/checks)

(defn ultimate-puzzle-data
  [grid grid-nums circles checks]
  (mapv
    (fn [[row actual-row] num-row circle-row]
      (mapv
        (fn [[col actual] num circle]
          {:row     row
           :col     col
           :black?  (= "." actual)
           :answer  actual
           :num     (if (= 0 num) nil num)
           :letters (count actual)
           :circle? (= 1 circle)
           :correct? (get-in checks [row col])})
        (map-indexed list actual-row) num-row circle-row))
    (map-indexed list grid)
    grid-nums
    circles))

(defn loop-row-clues
  [grid clues cols]
  (loop [grid            grid
         clues           clues
         x               0
         y               0
         initial-placed? false
         last-black?     false]
    (let [[current-clue & rest-clues] clues]
      (if-let [{:keys [black?]} (get-in grid [x y])]
        (cond
          (and (or (not initial-placed?)
                   last-black?)
               black?) (recur grid clues x (inc y) initial-placed? true)
          black? (recur grid rest-clues x (inc y) initial-placed? true)
          :else (recur (assoc-in grid [x y :across-clue] current-clue) clues x (inc y) true false))
        (if (= y cols)
          (recur grid (if last-black? clues rest-clues) (inc x) 0 false false)
          grid)))))

(defn number-question-map [clues]
  (reduce (fn [o clue]
            (assoc o (second (re-find utils/clue-num-regex clue)) clue))
          {} clues))

(defn insert-col-clues [grid x y clue]
  (if-let [{:keys [black? down-clue]} (get-in grid [x y])]
    (if (or black? down-clue)
      grid
      (insert-col-clues
        (assoc-in grid [x y :down-clue] clue)
        (inc x)
        y
        clue))
    grid))

(defn loop-col-clues
  [grid clues cols]
  (let [clue-map (number-question-map clues)]
    (loop [x    0
           y    0
           grid grid]
      (if-let [{:keys [num]} (get-in grid [x y])]
        (recur x (inc y) (insert-col-clues grid x y (get clue-map (str num))))
        (if (= y cols)
          (recur (inc x) 0 grid)
          grid)))))

(defn insert-clues [grid {:keys [across down]} cols]
  (-> grid
      (loop-row-clues across cols)
      (loop-col-clues down cols)))

(re-frame/reg-sub
  :core/get-answer
  :<- [:core/answers]
  (fn [answers [_ x y]]
    (get-in answers [x y] "")))

(re-frame/reg-sub
  :core/puzzle-for-display
  :<- [:core/puzzle]
  :<- [:core/checks]
  (fn [[{:keys [size grid gridnums circles clues]} checks] _]
    (let [{:keys [cols rows]} size
          circles (or circles (map (constantly 0) gridnums))]
      (insert-clues (ultimate-puzzle-data (partition cols grid) (partition cols gridnums) (partition cols circles) checks)
                    clues
                    cols))))

(re-frame/reg-sub
  :core/active-clue
  :<- [:core/puzzle-for-display]
  :<- [:core/active-cell]
  :<- [:core/orientation]
  (fn [[puzzle [x y] orientation] _]
    (get-in puzzle [x y (if (= :row orientation) :across-clue :down-clue)])))

(re-frame/reg-sub
  :core/opposite-clue
  :<- [:core/puzzle-for-display]
  :<- [:core/active-cell]
  :<- [:core/orientation]
  (fn [[puzzle [x y] orientation] _]
    (get-in puzzle [x y (if (= :col orientation) :across-clue :down-clue)])))