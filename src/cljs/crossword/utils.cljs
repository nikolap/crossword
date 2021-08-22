(ns crossword.utils)

(def filter-first (comp first filter))

(defn target-value [e]
  (.-value (.-target e)))

(def clue-num-regex #"^(\d+?)\.")
(def clue-text-regex #"^\d+?\.\s(.+?)$")
(def alpha-regex #"[a-zA-Z0-9]")