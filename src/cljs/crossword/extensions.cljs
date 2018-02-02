(ns crossword.extensions
  (:require-macros [cljs.core :refer [exists?]]))

(def types [(when (exists? js/NodeList) js/NodeList)
            (when (exists? js/HTMLCollection) js/HTMLCollection)
            (when (exists? js/HTMLFormControlsCollection) js/HTMLFormControlsCollection)
            (when (exists? js/HTMLOptionsCollection) js/HTMLOptionsCollection)
            (when (exists? js/HTMLDocument) js/HTMLDocument)
            (when (exists? js/HTMLDivElement) js/HTMLDivElement)
            (when (exists? js/HTMLParagraphElement) js/HTMLParagraphElement)
            (when (exists? js/HTMLSpanElement) js/HTMLSpanElement)])

(defn extend-type-fn
  "Given a type t, apply extensions."
  [t]
  (when t
    (extend-type t
      ISeqable
      (-seq [array] (array-seq array 0))
      ICounted
      (-count [a] (alength a))
      IIndexed
      (-nth
        ([array n]
         (if (< n (alength array)) (aget array n)))
        ([array n not-found]
         (if (< n (alength array)) (aget array n)
                                   not-found)))
      ILookup
      (-lookup
        ([array k]
         (aget array k))
        ([array k not-found]
         (-nth array k not-found)))
      IReduce
      (-reduce
        ([array f]
         (ci-reduce array f))
        ([array f start]
         (ci-reduce array f start))))))

(doseq [t types] (extend-type-fn t))