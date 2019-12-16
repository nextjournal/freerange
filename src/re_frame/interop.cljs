(ns re-frame.interop
  (:require [reagent.core]
            [reagent.impl.batching]
            [reagent.ratom]
            [react :as react]
            [react-dom :as react-dom]))

(def next-tick reagent.impl.batching/do-before-flush)

(def empty-queue #queue [])

(def after-render reagent.core/after-render)

;; Make sure the Google Closure compiler sees this as a boolean constant,
;; otherwise Dead Code Elimination won't happen in `:advanced` builds.
;; Type hints have been liberally sprinkled.
;; https://developers.google.com/closure/compiler/docs/js-for-compiler
(def ^boolean debug-enabled? "@define {boolean}" ^boolean goog/DEBUG)

(defn ratom [x]
  (reagent.core/atom x))

(defn ratom? [x]
  (satisfies? reagent.ratom/IReactiveAtom x))

(defn deref? [x]
  (satisfies? IDeref x))


(defn make-reaction [f]
  (reagent.ratom/make-reaction f))

(defn add-on-dispose! [a-ratom f]
  (reagent.ratom/add-on-dispose! a-ratom f))

(defn dispose! [a-ratom]
	(reagent.ratom/dispose! a-ratom))

(defn set-timeout! [f ms]
  (js/setTimeout f ms))

(defn now []
  (if (and
       (exists? js/performance)
       (exists? js/performance.now))
    (js/performance.now)
    (js/Date.now)))

(defn reagent-id
  "Produces an id for reactive Reagent values
  e.g. reactions, ratoms, cursors."
  [reactive-val]
  (when (implements? reagent.ratom/IReactiveAtom reactive-val)
    (str (condp instance? reactive-val
           reagent.ratom/RAtom "ra"
           reagent.ratom/RCursor "rc"
           reagent.ratom/Reaction "rx"
           reagent.ratom/Track "tr"
           "other")
         (hash reactive-val))))

;; Make reagent benefit from batched updates

(def ^:dynamic *in-batch?* false)

(defn batch-updates [f]
  (react-dom/unstable_batchedUpdates
   (fn []
     (binding [*in-batch?* true]
       (f)))))

(let [flush-queues (.bind (.-flush-queues reagent.impl.batching/render-queue)
                          reagent.impl.batching/render-queue)]
  (set! (.-flush-queues reagent.impl.batching/render-queue)
        #(batch-updates flush-queues)))

(let [queue-render reagent.impl.batching/queue-render]
  (set! reagent.impl.batching/queue-render (fn [^clj c]
                                             (if *in-batch?*
                                               (.forceUpdate c)
                                               (queue-render c)))))
