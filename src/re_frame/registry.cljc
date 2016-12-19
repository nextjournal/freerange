(ns re-frame.registry
  "In many places, re-frame asks you to associate an `id` (keyword)
  with a `handler` (function).  This namespace contains the
  central registry of such associations."
  (:require  [re-frame.interop :refer [debug-enabled?]]
             [re-frame.loggers :refer [console]]))


;; kinds of handlers
(def kinds #{:event :fx :cofx :sub})

(defprotocol IRegistry
  (get-handler
    [this kind]
    [this kind id]
    [this kind id required?])

  (register-handler
    [this kind id handler-fn])

  (clear-handlers
    [this]
    [this kind]
    [this kind id]))

(defrecord Registry [kinds kind->id->handler]
  IRegistry
  (get-handler [this kind]
    (get @kind->id->handler kind))
  (get-handler [this kind id]
    (get-in @kind->id->handler [kind id]))
  (get-handler [this kind id required?]
    (let [handler (get-handler this kind id)]
      (when debug-enabled?                                   ;; This is in a separate when so Closure DCE can run
        (when (and required? (nil? handler))                 ;; Otherwise you'd need to type hint the and with a ^boolean for DCE.
          (console :error "re-frame: no " (str kind) " handler registered for:" id)))
      handler))

  (register-handler [this kind id handler-fn]
    (when debug-enabled?                                       ;; This is in a separate when so Closure DCE can run
      (when (get-handler this kind id false)
        (console :warn "re-frame: overwriting" (str kind) "handler for:" id)))   ;; allow it, but warn. Happens on figwheel reloads.
    (swap! kind->id->handler assoc-in [kind id] handler-fn)
    handler-fn)    ;; note: returns the just registered handler

  (clear-handlers [this] ;; clear all kinds
    (reset! kind->id->handler {}))
  (clear-handlers [this kind] ;; clear all handlers for this kind
    (assert (kinds kind))
    (swap! kind->id->handler dissoc kind))
  (clear-handlers [this kind id] ;; clear a single handler for a kind
    (assert (kinds kind))
    (if (get-handler this kind id)
      (swap! kind->id->handler update-in [kind] dissoc id)
      (console :warn "re-frame: can't clear" (str kind) "handler for" (str id ". Handler not found.")))))

(defn make-registry []
  ;; This atom contains a register of all handlers.
  ;; Contains a map keyed first by `kind` (of handler), and then `id`.
  ;; Leaf nodes are handlers.
  (->Registry kinds (atom {})))